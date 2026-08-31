import React, { useEffect, useRef, useState } from 'react';
import type { ConversationBody, ConversationMessage } from 'perspectives-proxy';
import { i18next } from 'perspectives-react';
import { rewindToQuestion, selectAnswer, startConversation, type ConversationRun } from './conversationPlayback';
import type { HelpTarget } from './helpTypes';

export type HelpViewerContent =
  | { status: 'loading' }
  | { status: 'unavailable' }
  | { status: 'error' }
  | { status: 'open'; conversation: ConversationBody };

interface Point {
  left: number;
  top: number;
}

interface ConversationViewerProps {
  target: HelpTarget;
  content: HelpViewerContent;
  initialPosition: Point;
  requestId: number;
  onClose: () => void;
}

const VIEWER_MARGIN = 12;
const KEYBOARD_STEP = 16;

function translated(message: ConversationMessage): string {
  return i18next.t(message.message, { defaultValue: message.fallback });
}

function clampPosition(position: Point, element?: HTMLElement | null): Point {
  const width = element?.offsetWidth ?? Math.min(420, window.innerWidth - VIEWER_MARGIN * 2);
  const height = element?.offsetHeight ?? Math.min(560, window.innerHeight - VIEWER_MARGIN * 2);
  return {
    left: Math.min(Math.max(position.left, VIEWER_MARGIN), Math.max(VIEWER_MARGIN, window.innerWidth - width - VIEWER_MARGIN)),
    top: Math.min(Math.max(position.top, VIEWER_MARGIN), Math.max(VIEWER_MARGIN, window.innerHeight - height - VIEWER_MARGIN)),
  };
}

export function positionNearTarget(anchorRect: DOMRect): Point {
  const estimatedWidth = Math.min(420, window.innerWidth - VIEWER_MARGIN * 2);
  const estimatedHeight = Math.min(560, window.innerHeight - VIEWER_MARGIN * 2);
  const right = anchorRect.right + VIEWER_MARGIN;
  const left = anchorRect.left - estimatedWidth - VIEWER_MARGIN;
  const below = anchorRect.bottom + VIEWER_MARGIN;
  const above = anchorRect.top - estimatedHeight - VIEWER_MARGIN;

  return clampPosition({
    left: right + estimatedWidth <= window.innerWidth ? right : left,
    top: below + estimatedHeight <= window.innerHeight ? below : above,
  });
}

const ConversationViewer: React.FC<ConversationViewerProps> = ({ target, content, initialPosition, requestId, onClose }) => {
  const viewerRef = useRef<HTMLDivElement>(null);
  const headingRef = useRef<HTMLHeadingElement>(null);
  const historyRef = useRef<HTMLDivElement>(null);
  const dragOffset = useRef<Point | null>(null);
  const [position, setPosition] = useState(initialPosition);
  const [run, setRun] = useState<ConversationRun | null>(content.status === 'open' ? startConversation(content.conversation) : null);

  useEffect(() => {
    setPosition(initialPosition);
  }, [initialPosition, requestId]);

  useEffect(() => {
    setRun(content.status === 'open' ? startConversation(content.conversation) : null);
  }, [content]);

  useEffect(() => {
    headingRef.current?.focus();
  }, [requestId]);

  useEffect(() => {
    historyRef.current?.lastElementChild?.scrollIntoView({ block: 'nearest' });
  }, [run?.history.length]);

  useEffect(() => {
    const handleResize = () => setPosition(current => clampPosition(current, viewerRef.current));
    window.addEventListener('resize', handleResize);
    return () => window.removeEventListener('resize', handleResize);
  }, []);

  const startDragging = (event: React.PointerEvent<HTMLDivElement>) => {
    if ((event.target as HTMLElement).closest('button')) return;
    dragOffset.current = { left: event.clientX - position.left, top: event.clientY - position.top };
    event.currentTarget.setPointerCapture(event.pointerId);
  };

  const drag = (event: React.PointerEvent<HTMLDivElement>) => {
    if (!dragOffset.current) return;
    setPosition(clampPosition({
      left: event.clientX - dragOffset.current.left,
      top: event.clientY - dragOffset.current.top,
    }, viewerRef.current));
  };

  const stopDragging = (event: React.PointerEvent<HTMLDivElement>) => {
    dragOffset.current = null;
    if (event.currentTarget.hasPointerCapture(event.pointerId)) event.currentTarget.releasePointerCapture(event.pointerId);
  };

  const moveWithKeyboard = (event: React.KeyboardEvent<HTMLDivElement>) => {
    if (!event.altKey || !['ArrowLeft', 'ArrowRight', 'ArrowUp', 'ArrowDown'].includes(event.key)) return;
    const delta = {
      left: event.key === 'ArrowLeft' ? -KEYBOARD_STEP : event.key === 'ArrowRight' ? KEYBOARD_STEP : 0,
      top: event.key === 'ArrowUp' ? -KEYBOARD_STEP : event.key === 'ArrowDown' ? KEYBOARD_STEP : 0,
    };
    setPosition(current => clampPosition({ left: current.left + delta.left, top: current.top + delta.top }, viewerRef.current));
    event.preventDefault();
  };

  return (
    <div
      ref={viewerRef}
      className="conversation-viewer"
      style={position}
      role="dialog"
      aria-modal="false"
      aria-labelledby="conversation-viewer-title"
      aria-describedby="conversation-viewer-description"
    >
      <div
        className="conversation-viewer-toolbar"
        onPointerDown={startDragging}
        onPointerMove={drag}
        onPointerUp={stopDragging}
        onPointerCancel={stopDragging}
        onKeyDown={moveWithKeyboard}
      >
        <h2 id="conversation-viewer-title" ref={headingRef} tabIndex={-1} className="conversation-viewer-title">
          {i18next.t('help_title', { ns: 'mycontexts' })}: {target.label}
        </h2>
        <span id="conversation-viewer-description" className="visually-hidden">
          {i18next.t('help_move_instructions', { ns: 'mycontexts' })}
        </span>
        <button type="button" className="btn btn-sm conversation-viewer-close" onClick={onClose} aria-label={i18next.t('help_close', { ns: 'mycontexts' })}>
          <i className="bi bi-x-lg" aria-hidden="true" />
        </button>
      </div>

      <div ref={historyRef} className="conversation-viewer-history" aria-live="polite" aria-atomic="false">
        {content.status === 'loading' && <p className="conversation-viewer-status">{i18next.t('help_loading', { ns: 'mycontexts' })}</p>}
        {content.status === 'unavailable' && <p className="conversation-viewer-status">{i18next.t('help_unavailable', { ns: 'mycontexts' })}</p>}
        {content.status === 'error' && <p className="conversation-viewer-status text-danger">{i18next.t('help_error', { ns: 'mycontexts' })}</p>}
        {run?.history.map((utterance, index) => {
          const content = <>
            <span className="visually-hidden">{i18next.t(`help_speaker_${utterance.speaker}`, { ns: 'mycontexts' })}: </span>
            {translated(utterance.message)}
          </>;
          const className = `conversation-utterance conversation-utterance-${utterance.speaker}`;

          return utterance.answers ? (
            <button key={`${utterance.message.message}-${index}`} type="button" className={`${className} conversation-utterance-question`} onClick={() => setRun(current => current ? rewindToQuestion(current, index) : current)}>
              {content}
            </button>
          ) : (
            <div key={`${utterance.message.message}-${index}`} className={className}>
              {content}
            </div>
          );
        })}
        {run?.status === 'invalid' && <p className="conversation-viewer-status text-danger">{i18next.t('help_invalid', { ns: 'mycontexts' })}</p>}
        {run?.status === 'complete' && <p className="conversation-viewer-complete">{i18next.t('help_complete', { ns: 'mycontexts' })}</p>}
      </div>

      {run?.status === 'playing' && (
        <div className="conversation-viewer-answers" aria-label={i18next.t('help_answers', { ns: 'mycontexts' })}>
          {run.answers.map((answer, index) => (
            <button key={`${answer.message.message}-${index}`} type="button" className="btn btn-outline-primary text-start" onClick={() => setRun(current => current ? selectAnswer(current, index) : current)}>
              {translated(answer.message)}
            </button>
          ))}
        </div>
      )}
    </div>
  );
};

export default ConversationViewer;