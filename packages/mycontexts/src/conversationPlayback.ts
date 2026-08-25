import type { ConversationBody, ConversationElement, ConversationMessage } from 'perspectives-proxy';

export interface Utterance {
  speaker: 'bot' | 'human';
  message: ConversationMessage;
}

export interface RuntimeAnswer {
  message: ConversationMessage;
  sequence?: ConversationElement[];
}

export interface ConversationRun {
  history: Utterance[];
  answers: RuntimeAnswer[];
  status: 'playing' | 'complete' | 'invalid';
}

interface AdvanceResult {
  history: Utterance[];
  answers: RuntimeAnswer[];
  status: ConversationRun['status'];
}

// Structural sequences are transparent during playback. A question pauses the
// walk and exposes only the contiguous answers that immediately follow it.
function advance(sequence: ConversationElement[], history: Utterance[]): AdvanceResult {
  const pending = [...sequence];

  while (pending.length > 0) {
    const element = pending.shift()!;

    if ('statement' in element) {
      history = [...history, { speaker: 'bot', message: element.statement }];
      continue;
    }

    if ('question' in element) {
      const answers: RuntimeAnswer[] = [];
      while (pending.length > 0 && 'answer' in pending[0]) {
        const answer = pending.shift() as Extract<ConversationElement, { answer: ConversationMessage }>;
        answers.push({ message: answer.answer, sequence: answer.sequence });
      }

      return {
        history: [...history, { speaker: 'bot', message: element.question }],
        answers,
        status: answers.length > 0 ? 'playing' : 'invalid',
      };
    }

    if ('answer' in element) {
      return { history, answers: [], status: 'invalid' };
    }

    if ('sequence' in element) {
      pending.unshift(...element.sequence);
      continue;
    }

    // The build pipeline should reject this shape; retaining an explicit
    // invalid state keeps corrupt attachments from looking like completion.
    return { history, answers: [], status: 'invalid' };
  }

  return { history, answers: [], status: 'complete' };
}

export function startConversation(body: ConversationBody): ConversationRun {
  return advance(body.conversation, []);
}

export function selectAnswer(run: ConversationRun, answerIndex: number): ConversationRun {
  if (run.status !== 'playing' || !run.answers[answerIndex]) {
    return run;
  }

  const answer = run.answers[answerIndex];
  const history = [...run.history, { speaker: 'human' as const, message: answer.message }];
  return answer.sequence ? advance(answer.sequence, history) : { history, answers: [], status: 'complete' };
}