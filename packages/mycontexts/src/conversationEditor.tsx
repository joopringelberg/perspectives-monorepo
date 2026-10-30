import React from 'react';
import type { ConversationBranch } from 'perspectives-proxy';
import { i18next } from 'perspectives-react';

interface ConversationEditorProps {
  branch: ConversationBranch;
  saving: boolean;
  error?: string;
  onCancel: () => void;
  onSave: (conversationText: string) => void;
}

interface ConversationEditorState {
  conversationText: string;
}

class ConversationEditor extends React.Component<ConversationEditorProps, ConversationEditorState> {
  state: ConversationEditorState = { conversationText: '' };

  componentDidMount(): void {
    this.setState({ conversationText: this.props.branch.conversationText });
  }

  render() {
    return (
      <div className="conversation-editor-backdrop">
        <div className="conversation-editor" role="dialog" aria-modal="true" aria-labelledby="conversation-editor-title">
          <div className="conversation-editor-toolbar">
            <h2 id="conversation-editor-title" className="conversation-editor-title">
              {i18next.t('help_edit_title', { ns: 'mycontexts' })}
            </h2>
            <div className="conversation-editor-actions">
              <button type="button" className="btn btn-sm btn-outline-secondary" onClick={this.props.onCancel} disabled={this.props.saving}>
                {i18next.t('help_edit_cancel', { ns: 'mycontexts' })}
              </button>
              <button type="button" className="btn btn-sm btn-primary" onClick={() => this.props.onSave(this.state.conversationText)} disabled={this.props.saving}>
                {i18next.t('help_edit_save', { ns: 'mycontexts' })}
              </button>
            </div>
          </div>
          <textarea
            className="conversation-editor-text"
            value={this.state.conversationText}
            onChange={event => this.setState({ conversationText: event.target.value })}
            disabled={this.props.saving}
          />
          {this.props.error ? <p className="conversation-editor-error text-danger">{this.props.error}</p> : null}
        </div>
      </div>
    );
  }
}

export default ConversationEditor;
