export type FeedbackEntry = {
  text: string;
  position?: { lineNumber: number; column: number };
};

/** ArcFeedback uses the compiler's Show format, not LSP's zero-based positions. */
export function feedbackEntries(text: string): FeedbackEntry[] {
  return text.split(/(?=^\([A-Za-z]\w*\))/m).filter(Boolean).map(text => {
    const match = /line (\d+), column (\d+)\b/.exec(text);
    if (!match) return { text };
    const lineNumber = Number(match[1]);
    const column = Number(match[2]);
    return {
      text,
      position: Number.isSafeInteger(lineNumber) && Number.isSafeInteger(column) && lineNumber > 0 && column > 0
        ? { lineNumber, column } : undefined,
    };
  });
}
