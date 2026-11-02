// Navigation history (back/forward) over jumps to definitions, references and members.
// This module has no Monaco dependency so it can be unit tested in Node.

export class NavigationHistory<T> {
  private backStack: T[] = [];
  private forwardStack: T[] = [];
  private limit: number;

  constructor(limit = 100) {
    this.limit = limit;
  }

  /** Record the location we leave on a jump. A new jump discards the forward history. */
  record(from: T): void {
    this.backStack.push(from);
    if (this.backStack.length > this.limit) this.backStack.shift();
    this.forwardStack = [];
  }

  /** The location to return to; `current` becomes available for `forward`. */
  back(current: T): T | undefined {
    const prev = this.backStack.pop();
    if (prev !== undefined) this.forwardStack.push(current);
    return prev;
  }

  forward(current: T): T | undefined {
    const next = this.forwardStack.pop();
    if (next !== undefined) this.backStack.push(current);
    return next;
  }

  canGoBack(): boolean { return this.backStack.length > 0; }
  canGoForward(): boolean { return this.forwardStack.length > 0; }

  /** Drop entries that are no longer valid (e.g. disposed models). */
  prune(keep: (entry: T) => boolean): void {
    this.backStack = this.backStack.filter(keep);
    this.forwardStack = this.forwardStack.filter(keep);
  }
}
