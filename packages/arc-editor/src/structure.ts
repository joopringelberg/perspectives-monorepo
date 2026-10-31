// Lexical structure of ARC sources, derived from indentation. No Monaco dependency so it can be unit tested in Node.

const roleDeclaration = /^\s*(aspect\s+)?(user|thing|context|external)\b/;
const contextDeclaration = /^\s*(domain|case|party|activity)\b/;

const indentOf = (line: string) => line.length - line.trimStart().length;
const isBlankOrComment = (line: string) => /^\s*($|--)/.test(line);

/**
 * The 0-based index of the line declaring the context that lexically encloses the role declaration containing
 * line `lineIndex` (or declared on it). For an aspect role this is the context the aspect is added to.
 * Undefined if the line is not inside a role declaration.
 */
export function enclosingContextOfRole(lines: string[], lineIndex: number): number | undefined {
  let inRole = false;
  let indent = Infinity;
  for (let i = lineIndex; i >= 0; i--) {
    const line = lines[i];
    if (isBlankOrComment(line)) continue;
    const ind = indentOf(line);
    if (i !== lineIndex && ind >= indent) continue;
    indent = ind;
    if (inRole && contextDeclaration.test(line)) return i;
    if (!inRole && roleDeclaration.test(line)) inRole = true;
    if (ind === 0) break;
  }
  return undefined;
}
