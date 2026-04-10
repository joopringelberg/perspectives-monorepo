// BEGIN LICENSE
// Perspectives Distributed Runtime
// SPDX-FileCopyrightText: 2024 Joop Ringelberg (joopringelberg@gmail.com)
// SPDX-License-Identifier: GPL-3.0-or-later
// END LICENSE

// ---------------------------------------------------------------------------
// Shared naming utilities
// ---------------------------------------------------------------------------

/**
 * Resolve a stable type ID to a human-readable two-segment SQL name using the
 * provided nameMap.
 *
 * Readable names from the PDR have the form:
 *   `"model://...#Segment1$Segment2$...$SegmentN"`
 *
 * This function strips the URI prefix (everything up to and including '#'),
 * splits by '$', and returns the last two segments joined by '$'.  This
 * produces context-qualified names like `"Bijeenkomst$Aanwezigen"` that are
 * collision-resistant and meaningful for the end user.
 *
 * Examples:
 *   - "model://a.b#Bijeenkomst$Aanwezigen"           → "Bijeenkomst$Aanwezigen"
 *   - "model://a.b#System$SocialEnvironment$Persons"  → "SocialEnvironment$Persons"
 *   - "model://a.b#System$User"                       → "System$User"
 *
 * Falls back to the stable ID itself when the type is not in the nameMap.
 */
export function stableToTwoSegmentName(
  stableId: string,
  nameMap: Record<string, string>,
): string {
  const readable = nameMap[stableId] ?? stableId;
  const afterHash = readable.includes('#') ? readable.split('#')[1]! : readable;
  const parts = afterHash.split('$');
  return parts.length >= 2
    ? parts.slice(-2).join('$')
    : parts[parts.length - 1] || readable;
}
