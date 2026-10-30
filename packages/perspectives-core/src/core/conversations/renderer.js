import { dump } from "js-yaml";

export const dumpConversationBodyImpl = (body) => dump(body, {
  lineWidth: -1,
  noRefs: true,
  noCompatMode: true,
});