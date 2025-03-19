import { default as enPreact } from "./lang/en/preact.json";
import { default as nlPreact } from "./lang/nl/preact.json";

export async function getPreact(LANG_KEY : string) {
  function getPreactSync(LANG_KEY : string) {
    switch (LANG_KEY) {
      case "en":
        return enPreact;
      case "nl":
        return nlPreact;
      default:
        return enPreact;
    }
  }
  return Promise.resolve(getPreactSync(LANG_KEY));
}

