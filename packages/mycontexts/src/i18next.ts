// See: https://phrase.com/blog/posts/step-step-guide-javascript-localization/

// We need http-backend to load the translations from the server.

import HttpApi from "i18next-http-backend";
import {i18next, getPreact} from "perspectives-react";
import LanguageDetector from 'i18next-browser-languagedetector';
import {get, set} from 'idb-keyval';

export async function initI18next () : Promise<void>
{
  let currentLanguage = await get("currentLanguage");

  return i18next
    .use(HttpApi)
    .use(LanguageDetector)
    .init({
      supportedLngs: ["en", "nl"],
      // Enabled useful console output when developing
      debug: true,
      // Disable loading of dev locale
      fallbackLng: false,
      ns: ["mycontexts", "preact"],
      lng: currentLanguage
      })
    .then(() => {

      if (!currentLanguage)
        {
          set("currentLanguage", i18next.language);
          currentLanguage = i18next.language;
        }
      return loadLanguageResources(currentLanguage)
      .then( () => i18next.loadNamespaces(["mycontexts", "preact"]))
      });
}

export function loadLanguageResources (currentLanguage : string) : Promise<any>
{
  return Promise.all([
    import(`./lang/${currentLanguage}/mycontexts.json`).then( t => i18next.addResourceBundle(currentLanguage, "mycontexts", t.default || t, true)),
    getPreact(currentLanguage).then( t => i18next.addResourceBundle(currentLanguage, "preact", t, true))
  ])
}

// Add this function to your i18next.ts file
export async function changeLanguage(language: string): Promise<void> {
  // Store the selected language in IndexedDB
  await set("currentLanguage", language);
  
  // Load the language resources if needed
  await loadLanguageResources(language);
  
  // Change the i18next language
  await i18next.changeLanguage(language);
  
  // Force a re-render of components that use translation
  // document.dispatchEvent(new Event('languageChanged'));
}