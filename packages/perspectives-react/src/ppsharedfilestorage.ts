// BEGIN LICENSE
// Perspectives Distributed Runtime
// Copyright (C) 2019 Joop Ringelberg (joopringelberg@perspect.it), Cor Baars
//
// This program is free software: you can redistribute it and/or modify
// it under the terms of the GNU General Public License as published by
// the Free Software Foundation, either version 3 of the License, or
// (at your option) any later version.
//
// This program is distributed in the hope that it will be useful,
// but WITHOUT ANY WARRANTY; without even the implied warranty of
// MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
// GNU General Public License for more details.
//
// You should have received a copy of the GNU General Public License
// along with this program.  If not, see <https://www.gnu.org/licenses/>.
//
// Full text of this license can be found in the LICENSE file in the projects root.
// END LICENSE


// This module supports part of the interface of the Mega Storage object.
// We use it as a drop-in replacement for new users who can upload a limited number of 
// files to the courtesy shared file storage offered by Perspectives.

// @ts-ignore
const ppStorageUrl = __PPSTORAGEURL__;
import i18next from "i18next";

interface UploadResponse {
  megaUrl: string;
  error?: number;
}

interface ErrorResponse {
  error: number;
  message: string;
}

type UploadResult = Promise<string | ErrorResponse>;


export default class PPStorage 
{
    private sharedFileServerKey: string;

    constructor(key: string) {
      this.sharedFileServerKey = key;
    }

    upload(theFile: File): UploadResult {
      // Create a FormData object and append the file
      const formData = new FormData();
      formData.append('file', theFile);
      formData.append('sharedfileserverkey', this.sharedFileServerKey);

      return new Promise((resolve, reject) => {
        try {
          // Post the file to the perspectives-sharedfilestorage relay service
          fetch(ppStorageUrl, {
            method: 'POST',
            body: formData
          }).then(response => {
            if (response.status == 201) {
              // The mega url.
              response.json().then((r: UploadResponse) => resolve(r.megaUrl));
            } else {
              response.json().then((result: ErrorResponse) => {
                switch (result.error) {
                  case NOFILE:
                    reject({ error: result.error, message: i18next.t("ppsharedfilestorage_nofile", { ns: "preact" }) });
                    break;

                  case NOKEY:
                    reject({ error: result.error, message: i18next.t("ppsharedfilestorage_nokey", { ns: "preact" }) });
                    break;

                  case KEYUNKNOWN:
                    reject({ error: result.error, message: i18next.t("ppsharedfilestorage_keyunknown", { ns: "preact" }) });
                    break;

                  case MAXFILESREACHED:
                    reject({ error: result.error, message: i18next.t("ppsharedfilestorage_maxfilesreached", { ns: "preact" }) });
                    break;
                }
              });
            }
          });
        } catch (error) {
          console.error('Error:', error);
          reject({ message: i18next.t("ppsharedfilestorage_serviceerror", { ns: "preact" }) });
        }
      });
    }
  }


// Error types
const NOKEY = 1;
const NOFILE = 2;
const KEYUNKNOWN = 3;
const MAXFILESREACHED = 4;
const MEGAERROR = 5;
const MAXKEYSREACHED = 6;
