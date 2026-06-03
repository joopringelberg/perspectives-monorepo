// SPDX-FileCopyrightText: 2019 Joop Ringelberg (joopringelberg@gmail.com), Cor Baars
// SPDX-License-Identifier: GPL-3.0-or-later
//
// Node.js-compatible replacements for the browser-only functions in authenticate.js.
// FileReader and File are not available in Node.js; we use Buffer instead.

export function bytesToBase64DataUrlImpl(bytes) {
  let binary = '';
  for (let i = 0; i < bytes.byteLength; i++) {
    binary += String.fromCharCode(bytes[i]);
  }
  return Promise.resolve('data:application/octet-stream;base64,' + btoa(binary));
}

export function dataUrlToBytesImpl(dataUrl) {
  const base64 = dataUrl.split(',')[1];
  const binary = atob(base64);
  const bytes = new Uint8Array(binary.length);
  for (let i = 0; i < binary.length; i++) {
    bytes[i] = binary.charCodeAt(i);
  }
  return Promise.resolve(bytes);
}