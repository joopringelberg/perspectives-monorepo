// SPDX-FileCopyrightText: 2019 Joop Ringelberg (joopringelberg@gmail.com), Cor Baars
// SPDX-License-Identifier: GPL-3.0-or-later
//
// Node.js-compatible replacements for the browser-only functions in authenticate.js.
// FileReader and File are not available in Node.js; we use Buffer instead.

export function bytesToBase64DataUrlImpl(bytes) {
  return Promise.resolve(
    'data:application/octet-stream;base64,' + Buffer.from(bytes).toString('base64')
  );
}

export function dataUrlToBytesImpl(dataUrl) {
  // Data URLs have the form: data:<mime>;base64,<data>
  const base64 = dataUrl.slice(dataUrl.indexOf(',') + 1);
  const buf = Buffer.from(base64, 'base64');
  return Promise.resolve(new Uint8Array(buf.buffer, buf.byteOffset, buf.byteLength));
}
