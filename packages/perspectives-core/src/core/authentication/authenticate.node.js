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

function arrayBufferToBase64(buffer) {
  return Buffer.from(buffer).toString('base64');
}

function base64ToUint8Array(base64) {
  return new Uint8Array(Buffer.from(base64, 'base64'));
}

export function encryptForRecipientsImpl(payload, recipients) {
  return (async () => {
    const contentKey = await crypto.subtle.generateKey(
      { name: 'AES-GCM', length: 256 },
      true,
      ['encrypt', 'decrypt']
    );
    const iv = crypto.getRandomValues(new Uint8Array(12));
    const ciphertext = await crypto.subtle.encrypt(
      { name: 'AES-GCM', iv },
      contentKey,
      new TextEncoder().encode(payload)
    );
    const rawContentKey = await crypto.subtle.exportKey('raw', contentKey);
    const wrappedKeys = await Promise.all(
      recipients.map(async ({ recipient, transportKey }) => {
        const publicKey = await crypto.subtle.importKey(
          'jwk',
          JSON.parse(transportKey),
          { name: 'RSA-OAEP', hash: 'SHA-256' },
          true,
          ['encrypt']
        );
        const wrappedKey = await crypto.subtle.encrypt(
          { name: 'RSA-OAEP' },
          publicKey,
          rawContentKey
        );
        return { recipient, wrappedKey: arrayBufferToBase64(wrappedKey) };
      })
    );

    return {
      ciphertext: arrayBufferToBase64(ciphertext),
      iv: arrayBufferToBase64(iv.buffer),
      wrappedKeys
    };
  })();
}

export function decryptForRecipientImpl(ciphertext, iv, wrappedKey, privateKey) {
  return (async () => {
    const rawContentKey = await crypto.subtle.decrypt(
      { name: 'RSA-OAEP' },
      privateKey,
      base64ToUint8Array(wrappedKey)
    );
    const contentKey = await crypto.subtle.importKey(
      'raw',
      rawContentKey,
      { name: 'AES-GCM', length: 256 },
      false,
      ['decrypt']
    );
    const cleartext = await crypto.subtle.decrypt(
      { name: 'AES-GCM', iv: base64ToUint8Array(iv) },
      contentKey,
      base64ToUint8Array(ciphertext)
    );
    return new TextDecoder().decode(cleartext);
  })();
}