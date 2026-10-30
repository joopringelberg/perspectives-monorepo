
export function bytesToBase64DataUrlImpl (bytes) {
  const type = "application/octet-stream";
  return new Promise((resolve, reject) => 
  {
    const reader = Object.assign(new FileReader(), 
      {
        onload: function() {return resolve(reader.result)},
        onerror: function() { return reject(reader.error) },
      });
    reader.readAsDataURL(new File([bytes], "", { type: type }));
  })
}

export function dataUrlToBytesImpl (dataUrl) 
{
  return fetch(dataUrl)
    .then( function(res) 
      { 
        return res.arrayBuffer()
      })
    .then( function (buff) { return new Uint8Array( buff) } ) 
  }

function arrayBufferToBase64(buffer) {
  if (typeof Buffer !== 'undefined') {
    return Buffer.from(buffer).toString('base64');
  }
  let binary = '';
  const bytes = new Uint8Array(buffer);
  const len = bytes.byteLength;
  for (let i = 0; i < len; i++) {
    binary += String.fromCharCode(bytes[i]);
  }
  return btoa(binary);
}

function base64ToUint8Array(base64) {
  if (typeof Buffer !== 'undefined') {
    return new Uint8Array(Buffer.from(base64, 'base64'));
  }
  const binary = atob(base64);
  const bytes = new Uint8Array(binary.length);
  for (let i = 0; i < binary.length; i++) {
    bytes[i] = binary.charCodeAt(i);
  }
  return bytes;
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
