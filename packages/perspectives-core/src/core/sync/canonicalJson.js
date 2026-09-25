const canonicalize = (value) => {
  if (Array.isArray(value)) {
    return value.map(canonicalize);
  }

  if (value && typeof value === "object") {
    return Object.keys(value)
      .sort()
      .reduce((result, key) => {
        result[key] = canonicalize(value[key]);
        return result;
      }, {});
  }

  if (typeof value === "number" && !Number.isFinite(value)) {
    throw new TypeError("Canonical delta JSON does not support non-finite numbers.");
  }

  return value;
};

export const canonicalizeJsonStringImpl = (jsonString) =>
  JSON.stringify(canonicalize(JSON.parse(jsonString)));

export const computeDeltaIdImpl = async (author, payload) => {
  const encoder = new TextEncoder();
  const bytes = encoder.encode(author + payload);
  const hash = await crypto.subtle.digest("SHA-256", bytes);
  return Array.from(new Uint8Array(hash), (byte) => byte.toString(16).padStart(2, "0")).join("");
};
