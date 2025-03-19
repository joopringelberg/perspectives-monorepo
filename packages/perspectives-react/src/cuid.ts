import {init} from '@paralleldrive/cuid2';

// A function that generates a CUID using the current epoch as fingerprint.
export const cuid2 = init({
  // A custom random function with the same API as Math.random.
  // You can use this to pass a cryptographically secure random function.
  random: Math.random,
  // the length of the id
  length: 10,
  // A custom fingerprint for the host environment. This is used to help
  // prevent collisions when generating ids in a distributed system.
  fingerprint: Date.now().toString(36),
});
