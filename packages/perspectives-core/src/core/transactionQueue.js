// SPDX-FileCopyrightText: 2026 Joop Ringelberg (joopringelberg@gmail.com), Cor Baars
// SPDX-License-Identifier: GPL-3.0-or-later

export const newTransactionQueue = () => ({
  active: null,
  users: [],
  background: [],
  userBurst: 0
});

function dispatch(queue) {
  if (queue.active) return;
  const useUser = queue.users.length > 0 &&
    (queue.background.length === 0 || queue.userBurst < 4);
  const next = (useUser ? queue.users : queue.background).shift();
  if (!next) {
    queue.userBurst = 0;
    return;
  }
  queue.userBurst = useUser ? queue.userBurst + 1 : 0;
  queue.active = next;
  next.ready();
}

// The returned effect both removes a cancelled waiter and releases an admitted job.
export const enqueue = queue => user => ready => () => {
  const waiting = user ? queue.users : queue.background;
  const job = { ready };
  waiting.push(job);
  dispatch(queue);
  let released = false;
  return () => {
    if (released) return;
    released = true;
    if (queue.active === job) {
      queue.active = null;
    } else {
      const index = waiting.indexOf(job);
      if (index >= 0) waiting.splice(index, 1);
    }
    dispatch(queue);
  };
};
