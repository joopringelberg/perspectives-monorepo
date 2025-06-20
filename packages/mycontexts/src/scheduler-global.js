// This file provides a simplified scheduler API for global use

// Basic priority levels
export const unstable_ImmediatePriority = 1;
export const unstable_UserBlockingPriority = 2;
export const unstable_NormalPriority = 3;
export const unstable_LowPriority = 4;
export const unstable_IdlePriority = 5;

// Basic scheduler functions 
export function unstable_runWithPriority(priorityLevel, fn) {
  try {
    return fn();
  } catch (error) {
    console.error('Error in scheduled callback:', error);
    return null;
  }
}

export function unstable_next(fn) {
  return setTimeout(() => {
    try {
      fn();
    } catch (error) {
      console.error('Error in next callback:', error);
    }
  }, 0);
}

export function unstable_scheduleCallback(priorityLevel, callback, options) {
  const delay = options?.delay || 0;
  const timeoutId = setTimeout(() => {
    try {
      callback();
    } catch (error) {
      console.error('Error in scheduled callback:', error);
    }
  }, delay);
  
  return { id: timeoutId };
}

export function unstable_cancelCallback(callbackNode) {
  if (callbackNode && callbackNode.id) {
    clearTimeout(callbackNode.id);
  }
}

export function unstable_wrapCallback(callback) {
  return function() {
    try {
      return callback.apply(this, arguments);
    } catch (error) {
      console.error('Error in wrapped callback:', error);
      return null;
    }
  };
}

export function unstable_getCurrentPriorityLevel() {
  return unstable_NormalPriority;
}

export function unstable_shouldYield() {
  return false;
}

export function unstable_requestPaint() {
  // No-op in this simple implementation
}

export function unstable_continueExecution() {
  // No-op in this simple implementation
}

export function unstable_pauseExecution() {
  // No-op in this simple implementation
}

export function unstable_getFirstCallbackNode() {
  return null;
}

export const unstable_Profiling = null;

// Default export
export default {
  unstable_ImmediatePriority,
  unstable_UserBlockingPriority,
  unstable_NormalPriority,
  unstable_LowPriority,
  unstable_IdlePriority,
  unstable_runWithPriority,
  unstable_next,
  unstable_scheduleCallback,
  unstable_cancelCallback,
  unstable_wrapCallback,
  unstable_getCurrentPriorityLevel,
  unstable_shouldYield,
  unstable_requestPaint,
  unstable_continueExecution,
  unstable_pauseExecution,
  unstable_getFirstCallbackNode,
  unstable_Profiling
};