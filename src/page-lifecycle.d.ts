declare module 'page-lifecycle' {
  export type LifecycleState =
    | 'active'
    | 'passive'
    | 'hidden'
    | 'frozen'
    | 'terminated';

  export interface LifecycleEvent extends Event {
    oldState: LifecycleState;
    newState: LifecycleState;
  }

  export function addEventListener(
    type: 'statechange',
    listener: (event: LifecycleEvent) => void,
    options?: boolean | AddEventListenerOptions
  ): void;

  export function removeEventListener(
    type: 'statechange',
    listener: (event: LifecycleEvent) => void,
    options?: boolean | EventListenerOptions
  ): void;

  export function addUnsavedChanges(label: string): void;
  export function removeUnsavedChanges(label: string): void;
}