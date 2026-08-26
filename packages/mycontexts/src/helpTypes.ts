import { createContext } from 'react';
import type { ContextInstanceT, ContextType, RoleType } from 'perspectives-proxy';

export type HelpTarget =
  | {
      kind: 'context';
      contextInstance: ContextInstanceT;
      contextType: ContextType;
      userRoleType: RoleType;
      label: string;
    }
  | {
      kind: 'role';
      contextInstance: ContextInstanceT;
      contextType: ContextType;
      userRoleType: RoleType;
      roleType: RoleType;
      perspectiveId: string;
      label: string;
    };

export interface HelpActivation {
  target: HelpTarget;
  anchorRect: DOMRect;
  triggerElement: HTMLElement;
}

export interface HelpModeContextValue {
  active: boolean;
  openHelp: (activation: HelpActivation) => void;
}

export const HelpModeContext = createContext<HelpModeContextValue>({
  active: false,
  openHelp: () => undefined,
});