// import {PDRproxy} from "perspectives-proxy";

import {PDRproxy, FIREANDFORGET, RoleInstanceT, PropertyType, RoleType, ValueT, PropertySerialization, ContextInstanceT, RoleOnClipboard, InputType} from "perspectives-proxy";
import {deconstructContext, isQualifiedName} from "./urifunctions";
import { default as ModelDependencies } from "./modelDependencies";
import {UserMessagingPromise} from "./userMessaging.js";
import i18next from "i18next";
import { PSRolType } from "./reactcontexts";
import { t } from "i18next";
import React from 'react';
// import { useLongPress } from 'use-long-press';

/*
This module gives functions that add behaviour to a component that represents a role.
The functions build on the assumption that this role component has a PSRol context.
We will call that component a `Card` here, though there is no reason to use a Reactbootstrap.Card component.
However, it must be class based (implemented by extending React Component and it must implement the
CardProperties interface.

Add behaviours to the Card by using the AdorningComponentWrapper Component.

IMPLEMENTATION NOTES.
1. The behaviours stack handlers. However, for the dragstart event that is not possible. Hence
we check whether a handler exists before installing one and will call the old one if appropriate.

2. As soon as one behaviour allows dragging, this would unlock all behaviours accessible by dropping the target, e.g.
on a tool in the bar or on a dropzone. To prevent this, we annotate the React element with the behaviours added to it.
On dropping, we check those annotations to prevent behaviour that was not added (e.g. when dropping on the Trash we make sure
that the removerolefromcontext behaviour was added to the origin component).
The behaviour annotations are kept in an array of strings in the member "addedBehaviour". Possible values are:
  - openContextOrRoleForm
  - fillWithARole
  - fillARole
  - removeFiller
  - removeRoleFromContext
*/

////////////////////////////////////////
// THE CARD INTERFACES
////////////////////////////////////////
export interface InnerCardProperties {
  title?: string;
  "aria-label": string;
  className?: string;
  type?: InputType
}

export interface CardProperties extends InnerCardProperties{
  tabIndex?: number;
  // This handler has nothing to do with behaviour. It is used to add column- and row selection in a perspectives table.
  onClick?: (event: React.MouseEvent) => void;
}

////////////////////////////////////////
// THE BEHAVIOUR COMPONENT
////////////////////////////////////////
export interface BehaviourComponentProps {
  cardprop?: PropertyType;
  myroletype: RoleType;
  systemExternalRole: RoleInstanceT;
}

export interface BehaviourComponent extends React.Component<BehaviourComponentProps> {
  context: PSRolType;
  addedBehaviour: string[];
}

// All the functions that add a behaviour have type BehaviourAdder.
export type BehaviourAdder = (domEl: HTMLElement, component: BehaviourComponent, title?: string) => void;

////////////////////////////////////////
// OPEN CONTEXT OR ROLE FORM
////////////////////////////////////////
// adds a doubleclick handler and a keydown handler.
//  - doubleclick opens in the same screen;
//  - shift-doubleclick opens in another screen (tab or window).
//  - shift-space opens the contextrole or external rol in the same screen;
//  - alt-shift-space opens it in another screen (tab or window).
// If the role has rolekind RoleInContext or UserRole, the RoleForm is opened.
// If the role has rolekind ContextRole or ExternalRole, the corresponding context is opened, unless FormMode is active.
// In that case, the RoleForm is opened.
// The component that adds this behaviour may receive three additional props:
//  * cardprop, that makes a card appear on the form, representing the role itself, when specified.
// The component should have a PSRol context. This is provided by ExternalRole, RoleInstance and RoleInstanceIterator.
// Consequently, we can use this behaviour in the PerspectivesBasedForm and in TableCell, on a selected card that is not editable.
export const addOpenContextOrRoleForm : BehaviourAdder = (domEl, component) => {
  function handle(onNewTab: boolean) {
    const roleKind = component.context.roleKind;
    const appLocation = location.origin + location.pathname;
    if (roleKind == "ContextRole" || roleKind == "ExternalRole") {
      if (onNewTab) {
        window.open(appLocation + "?opencontext=" + encodeURIComponent(component.context.rolinstance!), "mycontexts", "left=100,top=100,width=800,height=600");
      } else {
        domEl.dispatchEvent(new CustomEvent('OpenContext', { detail: component.context.rolinstance, bubbles: true }));
      }
    } 
  }

  function handleDoubleClick(e: MouseEvent) {
    handle((e.shiftKey || e.altKey));
    // NOTE: We have to let the event propagate because we want to catch it in TableRow, where it is used to open the details form.
    // We may later discover situations where we want to stop the propagation but that has to happen higher up.
    // e.stopPropagation();
  }

  function handleKeyDown(e: KeyboardEvent) {
    switch (e.code) {
      case "Enter": // Return
      case "Space": // Space
        if (component.context.isselected) {
          if (e.shiftKey) {
            handle(e.altKey);
            // I've commented this out to enable opening the details form of the master-slave component through the keyboard.
            // e.stopPropagation();
          }
        }
    }
  }

  const previousOnDragStart = domEl.ondragstart;

  addBehaviour(component, "openContextOrRoleForm", function (component : BehaviourComponent) {
    domEl.addEventListener("keydown", handleKeyDown);
    domEl.addEventListener("dblclick", handleDoubleClick);

    if (previousOnDragStart) {
      domEl.ondragstart = function (ev) {
        previousOnDragStart.call(domEl, ev);
      };
    } else {
      domEl.ondragstart = function (ev) {
        const payload = JSON.stringify({
          roleData: component.context,
          addedBehaviour: component.addedBehaviour,
          myroletype: component.props.myroletype
        });
        if (ev.dataTransfer) {
          ev.dataTransfer.setData("PSRol", payload);
        }
      };
    }
  });
}

////////////////////////////////////////
// FILL WITH A ROLE
////////////////////////////////////////
export const addFillWithRole : BehaviourAdder = (domEl: HTMLElement, component: BehaviourComponent) => {
  function readClipBoard(receiveResults: (data: RoleOnClipboard | null) => void) {
    PDRproxy.then(function (pproxy) {
      pproxy.getSelectedRoleFromClipboard().then( receiveResults );
    });
  }

  function handleKeyDown(event: KeyboardEvent) {
    if (event.ctrlKey) {
      switch (event.key) {
        case 'v':
          event.preventDefault();
          event.stopPropagation();
          readClipBoard(function (roleDataAndBehaviour) {
            if (roleDataAndBehaviour) {
              tryToBind(event, roleDataAndBehaviour);
            }
          });
      }
    }
  }

  function tryToBind(event: Event, { roleData, addedBehaviour }: RoleOnClipboard) {
    if (addedBehaviour.includes("fillARole")) {
      component.context.checkbinding(roleData).then( function (bindingAllowed: boolean) {
        if (bindingAllowed) {
          component.context.bind_(roleData);
          PDRproxy.then((pproxy) =>
            pproxy.removeRoleFromClipboard(roleData.rolinstance) ).catch((e) =>
            UserMessagingPromise.then((um) =>
              um.addMessageForEndUser({
                title: i18next.t("clipboardEmpty_title", { ns: "preact" }),
                message: i18next.t("clipboardEmpty_message", { ns: "preact" }),
                error: e.toString(),
              })
            )
          );
        } else {
          domEl.classList.add("border-danger", "border");
          UserMessagingPromise.then((um) =>
            um.addMessageForEndUser({
              title: i18next.t("fillerNotAllowed_title", { ns: "preact" }),
              message: i18next.t("fillerNotAllowed_message", { ns: "preact" }),
              error: "",
            })
          );
        }
      });
    }
  }

  addBehaviour(component, "fillWithARole", function (component: BehaviourComponent) {
    domEl.addEventListener("dragenter", (event) => {
      event.preventDefault();
      event.stopPropagation();
      domEl.setAttribute("tabIndex", "0");
      domEl.focus();
    });

    domEl.addEventListener("dragover", (event) => {
      event.preventDefault();
      event.stopPropagation();
    });

    domEl.addEventListener("dragleave", (ev) => {
      if (ev.target) {
        (ev.target as HTMLElement).classList.remove("border-danger", "border", "border-success");
      }
    });
    domEl.addEventListener("blur", (ev) => {
      if (ev.target) {
        (ev.target as HTMLElement).classList.remove("border-danger", "border", "border-success");
      }
    });

    domEl.addEventListener("drop", (ev) => {
      tryToBind(ev, JSON.parse(ev.dataTransfer?.getData("PSRol") || "{}"));
    });

    domEl.addEventListener("keydown", handleKeyDown);
  });
}

////////////////////////////////////////
// FILL A ROLE
////////////////////////////////////////
// Makes the Card draggable.
// Adds keydown behaviour: ctrl-c will put the Card on the Card clipboard.

export const addFillARole : BehaviourAdder = (domEl, component, title) => {
  function handleKeyDown(event: KeyboardEvent) {
    switch (event.key) {
      case 'c':
        if (event.ctrlKey) {
          event.preventDefault();
          event.stopPropagation();
          copy(event);
        }
    }
  }


  function copy(event: Event) {
      navigator.clipboard.writeText(component.context.rolinstance!);
      addRoleToClipboard(
        component.context.rolinstance!, 
        {
          roleData: {
            rolinstance: component.context.rolinstance!,
            cardTitle: title || "No title",
            roleType: component.context.roltype,
            contextType: component.context.contexttype,
          },
          addedBehaviour: component.addedBehaviour,
          myroletype: component.props.myroletype,
        },
        component.props.systemExternalRole,
        component.props.myroletype
        ).catch((e) =>
            UserMessagingPromise.then((um) =>
              um.addMessageForEndUser({
                title: i18next.t("clipboardSet_title", { ns: "preact" }),
                message: i18next.t("clipboardSet_message", { ns: "preact" }),
                error: e.toString(),
              })
            )
          );
  }

  addBehaviour(component, "fillARole", function (component: BehaviourComponent) {
    if (!domEl.ondragstart) {
        (domEl.ondragstart = (ev: DragEvent) => {
          const payload = JSON.stringify({
            roleData: component.context,
            cardTitle: title || "No title",
            addedBehaviour: component.addedBehaviour,
            myroletype: component.props.myroletype,
          });
          if (ev.dataTransfer) {
            ev.dataTransfer.setData("PSRol", payload);
          }
        });
    }
    domEl.draggable = true;
    domEl.addEventListener("keydown", handleKeyDown);
  });
}

////////////////////////////////////////
// REMOVE A FILLER
////////////////////////////////////////
// Makes the Card draggable, so it can be dropped in the Unbind tool.
// Adds keydown behaviour for delete.

export const addRemoveFiller : BehaviourAdder = (domEl, component) => {
  function handleKeyDown(event: KeyboardEvent) {
    switch (event.code) {
      case "Backspace": // Backspace
        if (event.shiftKey) {
          PDRproxy.then(pproxy =>
            pproxy.getBinding(component.context.rolinstance!, function (rolIdArr : RoleInstanceT[]) {
              if (rolIdArr[0]) {
                pproxy
                  .removeBinding(component.context.rolinstance!, component.props.myroletype )
                  .catch(e => UserMessagingPromise.then(um =>
                    um.addMessageForEndUser({
                      title: i18next.t("unfill_title", { ns: 'preact' }),
                      message: i18next.t("unfill_message", { ns: 'preact' }),
                      error: e.toString()
                    })));
              }
            },
              FIREANDFORGET));
          event.preventDefault();
          event.stopPropagation();
        }
    }
  }

  addBehaviour(component, "removeFiller", function (component: BehaviourComponent) {
    domEl.addEventListener("keydown", handleKeyDown);
    domEl.draggable = true;
    // Notice that this code is highly contextual.
    // It may have to change if the other behaviours that add dragstart methods
    // change.
    if (!domEl.ondragstart) {
      domEl.ondragstart = ev => {
        const payload = JSON.stringify(
          {
            roleData: component.context,
            addedBehaviour: component.addedBehaviour,
            myroletype: component.props.myroletype
          }
        );
        if (ev.dataTransfer) {
          ev.dataTransfer.setData("PSRol", payload);
        }
      };
    }
  });
}

////////////////////////////////////////
// REMOVE ROLE FROM CONTEXT
////////////////////////////////////////
// Makes the Card draggable, so it can be dropped in the Trash.
// Adds keydown behaviour for shift-delete.
export const addRemoveRoleFromContext : BehaviourAdder = (domEl, component) => {
  function handleKeyDown(event: KeyboardEvent) {
    switch (event.code) {
      case 'Backspace':
        event.preventDefault();
        event.stopPropagation();
        component.context.removerol();
    }
  }

  addBehaviour(component, "removeRoleFromContext", function (component: BehaviourComponent) {
    domEl.addEventListener("keydown", handleKeyDown);
    domEl.draggable = true;
    
    // Add swipe to delete functionality
    addSwipeToDelete(
      domEl, 
      component, 
      () => component.context.removerol(),
      i18next.t("swipe_remove_role_confirm", { ns: "preact", defaultValue: "Remove this role?" })
    );
    
    // Existing drag behavior
    if (!domEl.ondragstart) {
      domEl.ondragstart = (ev: DragEvent) => {
        const payload = JSON.stringify({
          roleData: component.context,
          addedBehaviour: component.addedBehaviour,
          myroletype: component.props.myroletype,
        });
        if (ev.dataTransfer) {
          ev.dataTransfer.setData("PSRol", payload);
        }
      };
    }
  });
}

////////////////////////////////////////
// REMOVE CONTEXT
////////////////////////////////////////

// Makes the Card draggable, so it can be dropped in the Trash.
// Adds keydown behaviour for shift-delete.
export const addRemoveContext : BehaviourAdder = (domEl, component) => {
  function handleKeyDown(event: KeyboardEvent) {
    switch (event.code) {
      case "Backspace": // Backspace
        event.preventDefault();
        event.stopPropagation();
        component.context.removecontext();
    }
  }

  addBehaviour(component, "addRemoveContext", function (component: BehaviourComponent) {
    domEl.addEventListener("keydown", handleKeyDown);
    domEl.draggable = true;
    
    // Add swipe to delete functionality
    addSwipeToDelete(
      domEl, 
      component, 
      () => component.context.removecontext(),
      i18next.t("swipe_remove_context_confirm", { ns: "preact", defaultValue: "Remove this context?" })
    );
    
    // Existing drag behavior
    if (!domEl.ondragstart) {
      domEl.ondragstart = (ev: DragEvent) => {
        const payload = JSON.stringify({
          roleData: component.context,
          addedBehaviour: component.addedBehaviour,
          myroletype: component.props.myroletype,
        });
        if (ev.dataTransfer) {
          ev.dataTransfer.setData("PSRol", payload);
        }
      };
    }
  });
}

////////////////////////////////////////
// SWIPE BEHAVIOUR
////////////////////////////////////////
// Extend HTMLElement to allow __swipeCleanup property
interface HTMLElementWithSwipeCleanup extends HTMLElement {
  __swipeCleanup?: () => void;
}

function addSwipeToDelete(
  domEl: HTMLElementWithSwipeCleanup, 
  component: BehaviourComponent, 
  deleteAction: () => void,
  confirmText: string
) {
  let touchStartX = 0;
  let touchEndX = 0;
  const minSwipeDistance = 100; // Minimum distance in pixels to trigger swipe
  
  // Create a confirmation element that appears after swiping
  const confirmElement = document.createElement('div');
  confirmElement.className = 'swipe-confirm-element';
  confirmElement.innerHTML = `
    <div class="swipe-confirm-message">${confirmText}</div>
    <div class="swipe-confirm-buttons">
      <button class="btn btn-sm btn-danger confirm-delete">Delete</button>
      <button class="btn btn-sm btn-secondary cancel-delete">Cancel</button>
    </div>
  `;
  confirmElement.style.position = 'absolute';
  confirmElement.style.backgroundColor = 'rgba(255, 255, 255, 0.95)';
  confirmElement.style.padding = '10px';
  confirmElement.style.borderRadius = '5px';
  confirmElement.style.boxShadow = '0 2px 5px rgba(0, 0, 0, 0.2)';
  confirmElement.style.zIndex = '1000';
  confirmElement.style.display = 'none';
  
  // Add it to the DOM element
  domEl.style.position = 'relative';
  domEl.appendChild(confirmElement);
  
  // Attach delete and cancel handlers
  const confirmButton = confirmElement.querySelector('.confirm-delete');
  const cancelButton = confirmElement.querySelector('.cancel-delete');
  
  if (confirmButton) {
    confirmButton.addEventListener('click', (e) => {
      e.stopPropagation();
      hideConfirmElement();
      deleteAction();
    });
  }
  
  if (cancelButton) {
    cancelButton.addEventListener('click', (e) => {
      e.stopPropagation();
      hideConfirmElement();
    });
  }
  
  function showConfirmElement(clientX: number) {
    // Position the confirm element near where the user finished swiping
    const rect = domEl.getBoundingClientRect();
    confirmElement.style.left = `${Math.min(clientX - rect.left, rect.width - confirmElement.offsetWidth)}px`;
    confirmElement.style.top = `${rect.height / 2 - confirmElement.offsetHeight / 2}px`;
    confirmElement.style.display = 'block';
    
    // Add a timeout to auto-hide the confirmation after 5 seconds of inactivity
    setTimeout(() => {
      if (confirmElement.style.display === 'block') {
        hideConfirmElement();
      }
    }, 5000);
  }
  
  function hideConfirmElement() {
    confirmElement.style.display = 'none';
  }

  // Touch event handlers
  function handleTouchStart(e: TouchEvent) {
    touchStartX = e.touches[0].clientX;
    domEl.classList.add('swiping'); // Optional: add visual feedback
  }
  
  function handleTouchMove(e: TouchEvent) {
    if (touchStartX === 0) return;
    
    const currentX = e.touches[0].clientX;
    const diff = touchStartX - currentX;
    
    // If swiping left
    if (diff > 0) {
      // Optional: add visual feedback as user swipes
      const swipePercentage = Math.min((diff / minSwipeDistance) * 100, 100);
      domEl.style.transform = `translateX(-${swipePercentage * 0.2}%)`;
      domEl.style.opacity = `${1 - (swipePercentage / 300)}`;
      
      // Prevent scrolling while swiping
      if (diff > 10) {
        e.preventDefault();
      }
    }
  }
  
  function handleTouchEnd(e: TouchEvent) {
    touchEndX = e.changedTouches[0].clientX;
    const swipeDistance = touchStartX - touchEndX;
    
    // Reset visual effects
    domEl.style.transform = '';
    domEl.style.opacity = '';
    domEl.classList.remove('swiping');
    
    // Check if the swipe was long enough and in the left direction
    if (swipeDistance > minSwipeDistance) {
      // Show confirmation dialog
      showConfirmElement(touchEndX);
    }
    
    // Reset values
    touchStartX = 0;
    touchEndX = 0;
  }

  // Add event listeners
  domEl.addEventListener('touchstart', handleTouchStart, { passive: false });
  domEl.addEventListener('touchmove', handleTouchMove, { passive: false });
  domEl.addEventListener('touchend', handleTouchEnd);
  
  // Store cleanup function on the element to be called when behavior is removed
  const cleanup = () => {
    domEl.removeEventListener('touchstart', handleTouchStart);
    domEl.removeEventListener('touchmove', handleTouchMove);
    domEl.removeEventListener('touchend', handleTouchEnd);
    if (confirmElement.parentNode === domEl) {
      domEl.removeChild(confirmElement);
    }
  };
  
  // Store cleanup function for later use
  domEl.__swipeCleanup = cleanup;
}
////////////////////////////////////////
// CONDITIONALLY ADD BEHAVIOUR
////////////////////////////////////////

function addBehaviour(component: BehaviourComponent, behaviour: string, behaviourAdder: ((component: BehaviourComponent) => void)): void {
  // First check if the behaviour is already added. For this we keep a list of behaviour names in the addedBehaviour member.
  if (component.addedBehaviour) {
    if (!component.addedBehaviour.find(b => b === behaviour)) {
      // If not, add it.
      component.addedBehaviour.push(behaviour);
      behaviourAdder(component);
    }
  } else {
    // If the addedBehaviour member is not initialized, we initialize it with the behaviour and add the behaviour.
    component.addedBehaviour = [behaviour];
    behaviourAdder(component);
  }
}

////////////////////////////////////////
// MAP BEHAVIOUR NAMES TO ADDERS
////////////////////////////////////////

const behaviourMap: { [key: string]: BehaviourAdder } = {
  addOpenContextOrRoleForm: addOpenContextOrRoleForm,
  addFillWithARole: addFillWithRole,
  addFillARole: addFillARole,
  addRemoveFiller: addRemoveFiller,
  addRemoveRoleFromContext: addRemoveRoleFromContext,
  addRemoveContext: addRemoveContext,
};

export function getBehaviourAdder(name: string): BehaviourAdder {
  return behaviourMap[name];
}

////////////////////////////////////////
// ADD A ROLE TO CLIPBOARD
////////////////////////////////////////
export function addRoleToClipboard(roleInstance : RoleInstanceT, roleData: RoleOnClipboard,systemExternalRole : RoleInstanceT, myroletype : RoleType) : Promise<RoleInstanceT | void>
{
  const properties: PropertySerialization = {};
  properties[ModelDependencies.itemOnClipboardSelected] =  ["true" as ValueT ];
  properties[ModelDependencies.itemOnClipboardClipboardData] = [JSON.stringify(roleData) as ValueT];
  const itemOnClipboard = 
    { id : undefined // We let the core create an id for the item.
    , properties
    , binding: roleInstance
    }
  return PDRproxy.then((pproxy) => {
      pproxy.bind( deconstructContext(systemExternalRole) as ContextInstanceT,
      ModelDependencies.itemsOnClipboard,
      ModelDependencies.system,
      itemOnClipboard,
      myroletype
      ).then( uniqueFiller => {
        if (!uniqueFiller)
        {
          UserMessagingPromise.then( um => 
            um.addMessageForEndUser(
              { title: i18next.t("fillRole_title", { ns: 'preact' }) 
              , message: i18next.t("fillRole_filler_has_been_used", {ns: 'preact'})
              , error: i18next.t("fillRole_error", {ns: 'preact'})
              }));
        }})
    });
  }