import React, { useRef } from 'react';
import { MainContentProps, SlidingPanelContentProps } from './mscomponent';


type RoleInstance = string;
type RoleInstanceSelectionEvent = Event & { detail: { roleInstance: RoleInstance } };

export const MainContentStub: React.FC<MainContentProps> = ({ className }) => {
  const divRef = useRef<HTMLDivElement>(null);

  const setSelectedRoleInstance = (rid: RoleInstance) => {
    if (divRef.current) {
      const event: RoleInstanceSelectionEvent = new CustomEvent('SetSelectRow', { detail: { roleInstance: rid } });
      divRef.current.dispatchEvent(event);
    }
  };
  return (
    <div className={className} ref={divRef}>
      <ul>
        <li onClick={() => setSelectedRoleInstance('kaas')}>Kaas</li>
        <li onClick={() => setSelectedRoleInstance('boter')}>Boter</li>
        <li onClick={() => setSelectedRoleInstance('eieren')}>Eieren</li>
      </ul>
    </div>
  );
};

export const SlidingPanelContentStub: React.FC<SlidingPanelContentProps> = ({ className, selectedRoleInstance }) => {
  return <p className={className}>A form showing the details of {selectedRoleInstance}</p>;
};