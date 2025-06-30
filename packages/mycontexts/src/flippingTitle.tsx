import React, { useEffect, useState } from 'react';
import { RoleType } from 'perspectives-proxy';
import { deconstructLocalName } from 'perspectives-react';

interface FlippingTitleProps {
  title: string;
  userRoleType?: string;
  revertAfterMs?: number;
  showTitleClass?: string;
  showRoleClass?: string;
}

const FlippingTitle: React.FC<FlippingTitleProps> = ({ 
  title, 
  userRoleType, 
  revertAfterMs = 3000, 
  showTitleClass = "text-light", 
  showRoleClass = "text-dark bg-primary-subtle p-1 rounded" 
}) => {
  const [showUserRole, setShowUserRole] = useState<boolean>(false);
  const [timer, setTimer] = useState<number | null>(null);

  // Format the role type to be more readable
  const formattedRoleType = userRoleType ? userRoleType : '';
  
  // Function to show role type and then revert
  const flipToRoleAndRevert = () => {
    setShowUserRole(true);
    
    // Clear any existing timer
    if (timer) {
      window.clearTimeout(timer);
    }
    
    // Set new timer to revert back to title
    const newTimer = window.setTimeout(() => {
      setShowUserRole(false);
    }, revertAfterMs);
    
    setTimer(newTimer as unknown as number);
  };

  // Show role type briefly on initial render
  useEffect(() => {
    if (userRoleType) {
      flipToRoleAndRevert();
    }
    
    // Clean up timer on unmount
    return () => {
      if (timer) {
        window.clearTimeout(timer);
      }
    };
  }, [userRoleType]); // Re-run when userRoleType changes

  return (
    <h1 
      className={`fs-4 mb-0 cursor-pointer ${showUserRole ? showRoleClass : showTitleClass}`}
      onClick={flipToRoleAndRevert}
      onKeyDown={(e) => {
        if (e.key === 'Enter' || e.key === ' ') {
          flipToRoleAndRevert();
          e.preventDefault();
        }
      }}
      tabIndex={0}
      role="button"
      aria-label={showUserRole 
        ? `Current role: ${formattedRoleType}. Click to show context name.` 
        : `Context: ${title}. Click to show your role.`}
    >
      {showUserRole ? formattedRoleType : title}
    </h1>
  );
};

export default FlippingTitle;