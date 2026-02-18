import React, { useEffect, useRef, useState } from 'react';
import { deconstructLocalName, i18next } from 'perspectives-react';

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
  const [canShowBoth, setCanShowBoth] = useState<boolean>(false);

  const headingRef = useRef<HTMLHeadingElement | null>(null);
  const measureRef = useRef<HTMLSpanElement | null>(null);

  // Format the role type to be more readable
  const formattedRoleType = userRoleType ? i18next.t("flipping_title", {ns: 'mycontexts', userRoleType }) : '';
  
  // Determine if there is enough space to show both title and role
  // side by side. We measure a hidden span with the combined text
  // and compare it to the available width in the brand area.
  useEffect(() => {
    const updateCanShowBoth = () => {
      if (!userRoleType || !headingRef.current || !measureRef.current) {
        setCanShowBoth(false);
        return;
      }

      const headingEl = headingRef.current;

      // Find the enclosing navbar and the elements that form the
      // true horizontal boundaries: the NavDropdown on the left
      // and the InternetConnectivityCheck spacer on the right.
      const navbarEl = headingEl.closest('#top-navbar') as HTMLElement | null
        || document.getElementById('top-navbar');

      if (!navbarEl) {
        setCanShowBoth(false);
        return;
      }

      const leftEl = navbarEl.querySelector('#nav-dropdown') as HTMLElement | null;
      const rightEl = navbarEl.querySelector('.internet-connectivity-check') as HTMLElement | null;

      if (!leftEl || !rightEl) {
        setCanShowBoth(false);
        return;
      }

      const leftRect = leftEl.getBoundingClientRect();
      const rightRect = rightEl.getBoundingClientRect();

      // Available horizontal space for the title+role is the gap
      // between the right edge of the dropdown and the left edge
      // of the connectivity component, minus a small padding.
      const horizontalPadding = 16; // px total safety margin
      const availableWidth = Math.max(rightRect.left - leftRect.right - horizontalPadding, 0);

      const combinedWidth = measureRef.current.offsetWidth;
      
      setCanShowBoth(prev => {
        const next = combinedWidth <= availableWidth;
        return prev === next ? prev : next;
      });
    };

    updateCanShowBoth();
    window.addEventListener('resize', updateCanShowBoth);
    return () => {
      window.removeEventListener('resize', updateCanShowBoth);
    };
  }, [title, formattedRoleType, userRoleType]);

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

  // If there is enough space and we have a role, show both title
  // and role statically; otherwise, use the flipping behaviour.
  const interactive = !!userRoleType && !canShowBoth;

  return (
    <h1
      ref={headingRef}
      className={`fs-4 mb-0 ${interactive ? 'cursor-pointer' : ''} ${showUserRole ? showRoleClass : showTitleClass}`}
      onClick={interactive ? flipToRoleAndRevert : undefined}
      onKeyDown={interactive ? (e => {
        if (e.key === 'Enter' || e.key === ' ') {
          flipToRoleAndRevert();
          e.preventDefault();
        }
      }) : undefined}
      tabIndex={interactive ? 0 : -1}
      role={interactive ? 'button' : undefined}
      aria-label={interactive
        ? (showUserRole
          ? `${formattedRoleType}. Click to show context name.`
          : `Context: ${title}. Click to show your role.`)
        : undefined}
      style={{ position: 'relative' }}
    >
      {/* Hidden measurement span for combined title + role text */}
      <span
        ref={measureRef}
        aria-hidden="true"
        style={{
          position: 'absolute',
          visibility: 'hidden',
          whiteSpace: 'nowrap',
          pointerEvents: 'none',
        }}
      >
        {title}{formattedRoleType ? ` ${formattedRoleType}` : ''}
      </span>

      {canShowBoth && formattedRoleType
        ? (
          <>
            <span className={showTitleClass}>{formattedRoleType}</span>
            {' in'}
            <span >{title}</span>
          </>
        )
        : (showUserRole ? formattedRoleType : title)}
    </h1>
  );
};

export default FlippingTitle;