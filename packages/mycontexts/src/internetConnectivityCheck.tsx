import * as React from 'react';
import { OverlayInjectedProps } from 'react-bootstrap/esm/Overlay';
import { OverlayTrigger, Tooltip } from 'react-bootstrap';
import { i18next } from 'perspectives-react';

export function InternetConnectivityCheck({ reportBack }: { reportBack: (isOnline: boolean) => void }) {
  // Don't destructure React hooks
  const [isOnline, setIsOnline] = React.useState(false);
  
  // Check connectivity by making a lightweight request
  const checkConnectivity = () => {
    fetch('https://www.google.com/favicon.ico', { 
      mode: 'no-cors',
      cache: 'no-store'
    })
    .then(() => registerOnlineStatusChange(true))
    .catch(() => registerOnlineStatusChange(false));
  };

  const registerOnlineStatusChange = (currentStatus: boolean) => {
    if (currentStatus !== isOnline) {
      console.log("Connectivity status changed: ", currentStatus);
      setIsOnline(currentStatus);
      reportBack(currentStatus);
    }
  };
  
  React.useEffect(() => {
    // Check immediately
    console.log("Checking connectivity for the first time");
    checkConnectivity();
    
    // Regular interval checking
    const interval = setInterval(() => {
      checkConnectivity();
    }, 10000);
    
    // Also use the browser events
    const handleOnline = () => {
      console.log("Browser online event");
      checkConnectivity();
    };
    
    const handleOffline = () => {
      console.log("Browser offline event");
      registerOnlineStatusChange(false);
    };
    
    window.addEventListener('online', handleOnline);
    window.addEventListener('offline', handleOffline);
    
    return () => {
      clearInterval(interval);
      window.removeEventListener('online', handleOnline);
      window.removeEventListener('offline', handleOffline);
    };
  }, []);
  
  const renderTooltip = (props: OverlayInjectedProps) => (
    <Tooltip id="amqp-tooltip" {...props} show={props.show}>
      {i18next.t("app_connected_to_internet_tooltip", { 
        ns: 'mycontexts',
        connected: isOnline,
        connectionState: isOnline ? 
          "" :
          i18next.t("app_connected_to_internet_connected", { ns: 'mycontexts' })
      })}
    </Tooltip>
  );

  return <div className='ms-auto'/>;
  return (
    <OverlayTrigger
      placement="left"
      delay={{ show: 250, hide: 400 }}
      overlay={renderTooltip}
    >
      <div className="ms-auto">
        {isOnline ? 
          <span className="text-success pe-2"><i className="bi bi-globe"></i></span> : 
          <span className="text-danger pe-2"><i className="bi bi-globe-americas"></i></span>}
      </div>
    </OverlayTrigger>
  );
}