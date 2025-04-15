// Create a new file: src/NotificationsSettings.tsx
import React, { useEffect, useState } from 'react';
import { Form, Button, Alert, Card } from 'react-bootstrap';
import i18next from 'i18next';

interface NotificationsSettingsProps {
}

const NotificationsSettings: React.FC<NotificationsSettingsProps> = () => {
  const [permissionState, setPermissionState] = useState<NotificationPermission>('default');
  const [supported, setSupported] = useState(true);

  useEffect(() => {
    // Check if notifications are supported
    if (!('Notification' in window)) {
      setSupported(false);
      return;
    }
    
    // Get current permission status
    setPermissionState(Notification.permission);
  }, []);

  const requestPermission = async () => {
    try {
      const permission = await Notification.requestPermission();
      setPermissionState(permission);
    } catch (err) {
      console.error('Error requesting notification permission:', err);
    }
  };

  if (!supported) {
    return (
      <Alert variant="warning">
        {i18next.t('notifications_not_supported', { ns: 'mycontexts' })}
      </Alert>
    );
  }

  return (
    <Card className="mb-4">
      <Card.Header>{i18next.t('notifications_settings_title', { ns: 'mycontexts' })}</Card.Header>
      <Card.Body>
        <div className="mb-3">
          <strong>{i18next.t('notifications_current_status', { ns: 'mycontexts' })}: </strong>
          {permissionState === 'granted' ? (
            <span className="text-success">{i18next.t('notifications_status_granted', { ns: 'mycontexts' })}</span>
          ) : permissionState === 'denied' ? (
            <span className="text-danger">{i18next.t('notifications_status_denied', { ns: 'mycontexts' })}</span>
          ) : (
            <span className="text-secondary">{i18next.t('notifications_status_default', { ns: 'mycontexts' })}</span>
          )}
        </div>

        {permissionState !== 'granted' && (
          <div>
            <p>{i18next.t('notifications_permission_explanation', { ns: 'mycontexts' })}</p>
            {permissionState === 'denied' ? (
              <Alert variant="info">
                {i18next.t('notifications_permission_denied_help', { ns: 'mycontexts' })}
              </Alert>
            ) : (
              <Button onClick={requestPermission} variant="primary">
                {i18next.t('notifications_request_permission', { ns: 'mycontexts' })}
              </Button>
            )}
          </div>
        )}
      </Card.Body>
    </Card>
  );
};

export default NotificationsSettings;