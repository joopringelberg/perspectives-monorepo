// Create a new file: src/Settings.tsx (or update your existing Settings component)
import React from 'react';
import { Container, Row, Col, Nav, Tab, Tabs } from 'react-bootstrap';
import NotificationsSettings from './notificationsSettings';
import i18next from 'i18next';

interface SettingsProps {
}

const Settings: React.FC<SettingsProps> = () => {
  return (
    <Tabs>
      <Tab eventKey="notifications" title={i18next.t('settings_notifications', { ns: 'mycontexts' })}>
        <NotificationsSettings />
      </Tab>
    </Tabs>
  )
};

export default Settings;