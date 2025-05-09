// Create a new file: src/Settings.tsx (or update your existing Settings component)
import React from 'react';
import { Tab, Tabs } from 'react-bootstrap';
import NotificationsSettings from './notificationsSettings';
import {i18next} from 'perspectives-react';
import { ModelSettings } from './modelSettings';

interface SettingsProps {
}

const Settings: React.FC<SettingsProps> = () => {
  return (
    <Tabs>
      <Tab eventKey="notifications" title={i18next.t('settings_notifications', { ns: 'mycontexts' })}>
        <NotificationsSettings />
      </Tab>
      <Tab eventKey="model" title={i18next.t('settings_model', { ns: 'mycontexts' })}>
        <ModelSettings/>
      </Tab>
    </Tabs>
  )
};

export default Settings;