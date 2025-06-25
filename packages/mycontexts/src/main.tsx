// import React, { StrictMode } from 'react';
import React from 'react';
import ReactDOM from 'react-dom';
import * as ReactDOMClient from 'react-dom/client';
import App from './App.tsx';

// Add the axe-core integration in development mode only
if (import.meta.env.DEV) {
  import('@axe-core/react').then(axeModule => {
    const { default: axe } = axeModule;
    axe(React, ReactDOM, 1000, {
      rules: [
        // You can configure specific rules here
        { id: 'color-contrast', enabled: true },
      ]
    });
  });
}

const { createRoot } = ReactDOMClient;

createRoot(document.getElementById('root')!).render(
  // <StrictMode>
    <App />
  // </StrictMode>,
)
