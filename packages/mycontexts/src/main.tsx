import * as React from 'react'
const { StrictMode } = React;
import * as ReactDOM from 'react-dom/client'
const { createRoot } = ReactDOM;
import App from './App.tsx'

createRoot(document.getElementById('root')!).render(
  // <StrictMode>
    <App />
  // </StrictMode>,
)
