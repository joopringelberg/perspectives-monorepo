import * as ReactDomClient from 'react-dom/client'
const { createRoot } = ReactDomClient 
import App from './App.tsx'

createRoot(document.getElementById('root')!).render(
  // <StrictMode>
    <App />
  // </StrictMode>,
)
