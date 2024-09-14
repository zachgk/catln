import './index.css'
import { StrictMode } from 'react'
import { createRoot } from 'react-dom/client'

// @ts-ignore
import Root from './App.jsx'

createRoot(document.getElementById('root')!).render(
  <StrictMode>
    <Root />
  </StrictMode>,
)
