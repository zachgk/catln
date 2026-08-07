import './index.css'
import { StrictMode } from 'react'
import { createRoot } from 'react-dom/client'

import './monacoSetup'
// @ts-ignore
import Root from './App.jsx'

createRoot(document.getElementById('root')!).render(
  <StrictMode>
    <Root />
  </StrictMode>,
)
