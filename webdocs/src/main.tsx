import './index.css'
import { StrictMode } from 'react'
import { createRoot } from 'react-dom/client'

import './monacoSetup'
// @ts-expect-error App is untyped JSX, which this TS project does not allow importing
import Root from './App.jsx'

createRoot(document.getElementById('root')!).render(
  <StrictMode>
    <Root />
  </StrictMode>,
)
