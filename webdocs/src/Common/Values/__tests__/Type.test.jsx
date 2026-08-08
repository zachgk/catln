import React from 'react'
import { render, screen } from '@testing-library/react'
import { MemoryRouter } from 'react-router-dom'
import { ThemeProvider, createTheme } from '@mui/material/styles'
import TypePage from '../Type'
import typecheckResponse from '../../../test/fixtures/typecheck-response.json'

const theme = createTheme()

function Providers({ children }) {
  return (
    <ThemeProvider theme={theme}>
      <MemoryRouter>{children}</MemoryRouter>
    </ThemeProvider>
  )
}

// The args of a compiled Show/TypePage: the name the annotation filtered by, and the listing the
// compiler produced for that name as a JSON string.
function typePage(name) {
  return {
    '/name': { tag: 'StrVal', contents: name },
    '/prgm': { tag: 'StrVal', contents: JSON.stringify(typecheckResponse.contents[0]) },
  }
}

describe('TypePage', () => {
  beforeEach(() => {
    global.fetch = vi.fn(() => Promise.reject(new Error('TypePage must not fetch')))
  })

  it('renders the listing carried by the value without fetching', () => {
    render(<Providers><TypePage {...typePage('/Integer')} /></Providers>)

    expect(screen.getByRole('heading', { name: '/Integer' })).toBeInTheDocument()
    expect(screen.getByText('Types')).toBeInTheDocument()
    expect(global.fetch).not.toHaveBeenCalled()
  })

  it('titles itself from the name the annotation was filtered by', () => {
    render(<Providers><TypePage {...typePage('/Catln/Doc/DShow')} /></Providers>)

    expect(screen.getByRole('heading', { name: '/Catln/Doc/DShow' })).toBeInTheDocument()
  })
})
