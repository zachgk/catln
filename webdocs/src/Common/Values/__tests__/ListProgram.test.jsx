import React from 'react'
import { render, screen, fireEvent } from '@testing-library/react'
import { MemoryRouter } from 'react-router-dom'
import { ThemeProvider, createTheme } from '@mui/material/styles'
import ListProgram from '../ListProgram'
import typecheckResponse from '../../../test/fixtures/typecheck-response.json'

const theme = createTheme()

function Providers({ children }) {
  return (
    <ThemeProvider theme={theme}>
      <MemoryRouter>{children}</MemoryRouter>
    </ThemeProvider>
  )
}

// The args of a compiled Show/ListProgram: the mode the annotation named, and the program the
// compiler listed at that mode as a JSON string.
function listing(modeName) {
  return {
    '/mode': { tag: 'TupleVal', name: modeName, args: {} },
    '/prgm': { tag: 'StrVal', contents: JSON.stringify(typecheckResponse.contents[0]) },
  }
}

describe('ListProgram', () => {
  beforeEach(() => {
    global.fetch = vi.fn(() => Promise.reject(new Error('ListProgram must not fetch')))
  })

  it.each([
    ['/Catln/Doc/Desugar', 'No Typecheck'],
    ['/Catln/Doc/Typecheck', 'List'],
    ['/Catln/Doc/TreeBuild', 'TreeBuild'],
  ])('renders the program carried for %s without fetching', (modeName, label) => {
    render(<Providers><ListProgram {...listing(modeName)} update={vi.fn()} /></Providers>)

    expect(screen.getByLabelText(label)).toBeChecked()
    expect(screen.getByText('Types')).toBeInTheDocument()
    expect(global.fetch).not.toHaveBeenCalled()
  })

  it('toggling a radio asks for a recompile instead of moving local state', () => {
    const update = vi.fn()
    render(<Providers><ListProgram {...listing('/Catln/Doc/Typecheck')} update={update} /></Providers>)

    fireEvent.click(screen.getByLabelText('TreeBuild'))

    expect(update).toHaveBeenCalledWith('#listProgram(mode= /Catln/Doc/TreeBuild)')
    // The mode comes from the annotation, so nothing moves until the recompile arrives.
    expect(screen.getByLabelText('List')).toBeChecked()
  })

  it('disables the radios when there is no update to make', () => {
    render(<Providers><ListProgram {...listing('/Catln/Doc/Typecheck')} /></Providers>)

    // Still shows which mode the source named, but the source cannot be rewritten from here.
    expect(screen.getByLabelText('List')).toBeChecked()
    for (const label of ['No Typecheck', 'List', 'TreeBuild']) {
      expect(screen.getByLabelText(label)).toBeDisabled()
    }
  })
})
