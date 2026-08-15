import React from 'react'
import { render, screen } from '@testing-library/react'
import { MemoryRouter } from 'react-router-dom'
import { ThemeProvider, createTheme } from '@mui/material/styles'
import Debugger from '../Debugger'
import debuggerResponse from '../../../test/fixtures/debugger-response.json'

const theme = createTheme()

// The real payload the compiler produced for a page whose main prints "Hello, world" and exits 0,
// narrowed to the fields the page reads: erCoverage is real but unread, and was half the bytes.
const FUN = '/main'

// The args of a compiled Show/Debugger: the run, as a JSON string, and the function it ran.
function debug(run = debuggerResponse) {
  return {
    '/run': { tag: 'StrVal', contents: JSON.stringify(run) },
    '/fun': { tag: 'StrVal', contents: FUN },
  }
}

function Providers({ children }) {
  return (
    <ThemeProvider theme={theme}>
      <MemoryRouter>{children}</MemoryRouter>
    </ThemeProvider>
  )
}

describe('Debugger', () => {
  beforeEach(() => {
    global.fetch = vi.fn(() => Promise.reject(new Error('Debugger must not fetch')))
  })

  it('renders the run carried by the value without fetching', () => {
    render(<Providers><Debugger {...debug()} /></Providers>)

    expect(screen.getByRole('heading', { name: FUN })).toBeInTheDocument()
    // The root of the evaluation tree, which is what the page exists to show.
    expect(screen.getByRole('treeitem')).toBeInTheDocument()
    expect(global.fetch).not.toHaveBeenCalled()
  })

  it('renders the mock IO handle the run threads as ordinary IO', () => {
    render(<Providers><Debugger {...debug()} /></Providers>)

    // Running under a mock is how the output above was captured, so every /io in the tree is a
    // MockIOVal. Left unhandled it renders as "Unknown Val tag" and logs an error.
    expect(screen.queryByText(/Unknown Val tag/)).not.toBeInTheDocument()
    expect(screen.getAllByText(/IOVal/).length).toBeGreaterThan(0)
  })

  it('shows what the run printed and exited with', () => {
    render(<Providers><Debugger {...debug()} /></Providers>)

    // Captured through the mock IO handle rather than written to the doc server's stdout, which is
    // the only reason there is anything here to show.
    expect(screen.getByText('Hello, world')).toBeInTheDocument()
    expect(screen.getByText(/Exited with 0/)).toBeInTheDocument()
  })

  it('says so when the run printed nothing and entered no arrow', () => {
    const empty = { drExit: 1, drOutput: '', drValue: null, drResult: { erCoverage: {}, erTreebug: [] } }
    render(<Providers><Debugger {...debug(empty)} /></Providers>)

    expect(screen.getByText(/Evaluated no arrows/)).toBeInTheDocument()
    expect(screen.getByText(/Printed nothing/)).toBeInTheDocument()
    expect(screen.getByText(/Exited with 1/)).toBeInTheDocument()
    expect(screen.queryByRole('treeitem')).not.toBeInTheDocument()
  })

  // A run of one expression (#debugExpr) rather than of a program: it was given no IO handle to
  // print with, so what it ended with is a value and there is no exit code to report.
  it('shows the value an expression run produced instead of an exit code', () => {
    const expr = {
      drExit: null,
      drOutput: '',
      drValue: { tag: 'IntVal', contents: 3 },
      drResult: { erCoverage: {}, erTreebug: [] },
    }
    render(<Providers><Debugger {...debug(expr)} /></Providers>)

    expect(screen.getByRole('heading', { name: 'Value' })).toBeInTheDocument()
    expect(screen.getByText('3')).toBeInTheDocument()
    expect(screen.queryByText(/Exited with/)).not.toBeInTheDocument()
    expect(screen.queryByText(/Printed nothing/)).not.toBeInTheDocument()
  })
})
