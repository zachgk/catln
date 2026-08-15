import React from 'react'
import { render, screen, fireEvent } from '@testing-library/react'
import { MemoryRouter, useLocation } from 'react-router-dom'
import { ThemeProvider, createTheme } from '@mui/material/styles'
import DebuggerMenu from '../DebuggerMenu'
import { PrgmNameContext } from '../Common'

const theme = createTheme()
const PAGE = '/stack/core/algebra.ct'

function LocationDisplay() {
  const location = useLocation()
  return <div data-testid="location">{location.pathname + location.search}</div>
}

function Providers({ children, prgmName = PAGE }) {
  return (
    <MemoryRouter>
      <ThemeProvider theme={theme}>
        <PrgmNameContext.Provider value={prgmName}>{children}</PrgmNameContext.Provider>
      </ThemeProvider>
    </MemoryRouter>
  )
}

function menuOn(expr, prgmName) {
  render(
    <Providers prgmName={prgmName}>
      <DebuggerMenu expr={expr}><code>shown</code></DebuggerMenu>
      <LocationDisplay />
    </Providers>
  )
  return screen.getByText('shown')
}

describe('DebuggerMenu', () => {
  it('offers a debugger of the expression on right-click', () => {
    fireEvent.contextMenu(menuOn('1 + 2'))

    expect(screen.getByText('Debugger')).toBeInTheDocument()
  })

  it('leaves what it wraps on screen', () => {
    expect(menuOn('1 + 2')).toBeInTheDocument()
    expect(screen.queryByText('Debugger')).not.toBeInTheDocument()
  })

  it('debugs the expression as an annotation scoped to the page', () => {
    fireEvent.contextMenu(menuOn('1 + 2'))
    fireEvent.click(screen.getByText('Debugger'))

    expect(decodeURIComponent(screen.getByTestId('location').textContent))
      .toBe(`/annot?annot=#debugExpr(expr= "1 + 2")&prgmName=${PAGE}`)
  })

  // A generated test case is rendered with its String arguments quoted, so the expression the menu
  // is given already contains quotes. They have to reach the compiler as quotes of the expression
  // rather than closing the one the annotation wraps it in.
  it('quotes an expression that itself contains quotes', () => {
    fireEvent.contextMenu(menuOn('f(a= "", b= "x")'))
    fireEvent.click(screen.getByText('Debugger'))

    expect(decodeURIComponent(screen.getByTestId('location').textContent))
      .toBe(`/annot?annot=#debugExpr(expr= "f(a= \\"\\", b= \\"x\\")")&prgmName=${PAGE}`)
  })

  // An annotation that is not a print has no computation behind it to run, and the page sends no
  // source for one. Offering a menu item that could only fail would be worse than offering none.
  it('offers nothing when there is no expression to debug', () => {
    fireEvent.contextMenu(menuOn(null))

    expect(screen.getByText('shown')).toBeInTheDocument()
    expect(screen.queryByText('Debugger')).not.toBeInTheDocument()
  })
})
