import React from 'react'
import { render, screen, fireEvent, act } from '@testing-library/react'
import { ThemeProvider, createTheme } from '@mui/material/styles'
import Overlay, { OverlaySplit, overlayValueUrl } from '../Overlay'

const theme = createTheme()
const URL = 'https://example.com/article'

// The args of a compiled Show/Overlay: the address the page is written over, as a string.
function overlay(url = URL) {
  return { '/url': { tag: 'StrVal', contents: url } }
}

function Providers({ children }) {
  return <ThemeProvider theme={theme}>{children}</ThemeProvider>
}

// Gives the split a size to measure a drag against, since jsdom lays nothing out.
function sizeSplit(top = 0, height = 1000) {
  screen.getByTestId('overlay-split').getBoundingClientRect = () => ({ top, height, bottom: top + height, left: 0, right: 0, width: 800, x: 0, y: top })
}

describe('Overlay', () => {
  beforeEach(() => {
    global.fetch = vi.fn(() => Promise.reject(new Error('Overlay must not fetch')))
  })

  it('shows the address the page is written over, without fetching', () => {
    render(<Providers><Overlay {...overlay()} /></Providers>)

    const link = screen.getByRole('link', { name: URL })
    expect(link).toHaveAttribute('href', URL)
    expect(link).toHaveAttribute('target', '_blank')
    expect(global.fetch).not.toHaveBeenCalled()
  })

  // The contract the whole layout rests on: the value displays as a link, so the annotation showing
  // where it was written and the page laying itself out around it are one frame and one link.
  it('is never a frame of its own', () => {
    const { container } = render(<Providers><Overlay {...overlay()} /></Providers>)

    expect(container.querySelector('iframe')).toBeNull()
  })
})

describe('overlayValueUrl', () => {
  it('reads the address out of a compiled overlay', () => {
    expect(overlayValueUrl({ tag: 'TupleVal', name: '/Catln/Doc/Show/Overlay', args: overlay() })).toBe(URL)
  })

  it('reads nothing out of another value', () => {
    expect(overlayValueUrl({ tag: 'TupleVal', name: '/Catln/Doc/Show/MD', args: {} })).toBeNull()
    expect(overlayValueUrl({ tag: 'StrVal', contents: 'x' })).toBeNull()
    expect(overlayValueUrl(null)).toBeNull()
  })
})

describe('OverlaySplit', () => {
  beforeEach(() => {
    window.localStorage.clear()
  })

  it('shows the writing above and the page it is written over below', () => {
    const { container } = render(
      <Providers><OverlaySplit url={URL}><p>The notes</p></OverlaySplit></Providers>,
    )

    expect(screen.getByText('The notes')).toBeInTheDocument()
    const frames = container.querySelectorAll('iframe')
    expect(frames).toHaveLength(1)
    expect(frames[0]).toHaveAttribute('src', URL)
    expect(frames[0]).toHaveAttribute('title', `Overlay of ${URL}`)
  })

  // A refusal to be framed arrives in a header this page may not read, so it cannot be told from a
  // slow load. The way through is therefore offered always rather than once something is known.
  it('always offers the way through to the page itself', () => {
    render(<Providers><OverlaySplit url={URL}><p>The notes</p></OverlaySplit></Providers>)

    expect(screen.getByRole('link', { name: /Open in a new tab/ })).toHaveAttribute('href', URL)
    expect(screen.getByText(/refuses to be shown inside another page/)).toBeInTheDocument()
  })

  it('opens the halves even when nothing is stored', () => {
    render(<Providers><OverlaySplit url={URL}><p>The notes</p></OverlaySplit></Providers>)

    expect(screen.getByRole('separator')).toHaveAttribute('aria-valuenow', '50')
  })

  it('opens where the reader last left the divider', () => {
    window.localStorage.setItem('catln.webdocs.overlaySplit', '0.7')

    render(<Providers><OverlaySplit url={URL}><p>The notes</p></OverlaySplit></Providers>)

    expect(screen.getByRole('separator')).toHaveAttribute('aria-valuenow', '70')
  })

  it('opens the halves even when what is stored is not a ratio', () => {
    window.localStorage.setItem('catln.webdocs.overlaySplit', 'not a number')

    render(<Providers><OverlaySplit url={URL}><p>The notes</p></OverlaySplit></Providers>)

    expect(screen.getByRole('separator')).toHaveAttribute('aria-valuenow', '50')
  })

  it('moves with the arrow keys and remembers where it was put', () => {
    render(<Providers><OverlaySplit url={URL}><p>The notes</p></OverlaySplit></Providers>)
    const divider = screen.getByRole('separator')

    fireEvent.keyDown(divider, { key: 'ArrowUp' })

    expect(divider).toHaveAttribute('aria-valuenow', '45')
    expect(window.localStorage.getItem('catln.webdocs.overlaySplit')).toBe('0.45')
  })

  it('keeps the writing from being closed away entirely', () => {
    window.localStorage.setItem('catln.webdocs.overlaySplit', '0.16')

    render(<Providers><OverlaySplit url={URL}><p>The notes</p></OverlaySplit></Providers>)
    const divider = screen.getByRole('separator')

    fireEvent.keyDown(divider, { key: 'ArrowUp' })
    fireEvent.keyDown(divider, { key: 'ArrowUp' })

    expect(divider).toHaveAttribute('aria-valuenow', '15')
  })

  it('follows a drag and stops following it when it ends', () => {
    render(<Providers><OverlaySplit url={URL}><p>The notes</p></OverlaySplit></Providers>)
    sizeSplit()
    const divider = screen.getByRole('separator')

    fireEvent.mouseDown(divider)
    fireEvent.mouseMove(window, { clientY: 250 })

    expect(divider).toHaveAttribute('aria-valuenow', '25')

    fireEvent.mouseUp(window)
    fireEvent.mouseMove(window, { clientY: 800 })

    expect(divider).toHaveAttribute('aria-valuenow', '25')
  })

  // Found in a real browser, where a drag is delivered faster than a render: the pointer was
  // followed by an effect watching the dragging state, so a drag begun and moved before that state
  // was committed was a drag nothing was listening to and the divider did not move. Both events are
  // dispatched inside one act, which is what puts the move before the effects flush.
  it('follows a drag that moves before the render which knows it started', () => {
    render(<Providers><OverlaySplit url={URL}><p>The notes</p></OverlaySplit></Providers>)
    sizeSplit()
    const divider = screen.getByRole('separator')

    act(() => {
      divider.dispatchEvent(new MouseEvent('mousedown', { bubbles: true, cancelable: true }))
      window.dispatchEvent(new MouseEvent('mousemove', { bubbles: true, clientY: 250 }))
    })

    expect(divider).toHaveAttribute('aria-valuenow', '25')
  })

  // Without this the pointer crosses into the frame, which is another origin, and the move events
  // stop arriving -- leaving the divider stuck to the pointer.
  it('takes the panes out of the pointer\'s way while dragging', () => {
    const { container } = render(
      <Providers><OverlaySplit url={URL}><p>The notes</p></OverlaySplit></Providers>,
    )
    sizeSplit()
    const frame = container.querySelector('iframe').parentElement

    expect(frame).not.toHaveStyle({ pointerEvents: 'none' })

    fireEvent.mouseDown(screen.getByRole('separator'))

    expect(container.querySelector('iframe').parentElement).toHaveStyle({ pointerEvents: 'none' })

    fireEvent.mouseUp(window)

    expect(container.querySelector('iframe').parentElement).not.toHaveStyle({ pointerEvents: 'none' })
  })
})
