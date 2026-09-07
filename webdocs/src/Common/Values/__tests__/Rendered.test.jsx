import React from 'react'
import { render, screen, waitFor } from '@testing-library/react'
import { ThemeProvider, createTheme } from '@mui/material/styles'

import Rendered from '../Rendered'
import { loadAssets } from '../assets'

// The loader is mocked rather than exercised: it ends in an import of a URL built at runtime, which
// jsdom has no way to resolve and which vi.mock cannot intercept by specifier. Keeping that one line
// in a module of its own is what makes this component testable at all.
vi.mock('../assets', () => ({
  assetUrl: path => `/api/asset?path=${path}`,
  loadAssets: vi.fn()
}))

const theme = createTheme()

function Providers({ children }) {
  return <ThemeProvider theme={theme}>{children}</ThemeProvider>
}

const nil = { tag: 'TupleVal', name: '/Data/List/Nil', args: {} }
const cons = (head, tail) => ({ tag: 'TupleVal', name: '/Data/List/Cons', args: { '/head': head, '/tail': tail } })
const strs = ss => ss.reduceRight((t, s) => cons({ tag: 'StrVal', contents: s }, t), nil)

// The args of a compiled /Catln/Doc/Show/Rendered: the value, and the paths of the files that draw
// it. The paths arrive as a /Data/List, which is a cons chain and not an array.
function shown(value, assets) {
  return { '/value': value, '/assets': strs(assets) }
}

const aValue = { tag: 'TupleVal', name: '/Test/Thing', args: { '/name': { tag: 'StrVal', contents: 'x' } } }

describe('Rendered', () => {
  beforeEach(() => {
    loadAssets.mockReset()
  })

  it('asks for the files the show named, in the order it named them', async () => {
    loadAssets.mockResolvedValue([{ render: vi.fn() }, null])

    render(<Providers><Rendered {...shown(aValue, ['thing/thing.js', 'thing/thing.css'])} /></Providers>)

    await waitFor(() => expect(loadAssets).toHaveBeenCalledWith(['thing/thing.js', 'thing/thing.css']))
  })

  // The value itself, not the spread of its arguments every other display takes. A library's script
  // walks its own union and reads the constructor name at each step.
  it('hands the whole value to the script that renders', async () => {
    const draw = vi.fn()
    loadAssets.mockResolvedValue([{ render: draw }])

    render(<Providers><Rendered {...shown(aValue, ['thing/thing.js'])} /></Providers>)

    await waitFor(() => expect(draw).toHaveBeenCalledTimes(1))
    expect(draw.mock.calls[0][1]).toBe(aValue)
  })

  // React must keep out of the node the script draws into, or a re-render of the page around it
  // would discard whatever the script put there and whatever state the reader had built up in it.
  it('draws into a node React leaves empty', async () => {
    loadAssets.mockResolvedValue([{
      render: root => { root.appendChild(document.createTextNode('drawn by the library')) }
    }])

    const { container, rerender } = render(<Providers><Rendered {...shown(aValue, ['thing/thing.js'])} /></Providers>)

    await waitFor(() => expect(container.textContent).toContain('drawn by the library'))

    rerender(<Providers><Rendered {...shown(aValue, ['thing/thing.js'])} /></Providers>)
    expect(container.textContent).toContain('drawn by the library')
  })

  it('runs what the script returned when the value goes away', async () => {
    const teardown = vi.fn()
    loadAssets.mockResolvedValue([{ render: () => teardown }])

    const { unmount } = render(<Providers><Rendered {...shown(aValue, ['thing/thing.js'])} /></Providers>)

    await waitFor(() => expect(loadAssets).toHaveBeenCalled())
    unmount()
    expect(teardown).toHaveBeenCalledTimes(1)
  })

  // Said rather than left blank, both ways round: a reader shown an empty row would read it as a
  // value holding nothing rather than as a display that never arrived.
  it('says so when nothing the show named can render', async () => {
    loadAssets.mockResolvedValue([null])

    render(<Providers><Rendered {...shown(aValue, ['thing/thing.css'])} /></Providers>)

    expect(await screen.findByText(/thing\/thing\.css/)).toBeInTheDocument()
  })

  it('says so when the files could not be loaded', async () => {
    loadAssets.mockRejectedValue(new Error('no such asset'))

    render(<Providers><Rendered {...shown(aValue, ['thing/thing.js'])} /></Providers>)

    expect(await screen.findByText(/no such asset/)).toBeInTheDocument()
  })
})
