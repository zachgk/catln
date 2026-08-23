import React from 'react'
import { fireEvent, render, screen } from '@testing-library/react'
import userEvent from '@testing-library/user-event'
import { ThemeProvider, createTheme } from '@mui/material/styles'
import { MemoryRouter, Route, Routes, useParams } from 'react-router-dom'
import PrgmGraph, { visiblePages, layout } from '../PrgmGraph'

const theme = createTheme()

// jsdom lays nothing out: every element measures zero and there is no ResizeObserver to report a
// size changing. The graph asks how much room it has before it decides what is on screen, and reads
// those as a pane that does not exist, so it draws nothing at all. Given a pane of a fixed size and
// an observer that reports it once, its pages reach the DOM for a case below to read.
//
// Here rather than in the suites' shared setup, because this is the one display that measures
// anything and giving every element in every suite a size it does not have is a change to what those
// suites are run against. A second display that measures is what would move it.
//
// The size is arbitrary and nothing below asserts on it: what is asked is which pages are drawn, not
// where. DOMMatrixReadOnly is what the pan and zoom transform is read back through, and jsdom has
// none.
class FakeResizeObserver {
  constructor(callback) { this.callback = callback }
  observe() { this.callback([], this) }
  unobserve() {}
  disconnect() {}
}

beforeEach(() => {
  global.ResizeObserver = FakeResizeObserver
  global.DOMMatrixReadOnly = class {
    constructor() {
      this.m22 = 1
      this.m41 = 0
      this.m42 = 0
    }
  }
  Object.defineProperties(window.HTMLElement.prototype, {
    offsetHeight: {configurable: true, get() { return parseFloat(this.style.height) || 800 }},
    offsetWidth: {configurable: true, get() { return parseFloat(this.style.width) || 800 }},
  })
  window.SVGElement.prototype.getBBox = () => ({x: 0, y: 0, width: 0, height: 0})
})

const PAGE = '/vault/Notes.md'
const NOTE = '/vault/Tasks.md'
const CORE = '/stack/core/main.ct'
const CORE_STR = '/stack/core/str.ct'

// One page of the graph, as the compiler renders it: the path it is opened by, the name it is drawn
// under, the pages it imports, and whether it is the standard library's and whether it is the root.
function page(path, imports, {core = false, root = false} = {}) {
  return {
    pgnPath: path,
    pgnName: path.split('/').pop(),
    pgnImports: imports,
    pgnCore: core,
    pgnRoot: root,
  }
}

// The graph the fixtures below are drawn from: a page importing a note beside it and the standard
// library, which imports one more of its own.
const PAGES = [
  page(PAGE, [NOTE, CORE], {root: true}),
  page(NOTE, [CORE]),
  page(CORE, [CORE_STR], {core: true}),
  page(CORE_STR, [], {core: true}),
]

// A compiled /Catln/Doc/Show/PrgmGraph, which carries the whole graph in its one field.
function graphed(pages = PAGES) {
  return { '/graph': { tag: 'StrVal', contents: JSON.stringify(pages) } }
}

// Where a double click took the reader, read off the route it landed on.
function Opened() {
  return <div data-testid="opened">{useParams().prgmName}</div>
}

function Providers({ children }) {
  return (
    <ThemeProvider theme={theme}>
      <MemoryRouter>
        <Routes>
          <Route path="/" element={children} />
          <Route path="/docs/:prgmName" element={<Opened />} />
        </Routes>
      </MemoryRouter>
    </ThemeProvider>
  )
}

// The library toggle, which is the one control the graph has.
function libraryToggle() {
  return screen.getByRole('checkbox', { name: 'Standard library' })
}

// The box a page is drawn in, which is where the classes saying how it is drawn are. Reached from
// its label, since the label is what a case below has in hand.
function boxOf(name) {
  return screen.getByText(name).closest('.react-flow__node')
}

describe('PrgmGraph', () => {
  beforeEach(() => {
    global.fetch = vi.fn(() => Promise.reject(new Error('PrgmGraph must not fetch')))
  })

  // The whole graph is the compiled value, so drawing it asks the compiler nothing.
  it('draws the program from the compiled value alone', () => {
    render(<Providers><PrgmGraph {...graphed()} /></Providers>)

    expect(screen.getByText('Notes.md')).toBeInTheDocument()
    expect(screen.getByText('Tasks.md')).toBeInTheDocument()
    expect(global.fetch).not.toHaveBeenCalled()
  })

  // A program of two pages resting on twenty of the library reads as a graph of the library with
  // something attached, so the library is left out until it is asked for.
  it('leaves the standard library out until it is asked for', async () => {
    render(<Providers><PrgmGraph {...graphed()} /></Providers>)

    expect(screen.queryByText('main.ct')).not.toBeInTheDocument()

    await userEvent.click(libraryToggle())

    expect(screen.getByText('main.ct')).toBeInTheDocument()
    expect(screen.getByText('str.ct')).toBeInTheDocument()
  })

  // Turning the graph over is a reader turning over what they were already handed, not a second
  // question, so nothing is asked again either way round.
  it('asks nothing again to show the library and hide it', async () => {
    render(<Providers><PrgmGraph {...graphed()} /></Providers>)

    await userEvent.click(libraryToggle())
    await userEvent.click(libraryToggle())

    expect(screen.queryByText('main.ct')).not.toBeInTheDocument()
    expect(global.fetch).not.toHaveBeenCalled()
  })

  // A page is labelled by its file name, which is all a box that size holds, so the path it is at is
  // what says which of two same-named pages it is.
  it('labels a page by its name and carries the path it is at', () => {
    render(<Providers><PrgmGraph {...graphed()} /></Providers>)

    expect(screen.getByText('Notes.md')).toHaveAttribute('title', PAGE)
  })

  // Dispatched rather than acted out, here and below. The graph's pan and zoom is d3's, which reads
  // the window off the mouse event it was given and throws on the one a simulated press builds. What
  // is being asked is what the graph does with a click on a page, so the click is the event and not
  // the press and release around it.
  it('opens the page a reader double clicks', () => {
    render(<Providers><PrgmGraph {...graphed()} /></Providers>)

    fireEvent.doubleClick(screen.getByText('Tasks.md'))

    expect(screen.getByTestId('opened')).toHaveTextContent(NOTE)
  })

  // A click is what picks a page, and only a second one opens it: a reader looking at what a page is
  // joined to is not taken away from the graph to look.
  it('stays on the graph when a page is picked', () => {
    render(<Providers><PrgmGraph {...graphed()} /></Providers>)

    fireEvent.click(screen.getByText('Tasks.md'))

    expect(screen.queryByTestId('opened')).not.toBeInTheDocument()
    expect(screen.getByText('Tasks.md')).toBeInTheDocument()
  })

  // A graph of thirty pages says nothing about any one of them until the rest is out of the way, so
  // picking one dims the pages it neither imports nor is imported by.
  it('dims the pages the picked one is not joined to', async () => {
    render(<Providers><PrgmGraph {...graphed()} /></Providers>)

    await userEvent.click(libraryToggle())
    fireEvent.click(screen.getByText('Notes.md'))

    // What it imports, and what imports it, stay lit.
    expect(boxOf('Notes.md').className).not.toMatch(/dimmed/)
    expect(boxOf('Tasks.md').className).not.toMatch(/dimmed/)
    expect(boxOf('main.ct').className).not.toMatch(/dimmed/)
    // Reached only through another page, so joined to this one by nothing.
    expect(boxOf('str.ct').className).toMatch(/dimmed/)
  })

  // A page picked and then hidden is nothing being picked. Dimming what a pick is not joined to,
  // with the pick itself gone, would dim every page there is and leave a reader looking at a graph
  // they cannot read and did not ask to change.
  it('keeps the graph readable when the page that was picked is hidden', async () => {
    render(<Providers><PrgmGraph {...graphed()} /></Providers>)

    await userEvent.click(libraryToggle())
    fireEvent.click(screen.getByText('str.ct'))
    await userEvent.click(libraryToggle())

    expect(boxOf('Notes.md').className).not.toMatch(/dimmed/)
    expect(boxOf('Tasks.md').className).not.toMatch(/dimmed/)
  })

  // A graph of a page of the library, with the library hidden. The root is kept whatever was asked,
  // since a graph of a program missing the page it is of is a graph of something else.
  it('keeps the page it was drawn for even when that page is the library', () => {
    const pages = [page(CORE, [CORE_STR], {core: true, root: true}), page(CORE_STR, [], {core: true})]
    render(<Providers><PrgmGraph {...graphed(pages)} /></Providers>)

    expect(screen.getByText('main.ct')).toBeInTheDocument()
    expect(screen.queryByText('str.ct')).not.toBeInTheDocument()
  })

  it('says so rather than drawing nothing when every page is the library', () => {
    const pages = [page(CORE, [CORE_STR], {core: true}), page(CORE_STR, [], {core: true})]
    render(<Providers><PrgmGraph {...graphed(pages)} /></Providers>)

    expect(screen.getByText(/part of the standard library/)).toBeInTheDocument()
    expect(screen.queryByTestId('prgm-graph')).not.toBeInTheDocument()
  })
})

describe('visiblePages', () => {
  it('keeps every page when the library is asked for', () => {
    expect(visiblePages(PAGES, true)).toHaveLength(PAGES.length)
  })

  it('keeps the pages that are not the library, and the root whatever it is', () => {
    expect(visiblePages(PAGES, false).map(p => p.pgnPath)).toEqual([PAGE, NOTE])
  })
})

describe('layout', () => {
  // An edge to a page that is not being drawn would be an arrow to nothing, so it is dropped with
  // the page rather than drawn.
  it('drops the edges to pages that are not drawn', () => {
    const { nodes, edges } = layout(visiblePages(PAGES, false))

    expect(nodes.map(n => n.id)).toEqual([PAGE, NOTE])
    expect(edges.map(e => e.id)).toEqual([`${PAGE}->${NOTE}`])
  })

  // A page sits above what it imports, which is the direction the arrows read in and the direction a
  // reader coming from a page's import list expects to find what it named.
  it('lays a page above what it imports', () => {
    const { nodes } = layout(PAGES)
    const at = id => nodes.find(n => n.id === id).position.y

    expect(at(PAGE)).toBeLessThan(at(NOTE))
    expect(at(NOTE)).toBeLessThan(at(CORE))
    expect(at(CORE)).toBeLessThan(at(CORE_STR))
  })
})
