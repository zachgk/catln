import React from 'react'
import { render, screen, waitFor, act } from '@testing-library/react'
import userEvent from '@testing-library/user-event'
import { createMemoryRouter, RouterProvider, createRoutesFromElements, Route } from 'react-router-dom'
import { ThemeProvider, createTheme } from '@mui/material/styles'
import { Root } from '../App'
import DocsPage from '../DocsPage'
import sidebarResponse from '../test/fixtures/sidebar-response.json'
import pageResponse from '../test/fixtures/page-response.json'

const theme = createTheme()

// The one statement of an /api/annot response: the annotation, carrying its compiled value.
function annotResponse(annot, value) {
  const expr = {
    tag: 'Value',
    contents: [{ getMetaDat: [null, null, value], getMetaID: annot, getMetaPos: null, getMetaType: null }, `/Catln/Doc/#${annot}`],
  }
  return { tag: 'Success', contents: [[[], [[{ tag: 'RawAnnot', contents: expr }, []]]], []] }
}

const sidebarValue = {
  tag: 'TupleVal',
  name: '/Catln/Doc/Show/Sidebar',
  args: { '/pages': { tag: 'StrVal', contents: JSON.stringify(sidebarResponse) } },
}

function renderRoot() {
  const router = createMemoryRouter(
    createRoutesFromElements(
      <Route path="/" element={<Root />}>
        <Route path="docs/:prgmName" element={<div>page body</div>} />
      </Route>
    ),
    { initialEntries: ['/docs/%2Ftest%2Fid.ct'] }
  )
  return render(<ThemeProvider theme={theme}><RouterProvider router={router} /></ThemeProvider>)
}

// The page the doc server opens at. The sidebar and the search box are compiled in it, so it has to
// be there before either is asked for.
const ROOT_PAGE = '/test/id.ct'

describe('Root', () => {
  beforeEach(() => {
    global.fetch = vi.fn(url => Promise.resolve({
      status: 200,
      json: () => Promise.resolve(annotFor(url) || { tag: 'Success', contents: [{ readonly: false, rootPage: ROOT_PAGE }, []] }),
    }))
  })

  function annotFor(url) {
    if (url.startsWith('/api/annot?annot=%23sidebar')) return annotResponse('sidebar', sidebarValue)
  }

  // The sidebar is about which pages there are rather than about one of them, and is answered that
  // way whatever page it is written in. So it names one -- the page the docs open at -- and is never
  // asked for without a page, which is what asks for it to be compiled against every page there is.
  //
  // And it is the only annotation the layout asks for: the navbar's search box is a box until it is
  // typed into, so drawing the navbar no longer compiles a listing of every page to fill one.
  //
  // The box does ask for one thing before being typed into -- the annotations that are pages of their
  // own -- because that is the section a reader cannot find by typing: they would have to already
  // know what to type. It is a handful of entries rather than the doc server's listing, which is what
  // the search itself still waits for a query to avoid sending.
  it('asks for the sidebar within the page the docs open at, and for nothing else', async () => {
    renderRoot()

    await waitFor(() => expect(screen.getByText('test')).toBeInTheDocument())
    const annotCalls = global.fetch.mock.calls.map(c => c[0]).filter(u => u.startsWith('/api/annot'))
    expect(annotCalls).toEqual([`/api/annot?annot=%23sidebar&prgmName=${encodeURIComponent(ROOT_PAGE)}`])

    const callUrls = global.fetch.mock.calls.map(c => c[0]).filter(u => u.startsWith('/api/call'))
    const args = encodeURIComponent(JSON.stringify({ query: '' }))
    expect(callUrls).toEqual([
      `/api/call?fun=${encodeURIComponent('/Catln/Doc/docPages')}&args=${args}&prgmName=${encodeURIComponent(ROOT_PAGE)}`,
    ])
  })

  // The page is not there until the config says what it is, and asking without one is exactly what
  // this exists to stop, so nothing is asked for before then.
  it('asks for nothing before it knows the page the docs open at', async () => {
    global.fetch = vi.fn(url => new Promise(resolve => {
      if (url === '/api/config') return
      resolve({ status: 200, json: () => Promise.resolve(annotFor(url)) })
    }))
    renderRoot()

    await waitFor(() => expect(global.fetch).toHaveBeenCalledWith('/api/config'))
    expect(global.fetch.mock.calls.map(c => c[0]).filter(u => u.startsWith('/api/annot'))).toEqual([])
  })

  it('gives back all of its width when the navbar control contracts it', async () => {
    const user = userEvent.setup()
    renderRoot()

    await waitFor(() => expect(screen.getByText('test')).toBeInTheDocument())
    expect(screen.getByTestId('sidebar')).toHaveStyle({ width: '300px' })

    await user.click(screen.getByLabelText('Hide sidebar'))
    expect(screen.getByTestId('sidebar')).toHaveStyle({ width: '0px' })

    // Contracted rather than unmounted, so showing it again is not a recompile of its annotation.
    expect(screen.getByText('test')).toBeInTheDocument()
  })

  // The sidebar and the window beside it are separate questions the server holds open, which is what
  // makes them independent: a change reaching one is a change the server never sends to the other.
  // A doc server which said what had changed and let the browser sort it out would have to be right
  // about this in two places; here it can only be right or wrong in one.
  it('follows the navigation and the page being read as two separate questions', async () => {
    global.fetch = vi.fn(url => Promise.resolve({
      status: 200,
      json: () => Promise.resolve(url.startsWith('/api/page') ? pageResponse
        : annotFor(url) || { tag: 'Success', contents: [{ readonly: false, rootPage: ROOT_PAGE }, []] }),
    }))
    const router = createMemoryRouter(
      createRoutesFromElements(
        <Route path="/" element={<Root />}>
          <Route path="docs/:prgmName" element={<DocsPage prgmName="/test/id.ct" />} />
        </Route>
      ),
      { initialEntries: ['/docs/%2Ftest%2Fid.ct'] }
    )
    render(<ThemeProvider theme={theme}><RouterProvider router={router} /></ThemeProvider>)

    await waitFor(() => expect(screen.getByText('test')).toBeInTheDocument())
    await waitFor(() => expect(eventSourceFor('/api/page')).toBeTruthy())

    const nav = eventSourceFor('/api/annot?annot=%23sidebar')
    const page = eventSourceFor('/api/page')
    expect(nav).toBeTruthy()
    expect(nav).not.toBe(page)

    // A page appearing is a change to which pages there are, so the navigation is answered again.
    // The window beside it is compiled from one page's program, which the new page is nothing to, so
    // the server sends it nothing and it is not even re-rendered from a stale answer.
    const grown = [...sidebarResponse, { tag: 'SidebarFile', sbName: 'new.ct', sbPath: '/new.ct', sbDir: '/' }]
    act(() => nav.frame(annotResponse('sidebar', {
      tag: 'TupleVal',
      name: '/Catln/Doc/Show/Sidebar',
      args: { '/pages': { tag: 'StrVal', contents: JSON.stringify(grown) } },
    })))
    await waitFor(() => expect(screen.getByText('new.ct')).toBeInTheDocument())
    expect(eventSourceFor('/api/page')).toBe(page)
    expect(page.readyState).toBe(1)

    // And the other way round: the page being written to is answered again, and the navigation is
    // left exactly as the reader had it.
    act(() => page.frame(pageResponse))
    expect(screen.getByText('new.ct')).toBeInTheDocument()
    expect(eventSourceFor('/api/annot?annot=%23sidebar')).toBe(nav)
  })

  it('opens as it was left', async () => {
    const user = userEvent.setup()
    const { unmount } = renderRoot()

    await waitFor(() => expect(screen.getByText('test')).toBeInTheDocument())
    await user.click(screen.getByLabelText('Hide sidebar'))
    unmount()

    renderRoot()
    expect(screen.getByTestId('sidebar')).toHaveStyle({ width: '0px' })
    expect(screen.getByLabelText('Show sidebar')).toBeInTheDocument()
  })
})
