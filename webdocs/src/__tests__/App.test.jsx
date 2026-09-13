import React from 'react'
import { render, screen, waitFor, act } from '@testing-library/react'
import userEvent from '@testing-library/user-event'
import { createMemoryRouter, RouterProvider, createRoutesFromElements, Route } from 'react-router-dom'
import { ThemeProvider, createTheme } from '@mui/material/styles'
import { Root } from '../App'
import Docs from '../Docs'
import DocsPage from '../DocsPage'
import sidebarResponse from '../test/fixtures/sidebar-response.json'
import pageResponse from '../test/fixtures/page-response.json'

const theme = createTheme()

// The one statement of an /api/annot response: the annotation, carrying its compiled value.
function annotResponse(annot, value) {
  // A RawValue and not a Value: what a page carries is the program as it was written, so the
  // annotation is a raw expression.
  const expr = {
    tag: 'RawValue',
    rvMeta: { getMetaDat: [null, null, value], getMetaId: annot, getMetaPos: null, getMetaType: null },
    rvName: `/Catln/Doc/#${annot}`,
  }
  return { tag: 'Success', contents: [{ rprgmImports: [], rprgmStatements: [{ rstStatement: { tag: 'RawAnnot', ranAnnot: expr }, rstChildren: [] }] }, []] }
}

const sidebarValue = {
  tag: 'TupleVal',
  name: '/Catln/Doc/Show/Sidebar',
  args: { '/pages': { tag: 'StrVal', contents: JSON.stringify(sidebarResponse) } },
}

function annotFor(url) {
  if (url.startsWith('/api/annot?annot=%23sidebar')) return annotResponse('sidebar', sidebarValue)
}

// jsdom does not scroll, so the window's own scrolling is not there to be called or read back.
// Stubbed for every case here rather than only the ones asserting on it, since the layout scrolls
// on every navigation and an unimplemented call is an error reported into every one of them.
beforeEach(() => {
  vi.spyOn(window, 'scrollTo').mockImplementation(() => {})
})

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

// Going somewhere is what replaces the page shown beside the sidebar. The layout does it, so it is
// about going somewhere rather than about what was clicked to go there: the cases below click the
// sidebar because that is how most pages are reached, but a link in a page's own writing is the
// same act and gets the same thing.
describe('going to another page', () => {
  // Two pages of one directory, so the sidebar is showing both: it opens the directories holding
  // the page being read, and the page being read is the first of these.
  const FROM = '/stack/core/main.ct'
  const TO = 'bool.ct'

  // Answers for every page but the one navigated to, which is left compiling. So what is on screen
  // after the click is what a reader waiting on the compiler is shown, rather than a page which
  // happened to arrive before the assertion.
  function fetchLeavingItCompiling() {
    return vi.fn(url => {
      if (url.startsWith('/api/page')) {
        return url.includes(TO)
          ? new Promise(() => {})
          : Promise.resolve({ status: 200, json: () => Promise.resolve(pageResponse) })
      }
      return Promise.resolve({
        status: 200,
        json: () => Promise.resolve(annotFor(url) || { tag: 'Success', contents: [{ readonly: false, rootPage: ROOT_PAGE }, []] }),
      })
    })
  }

  // The route which reads the page out of the address, rather than a fixed one, since what is being
  // tested is what going to a different address does.
  async function renderAt(page) {
    const router = createMemoryRouter(
      createRoutesFromElements(
        <Route path="/" element={<Root />}>
          <Route path="docs/:prgmName" element={<Docs />} />
        </Route>
      ),
      { initialEntries: [`/docs/${encodeURIComponent(page)}`] }
    )
    render(<ThemeProvider theme={theme}><RouterProvider router={router} /></ThemeProvider>)
    await waitFor(() => expect(screen.getByText('hello world')).toBeInTheDocument())
  }

  it('shows the page asked for waiting, not the page just left', async () => {
    const user = userEvent.setup()
    global.fetch = fetchLeavingItCompiling()

    await renderAt(FROM)
    await user.click(screen.getByRole('link', { name: TO }))

    expect(screen.getByText('Loading...')).toBeInTheDocument()
    expect(screen.queryByText('hello world')).not.toBeInTheDocument()
  })

  // The sidebar is as long as the doc server has pages, so a reader who scrolled down it to pick a
  // page would otherwise be left below everything the page they picked has to say.
  it('goes back to the top of it', async () => {
    const user = userEvent.setup()
    global.fetch = fetchLeavingItCompiling()

    await renderAt(FROM)
    window.scrollTo.mockClear()
    await user.click(screen.getByRole('link', { name: TO }))

    expect(window.scrollTo).toHaveBeenCalledWith(0, 0)
  })

  // Only the page is replaced. The sidebar is beside it rather than part of it, and it lists every
  // page the doc server holds -- compiling that again for each page a reader opens is the cost the
  // one-instance layout exists to avoid.
  it('leaves the sidebar as it was, without asking for it again', async () => {
    const user = userEvent.setup()
    global.fetch = fetchLeavingItCompiling()

    await renderAt(FROM)
    const asked = global.fetch.mock.calls.filter(([url]) => url.startsWith('/api/annot')).length
    await user.click(screen.getByRole('link', { name: TO }))

    expect(screen.getByRole('link', { name: TO })).toBeInTheDocument()
    expect(global.fetch.mock.calls.filter(([url]) => url.startsWith('/api/annot'))).toHaveLength(asked)
  })
})
