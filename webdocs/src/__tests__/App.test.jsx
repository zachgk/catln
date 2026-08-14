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

// The navbar's box covers the same pages the sidebar shows, by an annotation of its own.
const searchValue = {
  tag: 'TupleVal',
  name: '/Catln/Doc/Show/Search',
  args: {
    '/pages': {
      tag: 'StrVal',
      contents: JSON.stringify([
        '/stack/core/main.ct', '/stack/core/bool.ct', '/stack/core/webdoc.ct', '/test/id.ct',
      ]),
    },
  },
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
    if (url.startsWith('/api/annot?annot=%23search')) return annotResponse('search', searchValue)
  }

  // Both are about which pages there are rather than about one of them, and are answered that way
  // whatever page they are written in. So both name one -- the page the docs open at -- and neither
  // is ever asked for without a page, which is what asks for it to be compiled against every page
  // there is.
  it('asks for the sidebar and the search box within the page the docs open at', async () => {
    renderRoot()

    await waitFor(() => expect(screen.getByText('test')).toBeInTheDocument())
    const annotCalls = global.fetch.mock.calls.map(c => c[0]).filter(u => u.startsWith('/api/annot'))
    const scope = `&prgmName=${encodeURIComponent(ROOT_PAGE)}`
    // Sorted rather than in order: which mounts first is only where they sit in the layout.
    expect([...annotCalls].sort()).toEqual([
      `/api/annot?annot=%23search${scope}`,
      `/api/annot?annot=%23sidebar${scope}`,
    ])
  })

  // The page is not there until the config says what it is, and asking without one is exactly what
  // this exists to stop, so neither is asked for before then.
  it('asks for neither before it knows the page the docs open at', async () => {
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
