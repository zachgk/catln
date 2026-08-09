import React from 'react'
import { render, screen, waitFor } from '@testing-library/react'
import userEvent from '@testing-library/user-event'
import { createMemoryRouter, RouterProvider, createRoutesFromElements, Route } from 'react-router-dom'
import { ThemeProvider, createTheme } from '@mui/material/styles'
import { Root, flattenPages } from '../App'
import sidebarResponse from '../test/fixtures/sidebar-response.json'

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

// The navbar's box covers the same pages the sidebar shows, by an annotation of its own, so its
// list is read off the same fixture.
const searchValue = {
  tag: 'TupleVal',
  name: '/Catln/Doc/Show/Search',
  args: { '/pages': { tag: 'StrVal', contents: JSON.stringify(flattenPages(sidebarResponse).map(([page]) => page)) } },
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

describe('Root', () => {
  beforeEach(() => {
    global.fetch = vi.fn(url => Promise.resolve({
      status: 200,
      json: () => Promise.resolve(annotFor(url) || { tag: 'Success', contents: [{ readonly: false }, []] }),
    }))
  })

  function annotFor(url) {
    if (url.startsWith('/api/annot?annot=%23sidebar')) return annotResponse('sidebar', sidebarValue)
    if (url.startsWith('/api/annot?annot=%23search')) return annotResponse('search', searchValue)
  }

  // Both are about which pages there are rather than about one of them, which is what being asked
  // for without a page means.
  it('asks for the sidebar and the search box without a page, so both cover every page', async () => {
    renderRoot()

    await waitFor(() => expect(screen.getByText('test')).toBeInTheDocument())
    const annotCalls = global.fetch.mock.calls.map(c => c[0]).filter(u => u.startsWith('/api/annot'))
    // Sorted rather than in order: which mounts first is only where they sit in the layout.
    expect([...annotCalls].sort()).toEqual(['/api/annot?annot=%23search', '/api/annot?annot=%23sidebar'])
  })

  it('gives back all of its width when the navbar control contracts it', async () => {
    const user = userEvent.setup()
    renderRoot()

    await waitFor(() => expect(screen.getByText('test')).toBeInTheDocument())
    expect(screen.getByTestId('sidebar')).toHaveStyle({ width: '300px' })

    await user.click(screen.getByLabelText('Hide sidebar'))
    expect(screen.getByTestId('sidebar')).toHaveStyle({ width: '0px' })

    // Contracted it is still holding the page tree, which is the app's page list.
    expect(screen.getByText('test')).toBeInTheDocument()
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

describe('flattenPages', () => {
  it('reads the pages of a tree in the order it shows them', () => {
    expect(flattenPages(sidebarResponse)).toEqual([
      ['/stack/core/main.ct', { impDir: '/stack/core' }],
      ['/stack/core/bool.ct', { impDir: '/stack/core' }],
      ['/stack/core/webdoc.ct', { impDir: '/stack/core' }],
      ['/test/id.ct', { impDir: '/test' }],
    ])
  })
})
