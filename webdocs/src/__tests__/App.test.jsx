import React from 'react'
import { render, screen, waitFor } from '@testing-library/react'
import userEvent from '@testing-library/user-event'
import { createMemoryRouter, RouterProvider, createRoutesFromElements, Route } from 'react-router-dom'
import { ThemeProvider, createTheme } from '@mui/material/styles'
import { Root, flattenPages } from '../App'
import sidebarResponse from '../test/fixtures/sidebar-response.json'

const theme = createTheme()

// The one statement of an /api/annot response: the annotation, carrying its compiled value.
function annotResponse(value) {
  const expr = {
    tag: 'Value',
    contents: [{ getMetaDat: [null, null, value], getMetaID: 'sidebar', getMetaPos: null, getMetaType: null }, '/Catln/Doc/#sidebar'],
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

describe('Root', () => {
  beforeEach(() => {
    global.fetch = vi.fn(url => Promise.resolve({
      status: 200,
      json: () => Promise.resolve(url.startsWith('/api/annot')
        ? annotResponse(sidebarValue)
        : { tag: 'Success', contents: [{ readonly: false }, []] }),
    }))
  })

  it('asks for the sidebar without a page, so it lists every page', async () => {
    renderRoot()

    await waitFor(() => expect(screen.getByText('test')).toBeInTheDocument())
    const annotCalls = global.fetch.mock.calls.map(c => c[0]).filter(u => u.startsWith('/api/annot'))
    expect(annotCalls).toEqual(['/api/annot?annot=%23sidebar'])
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
