import React from 'react'
import { render, screen, act } from '@testing-library/react'
import userEvent from '@testing-library/user-event'
import { MemoryRouter, createMemoryRouter, RouterProvider } from 'react-router-dom'
import { ThemeProvider, createTheme } from '@mui/material/styles'
import Sidebar, { ancestorPaths, pageOf } from '../Sidebar'
import sidebarResponse from '../../../test/fixtures/sidebar-response.json'

const theme = createTheme()

// The args of a compiled Show/Sidebar: the page tree, as a JSON string.
function sidebar(tree = sidebarResponse) {
  return { '/pages': { tag: 'StrVal', contents: JSON.stringify(tree) } }
}

// Rendered at the route a page is read at, which is what the tree opens itself to.
function Providers({ children, at = '/docs/%2Ftest%2Fid.ct' }) {
  return (
    <ThemeProvider theme={theme}>
      <MemoryRouter initialEntries={[at]}>
        {children}
      </MemoryRouter>
    </ThemeProvider>
  )
}

describe('Sidebar', () => {
  beforeEach(() => {
    global.fetch = vi.fn(() => Promise.reject(new Error('Sidebar must not fetch')))
  })

  it('renders the page tree carried by the value without fetching', () => {
    render(<Providers><Sidebar {...sidebar()} /></Providers>)

    expect(screen.getByText('stack')).toBeInTheDocument()
    expect(screen.getByText('test')).toBeInTheDocument()
    expect(global.fetch).not.toHaveBeenCalled()
  })

  it('links a page by the path it is asked for', () => {
    render(<Providers><Sidebar {...sidebar()} /></Providers>)

    expect(screen.getByText('id.ct').closest('a')).toHaveAttribute('href', '/docs/%2Ftest%2Fid.ct')
  })

  it('opens the directories holding the page being read', () => {
    render(<Providers at="/docs/%2Fstack%2Fcore%2Fbool.ct"><Sidebar {...sidebar()} /></Providers>)

    // /stack and /stack/core are both open, so the page inside them is on screen without a click.
    expect(screen.getByText('bool.ct')).toBeInTheDocument()
    // A directory holding no part of it stays closed.
    expect(screen.queryByText('id.ct')).not.toBeInTheDocument()
  })

  it('leaves directories as the reader puts them', async () => {
    const user = userEvent.setup()
    render(<Providers at="/docs/%2Ftest%2Fid.ct"><Sidebar {...sidebar()} /></Providers>)

    expect(screen.queryByText('main.ct')).not.toBeInTheDocument()
    await user.click(screen.getByText('stack'))
    await user.click(screen.getByText('core'))
    expect(screen.getByText('main.ct')).toBeInTheDocument()
  })

  // The sidebar is mounted before the page is known: it sits in the layout, above the route that
  // names one, and /docs redirects to a page only once the config it asks for has arrived.
  it('opens to the page once one is being read', async () => {
    const router = createMemoryRouter(
      [{path: '*', element: <Sidebar {...sidebar()} />}],
      {initialEntries: ['/docs']}
    )
    render(
      <ThemeProvider theme={theme}>
        <RouterProvider router={router} />
      </ThemeProvider>
    )

    expect(screen.queryByText('bool.ct')).not.toBeInTheDocument()
    await act(() => router.navigate('/docs/%2Fstack%2Fcore%2Fbool.ct'))
    expect(screen.getByText('bool.ct')).toBeInTheDocument()
  })
})

describe('pageOf', () => {
  it('reads the page a docs location is reading, and nothing elsewhere', () => {
    expect(pageOf('/docs/%2Fstack%2Fcore%2Fbool.ct')).toBe('/stack/core/bool.ct')
    expect(pageOf('/annot')).toBe('')
  })
})

describe('ancestorPaths', () => {
  it('names every directory holding a page', () => {
    expect(ancestorPaths('/stack/core/bool.ct')).toEqual(['/stack', '/stack/core'])
  })

  it('names nothing for a page at the root, or for no page at all', () => {
    expect(ancestorPaths('/id.ct')).toEqual([])
    expect(ancestorPaths(undefined)).toEqual([])
  })
})
