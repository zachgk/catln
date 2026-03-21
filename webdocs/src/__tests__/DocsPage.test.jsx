import React from 'react'
import { render, screen, waitFor, fireEvent } from '@testing-library/react'
import { MemoryRouter } from 'react-router-dom'
import { ThemeProvider, createTheme } from '@mui/material/styles'
import { TocContext } from '../Common/Common'
import DocsPage from '../DocsPage'
import pageResponse from '../test/fixtures/page-response.json'

vi.mock('@monaco-editor/react', () => ({
  default: ({ value }) => (
    <textarea data-testid="monaco-editor" defaultValue={value} />
  )
}))

const theme = createTheme()

const tocData = {
  isLoaded: true,
  data: { '/test/main.ct': { impDir: '/test' } },
  notes: [],
}

function mockFetch(overrides = {}) {
  return vi.fn((url) => {
    if (url.startsWith('/api/page') && overrides.page) {
      return overrides.page(url)
    }
    if (url.startsWith('/api/page')) {
      return Promise.resolve({
        status: 200,
        json: () => Promise.resolve(pageResponse),
      })
    }
    return Promise.resolve({
      status: 200,
      json: () => Promise.resolve({ tag: 'Success', contents: [null, []] }),
    })
  })
}

function Providers({ children }) {
  return (
    <ThemeProvider theme={theme}>
      <TocContext.Provider value={tocData}>
        <MemoryRouter>{children}</MemoryRouter>
      </TocContext.Provider>
    </ThemeProvider>
  )
}

describe('DocsPage', () => {
  it('shows Loading then content', async () => {
    global.fetch = mockFetch()

    render(<Providers><DocsPage prgmName="test%2Fmain.ct" /></Providers>)

    expect(screen.getByText('Loading...')).toBeInTheDocument()

    await waitFor(() => {
      expect(screen.queryByText('Loading...')).not.toBeInTheDocument()
    })

    expect(global.fetch).toHaveBeenCalledWith('/api/page?prgmName=test%2Fmain.ct')
  })

  it('shows error on failed fetch', async () => {
    global.fetch = mockFetch({
      page: () => Promise.resolve({ status: 500, statusText: 'Server Error' }),
    })

    render(<Providers><DocsPage prgmName="bad" /></Providers>)

    await waitFor(() => {
      expect(screen.getByText('Error: Server Error')).toBeInTheDocument()
    })
  })

  it('shows notes on ResFail', async () => {
    global.fetch = mockFetch({
      page: () => Promise.resolve({
        status: 200,
        json: () => Promise.resolve({
          tag: 'ResFail',
          contents: [{ tp: 'CNoteError', msg: 'compilation failed' }],
        }),
      }),
    })

    render(<Providers><DocsPage prgmName="broken" /></Providers>)

    await waitFor(() => {
      expect(screen.getByText('compilation failed')).toBeInTheDocument()
    })
  })

  it('renders edit toggle button', async () => {
    global.fetch = mockFetch()

    render(<Providers><DocsPage prgmName="test%2Fmain.ct" /></Providers>)

    await waitFor(() => {
      expect(screen.queryByText('Loading...')).not.toBeInTheDocument()
    })

    expect(screen.getByLabelText('Edit')).toBeInTheDocument()
  })

  it('clicking edit shows editor with source from page response', async () => {
    global.fetch = mockFetch()

    render(<Providers><DocsPage prgmName="test%2Fmain.ct" /></Providers>)

    await waitFor(() => {
      expect(screen.queryByText('Loading...')).not.toBeInTheDocument()
    })

    fireEvent.click(screen.getByLabelText('Edit'))

    await waitFor(() => {
      expect(screen.getByTestId('monaco-editor')).toBeInTheDocument()
    })

    // Only one fetch call — the combined /api/page endpoint
    expect(global.fetch).toHaveBeenCalledTimes(1)
    expect(global.fetch).toHaveBeenCalledWith('/api/page?prgmName=test%2Fmain.ct')
    expect(screen.getByTestId('monaco-editor')).toHaveValue('main = 42\n')
  })

  it('clicking view returns to AST view', async () => {
    global.fetch = mockFetch()

    render(<Providers><DocsPage prgmName="test%2Fmain.ct" /></Providers>)

    await waitFor(() => {
      expect(screen.queryByText('Loading...')).not.toBeInTheDocument()
    })

    // Switch to edit mode
    fireEvent.click(screen.getByLabelText('Edit'))
    await waitFor(() => {
      expect(screen.getByTestId('monaco-editor')).toBeInTheDocument()
    })

    // Switch back to view mode
    fireEvent.click(screen.getByLabelText('View'))
    await waitFor(() => {
      expect(screen.queryByTestId('monaco-editor')).not.toBeInTheDocument()
    })
  })

  it('shows no source editor when page has no source text', async () => {
    global.fetch = mockFetch({
      page: () => Promise.resolve({
        status: 200,
        json: () => Promise.resolve({
          tag: 'Success',
          contents: [[pageResponse.contents[0][0], null], []],
        }),
      }),
    })

    render(<Providers><DocsPage prgmName="test%2Fmain.ct" /></Providers>)

    await waitFor(() => {
      expect(screen.queryByText('Loading...')).not.toBeInTheDocument()
    })

    fireEvent.click(screen.getByLabelText('Edit'))

    await waitFor(() => {
      expect(screen.queryByText('Loading...')).not.toBeInTheDocument()
    })

    expect(screen.queryByTestId('monaco-editor')).not.toBeInTheDocument()
  })
})
