import React from 'react'
import { render, screen, waitFor, fireEvent } from '@testing-library/react'
import { MemoryRouter } from 'react-router-dom'
import { ThemeProvider, createTheme } from '@mui/material/styles'
import { TocContext } from '../Common/Common'
import DocsPage from '../DocsPage'
import pageResponse from '../test/fixtures/page-response.json'

const theme = createTheme()

const tocData = {
  isLoaded: true,
  data: { '/test/main.ct': { impDir: '/test' } },
  notes: [],
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
    global.fetch = vi.fn(() =>
      Promise.resolve({
        status: 200,
        json: () => Promise.resolve(pageResponse),
      })
    )

    render(<Providers><DocsPage prgmName="test%2Fmain.ct" /></Providers>)

    // Initially shows loading
    expect(screen.getByText('Loading...')).toBeInTheDocument()

    // After fetch, shows content
    await waitFor(() => {
      expect(screen.queryByText('Loading...')).not.toBeInTheDocument()
    })

    // Verify it called the correct API
    expect(global.fetch).toHaveBeenCalledWith('/api/page?prgmName=test%2Fmain.ct')
  })

  it('shows error on failed fetch', async () => {
    global.fetch = vi.fn(() =>
      Promise.resolve({ status: 500, statusText: 'Server Error' })
    )

    render(<Providers><DocsPage prgmName="bad" /></Providers>)

    await waitFor(() => {
      expect(screen.getByText('Error: Server Error')).toBeInTheDocument()
    })
  })

  it('shows notes on ResFail', async () => {
    global.fetch = vi.fn(() =>
      Promise.resolve({
        status: 200,
        json: () => Promise.resolve({
          tag: 'ResFail',
          contents: [{ tp: 'CNoteError', msg: 'compilation failed' }],
        }),
      })
    )

    render(<Providers><DocsPage prgmName="broken" /></Providers>)

    await waitFor(() => {
      expect(screen.getByText('compilation failed')).toBeInTheDocument()
    })
  })
})
