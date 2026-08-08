import React from 'react'
import { render } from '@testing-library/react'
import { MemoryRouter } from 'react-router-dom'
import { ThemeProvider, createTheme } from '@mui/material/styles'
import { TocContext } from '../Common/Common'

const theme = createTheme()

// The shape TocContext actually holds: the pages of the sidebar's tree, keyed by the path a page is
// asked for, in the order the sidebar shows them.
export const mockTocData = {
  isLoaded: true,
  data: {
    '/test/main.ct': { impDir: '/test' },
    '/test/utils.ct': { impDir: '/test' },
    '/test/sub/nested.ct': { impDir: '/test/sub' },
  },
  notes: [],
}

export function renderWithProviders(ui, options = {}) {
  const {
    initialEntries = ['/'],
    tocData = mockTocData,
    ...renderOptions
  } = options

  function Wrapper({ children }) {
    return (
      <ThemeProvider theme={theme}>
        <TocContext.Provider value={tocData}>
          <MemoryRouter initialEntries={initialEntries}>
            {children}
          </MemoryRouter>
        </TocContext.Provider>
      </ThemeProvider>
    )
  }

  return render(ui, { wrapper: Wrapper, ...renderOptions })
}

export function mockApiResponse(tag, data, notes = []) {
  if (tag === 'Success') {
    return { tag: 'Success', contents: [data, notes] }
  }
  return { tag: 'ResFail', contents: notes }
}

export function mockFetchWith(responses) {
  global.fetch = vi.fn((url) => {
    for (const [pattern, response] of Object.entries(responses)) {
      if (url.includes(pattern)) {
        return Promise.resolve({
          status: 200,
          json: () => Promise.resolve(response),
        })
      }
    }
    return Promise.resolve({
      status: 404,
      statusText: 'Not Found',
    })
  })
}
