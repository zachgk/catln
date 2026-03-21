import React from 'react'
import { render } from '@testing-library/react'
import { MemoryRouter } from 'react-router-dom'
import { ThemeProvider, createTheme } from '@mui/material/styles'
import { TocContext } from '../Common/Common'

const theme = createTheme()

export const mockTocData = [
  ['test/main.ct', { prgmName: 'test/main.ct', displayName: 'main.ct' }],
  ['test/utils.ct', { prgmName: 'test/utils.ct', displayName: 'utils.ct' }],
  ['test/sub/nested.ct', { prgmName: 'test/sub/nested.ct', displayName: 'nested.ct' }],
]

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
