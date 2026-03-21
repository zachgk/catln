import React from 'react'
import { render, screen } from '@testing-library/react'
import userEvent from '@testing-library/user-event'
import { MemoryRouter } from 'react-router-dom'
import { ThemeProvider, createTheme } from '@mui/material/styles'
import { TocContext } from '../../Common'
import { AppBarSearch } from '../Search'

const theme = createTheme()

const tocData = {
  isLoaded: true,
  data: {
    '/test/main.ct': { impDir: '/test' },
    '/test/utils.ct': { impDir: '/test' },
  },
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

describe('AppBarSearch', () => {
  it('renders search input', () => {
    render(<Providers><AppBarSearch /></Providers>)
    const input = screen.getByRole('combobox')
    expect(input).toBeInTheDocument()
  })

  it('shows autocomplete options from TocContext', async () => {
    const user = userEvent.setup()
    render(<Providers><AppBarSearch /></Providers>)

    const input = screen.getByRole('combobox')
    await user.click(input)
    await user.type(input, '/test')

    // MUI Autocomplete should show matching options
    const listbox = await screen.findByRole('listbox')
    expect(listbox).toBeInTheDocument()
  })
})
