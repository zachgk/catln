import React from 'react'
import { render, screen, waitFor, fireEvent } from '@testing-library/react'
import { MemoryRouter } from 'react-router-dom'
import { ThemeProvider, createTheme } from '@mui/material/styles'
import ListProgram from '../ListProgram'
import typecheckResponse from '../../../test/fixtures/typecheck-response.json'

const theme = createTheme()

function Providers({ children }) {
  return (
    <ThemeProvider theme={theme}>
      <MemoryRouter>{children}</MemoryRouter>
    </ThemeProvider>
  )
}

describe('ListProgram', () => {
  beforeEach(() => {
    global.fetch = vi.fn((url) => {
      return Promise.resolve({
        status: 200,
        json: () => Promise.resolve(typecheckResponse),
      })
    })
  })

  it('default mode fetches /api/typecheck', async () => {
    render(<Providers><ListProgram /></Providers>)

    await waitFor(() => {
      expect(global.fetch).toHaveBeenCalledWith('/api/typecheck')
    })
  })

  it('selecting "No Typecheck" radio fetches /api/desugar', async () => {
    render(<Providers><ListProgram /></Providers>)

    const noTypecheckRadio = screen.getByLabelText('No Typecheck')
    fireEvent.click(noTypecheckRadio)

    await waitFor(() => {
      expect(global.fetch).toHaveBeenCalledWith('/api/desugar')
    })
  })

  it('selecting "TreeBuild" radio fetches /api/treebuild', async () => {
    render(<Providers><ListProgram /></Providers>)

    const treeBuildRadio = screen.getByLabelText('TreeBuild')
    fireEvent.click(treeBuildRadio)

    await waitFor(() => {
      expect(global.fetch).toHaveBeenCalledWith('/api/treebuild')
    })
  })
})
