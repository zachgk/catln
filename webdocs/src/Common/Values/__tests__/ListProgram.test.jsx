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

  describe('with a mode named by the compiled Show/ListProgram', () => {
    const modeVal = (name) => ({ tag: 'TupleVal', name, args: {} })

    it('shows the annotation mode rather than a local default', async () => {
      render(<Providers><ListProgram {...{ '/mode': modeVal('/Catln/Doc/TreeBuild') }} /></Providers>)

      expect(screen.getByLabelText('TreeBuild')).toBeChecked()
      await waitFor(() => {
        expect(global.fetch).toHaveBeenCalledWith('/api/treebuild')
      })
    })

    it('toggling a radio updates the annotation instead of local state', async () => {
      const update = vi.fn()
      render(
        <Providers>
          <ListProgram {...{ '/mode': modeVal('/Catln/Doc/Typecheck') }} update={update} />
        </Providers>
      )

      fireEvent.click(screen.getByLabelText('TreeBuild'))

      expect(update).toHaveBeenCalledWith('#listProgram(mode= /Catln/Doc/TreeBuild)')
      // The mode comes from the annotation, so nothing moves until the recompile arrives.
      expect(screen.getByLabelText('List')).toBeChecked()
    })
  })

  describe('with a program carried by the compiled Show/ListProgram', () => {
    // The value's prgm arg holds the same program /api/typecheck returns, as a JSON string.
    const carried = { tag: 'StrVal', contents: JSON.stringify(typecheckResponse.contents[0]) }

    it('renders the carried program without fetching', async () => {
      render(<Providers><ListProgram {...{ '/prgm': carried }} /></Providers>)

      await waitFor(() => {
        expect(screen.getByText('Types')).toBeInTheDocument()
      })
      expect(global.fetch).not.toHaveBeenCalled()
    })

    it('still fetches for the pipeline-stage modes it cannot carry', async () => {
      render(<Providers><ListProgram {...{ '/prgm': carried }} /></Providers>)

      fireEvent.click(screen.getByLabelText('TreeBuild'))

      await waitFor(() => {
        expect(global.fetch).toHaveBeenCalledWith('/api/treebuild')
      })
    })
  })
})
