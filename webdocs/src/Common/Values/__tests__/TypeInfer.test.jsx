import React from 'react'
import { render, screen, waitFor } from '@testing-library/react'
import userEvent from '@testing-library/user-event'
import { MemoryRouter } from 'react-router-dom'
import { ThemeProvider, createTheme } from '@mui/material/styles'
import TypeInfer, { TypeInferPnt } from '../TypeInfer'
import { PrgmNameContext } from '../../Common'
import typeInferResponse from '../../../test/fixtures/typeinfer-response.json'
import typeInferPntResponse from '../../../test/fixtures/typeinfer-pnt-response.json'

const theme = createTheme()

// Both fixtures are the real payloads the compiler produced for test/Integration/code/id.ct,
// narrowed to the one definition (/id) and the one point (3) they are asserted about.
const PAGE = '/some/where/id.ct'
const PNT = 3

function Providers({ prgmName = PAGE, children }) {
  return (
    <ThemeProvider theme={theme}>
      <MemoryRouter>
        <PrgmNameContext.Provider value={prgmName}>{children}</PrgmNameContext.Provider>
      </MemoryRouter>
    </ThemeProvider>
  )
}

// The only arg of a compiled Show/TypeInfer: the inferred program and its notes, as a JSON string.
function typeInfer(notes = []) {
  const [prgm] = typeInferResponse
  return { '/constrain': { tag: 'StrVal', contents: JSON.stringify([prgm, notes]) } }
}

// The args of a compiled Show/TypeInferPnt: the trace filtered to one point, and that point.
function typeInferPnt(pnt = PNT) {
  return {
    '/trace': { tag: 'StrVal', contents: JSON.stringify(typeInferPntResponse) },
    '/pnt': { tag: 'IntVal', contents: pnt },
  }
}

describe('TypeInfer', () => {
  beforeEach(() => {
    global.fetch = vi.fn(() => Promise.reject(new Error('unexpected fetch')))
  })

  it('renders the inferred program carried by the value without fetching', () => {
    render(<Providers><TypeInfer {...typeInfer()} /></Providers>)

    // Every meta of the listed definition renders as its point number, which is the link into the
    // trace pane.
    expect(screen.getAllByRole('link', { name: String(PNT) }).length).toBeGreaterThan(0)
    expect(global.fetch).not.toHaveBeenCalled()
  })

  it('shows the notes the typecheck produced', () => {
    const notes = [{ id: null, msg: 'something went wrong', pos: null, tp: 'CNoteError' }]
    render(<Providers><TypeInfer {...typeInfer(notes)} /></Providers>)

    expect(screen.getByText(/something went wrong/)).toBeInTheDocument()
  })

  it('asks for a point as a second annotation about the same page', async () => {
    const user = userEvent.setup()
    global.fetch = vi.fn(() => Promise.resolve({
      status: 200,
      // An empty page, as the compiler names its fields.
      json: () => Promise.resolve({ tag: 'Success', contents: [{ rprgmImports: [], rprgmStatements: [] }, []] }),
    }))

    render(<Providers><TypeInfer {...typeInfer()} /></Providers>)
    expect(global.fetch).not.toHaveBeenCalled()

    await user.click(screen.getAllByRole('link', { name: String(PNT) })[0])

    // The pane is an embedded AnnotPage, so the point is requested as #typeInferPnt scoped to the
    // page this inference is of -- not as a route of its own.
    await waitFor(() => expect(global.fetch).toHaveBeenCalled())
    const url = global.fetch.mock.calls[0][0]
    expect(decodeURIComponent(url)).toBe(`/api/annot?annot=#typeInferPnt(pnt= ${PNT})&prgmName=${PAGE}`)
  })
})

describe('TypeInferPnt', () => {
  beforeEach(() => {
    global.fetch = vi.fn(() => Promise.reject(new Error('TypeInferPnt must not fetch')))
  })

  it('renders the filtered trace carried by the value without fetching', () => {
    render(<Providers><TypeInferPnt {...typeInferPnt()} /></Providers>)

    expect(screen.getByRole('heading', { name: `Pnt ${PNT}` })).toBeInTheDocument()
    // The point had an initial scheme and the constraints mentioning it, which is the whole of what
    // the trace is filtered down to.
    expect(screen.getByRole('heading', { name: 'Initial' })).toBeInTheDocument()
    expect(screen.getByRole('heading', { name: 'Constraints' })).toBeInTheDocument()
    expect(screen.getByText('UnionOf')).toBeInTheDocument()
    expect(global.fetch).not.toHaveBeenCalled()
  })

  it('renders on its own, with no page to highlight notes against', () => {
    // No MetaContext provider: this is a #typeInferPnt annotation opened directly rather than
    // embedded in a TypeInfer page.
    render(
      <ThemeProvider theme={theme}>
        <MemoryRouter><TypeInferPnt {...typeInferPnt()} /></MemoryRouter>
      </ThemeProvider>
    )

    expect(screen.getByRole('heading', { name: `Pnt ${PNT}` })).toBeInTheDocument()
  })
})
