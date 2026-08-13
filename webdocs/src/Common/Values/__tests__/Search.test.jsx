import React from 'react'
import { render, screen, waitFor, within } from '@testing-library/react'
import userEvent from '@testing-library/user-event'
import { MemoryRouter, useLocation } from 'react-router-dom'
import { ThemeProvider, createTheme } from '@mui/material/styles'
import Search, { AppBarSearch } from '../Search'
import { ConfigContext } from '../../Common'

const theme = createTheme()

const PAGES = ['/stack/core/main.ct', '/test/id.ct', '/test/utils.ct']

// The args of a compiled Show/Search: the pages it covers, as a JSON string.
function search(pages = PAGES) {
  return { '/pages': { tag: 'StrVal', contents: JSON.stringify(pages) } }
}

function LocationDisplay() {
  const location = useLocation()
  return <div data-testid="location">{location.pathname}</div>
}

// The page the doc server opens at. The box is compiled in it, so it has to be there before the box
// asks for anything.
const ROOT_PAGE = '/test/id.ct'

function Providers({ children, config = { isLoaded: true, data: { rootPage: ROOT_PAGE } } }) {
  return (
    <ThemeProvider theme={theme}>
      <ConfigContext.Provider value={config}>
        <MemoryRouter>
          {children}
          <LocationDisplay />
        </MemoryRouter>
      </ConfigContext.Provider>
    </ThemeProvider>
  )
}

describe('Search', () => {
  beforeEach(() => {
    global.fetch = vi.fn(() => Promise.reject(new Error('Search must not fetch')))
  })

  it('offers the pages carried by the value without fetching', async () => {
    const user = userEvent.setup()
    render(<Providers><Search {...search()} /></Providers>)

    await user.type(screen.getByRole('combobox'), '/test')

    const listbox = await screen.findByRole('listbox')
    expect(within(listbox).getByText('/test/id.ct')).toBeInTheDocument()
    expect(within(listbox).getByText('/test/utils.ct')).toBeInTheDocument()
    // The options are the value's list, filtered by what was typed, rather than every page.
    expect(within(listbox).queryByText('/stack/core/main.ct')).not.toBeInTheDocument()
    expect(global.fetch).not.toHaveBeenCalled()
  })

  it('opens the page that is picked', async () => {
    const user = userEvent.setup()
    render(<Providers><Search {...search()} /></Providers>)

    const input = screen.getByRole('combobox')
    await user.type(input, 'utils')
    await user.click(await screen.findByText('/test/utils.ct'))

    expect(decodeURIComponent(screen.getByTestId('location').textContent)).toBe('/docs//test/utils.ct')
    // Left ready for the next search rather than holding the page now being read.
    expect(input).toHaveValue('')
  })

  // The editor renders a page's values into monaco view zones, which are React roots outside the
  // router. Rendering the box there threw and took that root down with it.
  it('shows nothing where it would have nowhere to navigate to', () => {
    render(<ThemeProvider theme={theme}><Search {...search()} /></ThemeProvider>)

    expect(screen.queryByRole('combobox')).not.toBeInTheDocument()
  })
})

describe('AppBarSearch', () => {
  // The one statement of an /api/annot response: the annotation, carrying its compiled value.
  function annotResponse(value) {
    const expr = {
      tag: 'Value',
      contents: [{ getMetaDat: [null, null, value], getMetaID: 'search', getMetaPos: null, getMetaType: null }, '/Catln/Doc/#search'],
    }
    return { tag: 'Success', contents: [[[], [[{ tag: 'RawAnnot', contents: expr }, []]]], []] }
  }

  const searchValue = { tag: 'TupleVal', name: '/Catln/Doc/Show/Search', args: search() }

  function mockAnnot(json) {
    global.fetch = vi.fn(() => Promise.resolve({ status: 200, json: () => Promise.resolve(json) }))
  }

  // What the box searches does not depend on the page it is compiled in -- it covers every page
  // either way -- so it names one. Being asked for with no page is being compiled against every
  // page there is, which for a control in the navbar is the whole doc server compiled to draw it.
  it('asks for the search within the page the docs open at', async () => {
    mockAnnot(annotResponse(searchValue))
    render(<Providers><AppBarSearch /></Providers>)

    await screen.findByRole('combobox')
    expect(global.fetch).toHaveBeenCalledWith(`/api/annot?annot=%23search&prgmName=${encodeURIComponent(ROOT_PAGE)}`)
  })

  it('asks for nothing before it knows the page the docs open at', async () => {
    mockAnnot(annotResponse(searchValue))
    render(<Providers config={{ isLoaded: false, data: null }}><AppBarSearch /></Providers>)

    expect(screen.queryByRole('combobox')).not.toBeInTheDocument()
    expect(global.fetch).not.toHaveBeenCalled()
  })

  // The box is a control in the navbar rather than a page of its own, so there is nowhere to report
  // to: a placeholder or a block of compiler notes would be laid out as part of the navbar.
  it('shows nothing while its annotation compiles', () => {
    global.fetch = vi.fn(() => new Promise(() => {}))
    render(<Providers><AppBarSearch /></Providers>)

    expect(screen.queryByRole('combobox')).not.toBeInTheDocument()
    expect(screen.queryByText(/Loading/)).not.toBeInTheDocument()
  })

  it('shows nothing, and reports why, when its annotation fails', async () => {
    const error = vi.spyOn(console, 'error').mockImplementation(() => {})
    mockAnnot({ tag: 'ResFail', contents: [{ tp: 'CNoteError', msg: 'no such annotation' }] })
    render(<Providers><AppBarSearch /></Providers>)

    await waitFor(() => expect(error).toHaveBeenCalled())
    expect(screen.queryByRole('combobox')).not.toBeInTheDocument()
    expect(screen.queryByText('no such annotation')).not.toBeInTheDocument()
  })
})
