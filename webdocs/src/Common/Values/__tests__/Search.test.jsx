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

// What /api/call answers a search with: the Show/Search the compiler evaluated it to.
function callResponse(pages) {
  return {
    tag: 'Success',
    contents: [{ tag: 'TupleVal', name: '/Catln/Doc/Show/Search', args: search(pages) }, []],
  }
}

function LocationDisplay() {
  const location = useLocation()
  return <div data-testid="location">{location.pathname}</div>
}

// The page the doc server opens at. A search is called in it, so it has to be there before the box
// asks for anything.
const ROOT_PAGE = '/test/id.ct'

const SEARCH_FUN = '/Catln/Doc/searchWebdocs'

// The call a box makes for what has been typed into it.
function searchUrl(query, prgmName = ROOT_PAGE) {
  const args = encodeURIComponent(JSON.stringify({ query }))
  return `/api/call?fun=${encodeURIComponent(SEARCH_FUN)}&args=${args}&prgmName=${encodeURIComponent(prgmName)}`
}

// Answers every search with the pages whose path holds the query, which is what the compiler's own
// searchPagesFor does. Recorded, so a case can say how many searches a burst of typing became.
function mockSearch(pages = PAGES) {
  global.fetch = vi.fn((url) => {
    const { query } = JSON.parse(decodeURIComponent(url.split('args=')[1].split('&')[0]))
    const matched = pages.filter(p => p.toLowerCase().includes(query.toLowerCase()))
    return Promise.resolve({ status: 200, json: () => Promise.resolve(callResponse(matched)) })
  })
}

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
  beforeEach(() => mockSearch())

  // The search is done by the compiler over the pages it holds, so what crosses is what matched
  // rather than every page there is. The box says what was typed and shows what it is answered with.
  it('asks the compiler for what was typed, and offers what it is answered with', async () => {
    const user = userEvent.setup()
    render(<Providers><Search {...search()} /></Providers>)

    await user.type(screen.getByRole('combobox'), '/test')

    const listbox = await screen.findByRole('listbox')
    await waitFor(() => expect(within(listbox).queryByText('/stack/core/main.ct')).not.toBeInTheDocument())
    expect(within(listbox).getByText('/test/id.ct')).toBeInTheDocument()
    expect(within(listbox).getByText('/test/utils.ct')).toBeInTheDocument()
    expect(global.fetch).toHaveBeenCalledWith(searchUrl('/test'))
  })

  // The options are the answer to what was typed, so they are shown as they came. Filtering them
  // again here would search them a second time by rules that are not the compiler's, and hide
  // whatever it found by rules of its own.
  it('offers what it was answered with even where it does not read as a match', async () => {
    const user = userEvent.setup()
    global.fetch = vi.fn(() => Promise.resolve({
      status: 200,
      json: () => Promise.resolve(callResponse(['/test/somewhere/else.ct'])),
    }))
    render(<Providers><Search {...search()} /></Providers>)

    await user.type(screen.getByRole('combobox'), 'utils')

    const listbox = await screen.findByRole('listbox')
    await waitFor(() => expect(within(listbox).getByText('/test/somewhere/else.ct')).toBeInTheDocument())
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

  // A reader types a letter at a time and is looking for a word, so the word is what is searched
  // for. That the letters before it are not searched for is useDebounced's, and is a case of its
  // own; what is here is that the box searches for what the reader stopped at.
  it('searches for the word rather than for the letter last typed', async () => {
    const user = userEvent.setup()
    render(<Providers><Search {...search()} /></Providers>)

    await user.type(screen.getByRole('combobox'), 'utils')

    await waitFor(() => expect(global.fetch).toHaveBeenCalledWith(searchUrl('utils')))
  })

  // Between two letters the box is waiting to be told what the second one found. Emptying it in the
  // meantime would close the list and reopen it under a reader partway through typing.
  it('goes on offering what it last found while the next search is answered', async () => {
    const user = userEvent.setup()
    render(<Providers><Search {...search()} /></Providers>)

    const input = screen.getByRole('combobox')
    await user.type(input, 'utils')
    await screen.findByText('/test/utils.ct')

    // Asked again, with nothing coming back yet.
    global.fetch = vi.fn(() => new Promise(() => {}))
    await user.type(input, 'x')

    await waitFor(() => expect(global.fetch).toHaveBeenCalled())
    expect(screen.getByText('/test/utils.ct')).toBeInTheDocument()
  })

  // Before anything has been asked, the box offers what the value it was rendered from carried,
  // which for a #search is every page. So a search written in a page shows that page's pages at once
  // rather than after a call.
  //
  // And asks for nothing to do it: a box nobody has typed into is every page there is, so asking
  // would be the doc server's whole list crossing to draw a control nobody has used.
  it('starts from the pages the value it was rendered from carried, asking for nothing', async () => {
    const user = userEvent.setup()
    render(<Providers><Search {...search()} /></Providers>)

    await user.click(screen.getByRole('combobox'))

    const listbox = await screen.findByRole('listbox')
    expect(within(listbox).getByText('/stack/core/main.ct')).toBeInTheDocument()
    expect(within(listbox).getByText('/test/id.ct')).toBeInTheDocument()
    expect(global.fetch).not.toHaveBeenCalled()
  })

  // Nothing is being searched for, so what was last found is not what is on offer.
  it('goes back to what it started from when it is emptied', async () => {
    const user = userEvent.setup()
    render(<Providers><Search {...search()} /></Providers>)

    const input = screen.getByRole('combobox')
    await user.type(input, 'utils')
    await screen.findByText('/test/utils.ct')

    await user.clear(input)

    const listbox = await screen.findByRole('listbox')
    await waitFor(() => expect(within(listbox).getByText('/stack/core/main.ct')).toBeInTheDocument())
  })

  // The editor renders a page's values into monaco view zones, which are React roots outside the
  // router. Rendering the box there threw and took that root down with it.
  it('shows nothing where it would have nowhere to navigate to', () => {
    render(<ThemeProvider theme={theme}><Search {...search()} /></ThemeProvider>)

    expect(screen.queryByRole('combobox')).not.toBeInTheDocument()
  })
})

describe('AppBarSearch', () => {
  beforeEach(() => mockSearch())

  // What the box searches does not depend on the page it is called in -- it covers every page either
  // way -- so it names one. Being asked for with no page is being compiled against every page there
  // is, which for a control in the navbar is the whole doc server compiled to draw it.
  it('searches within the page the docs open at', async () => {
    const user = userEvent.setup()
    render(<Providers><AppBarSearch /></Providers>)

    await user.type(screen.getByRole('combobox'), 'utils')

    await waitFor(() => expect(global.fetch).toHaveBeenCalledWith(searchUrl('utils')))
    expect(await screen.findByText('/test/utils.ct')).toBeInTheDocument()
  })

  // The box is a control in the navbar, so it is there to be typed into from the start. What it has
  // to wait for is the page to call the search in, which is the config's to say.
  it('is there before it knows the page the docs open at, and asks for nothing yet', async () => {
    const user = userEvent.setup()
    render(<Providers config={{ isLoaded: false, data: null }}><AppBarSearch /></Providers>)

    await user.type(screen.getByRole('combobox'), 'utils')

    expect(screen.getByRole('combobox')).toBeInTheDocument()
    expect(global.fetch).not.toHaveBeenCalled()
  })

  // The box is a control in the navbar rather than a page of its own, so there is nowhere to report
  // to: a placeholder or a block of compiler notes would be laid out as part of the navbar.
  it('stays a box, offering nothing, when a search fails', async () => {
    global.fetch = vi.fn(() => Promise.resolve({
      status: 200,
      json: () => Promise.resolve({ tag: 'ResFail', contents: [{ tp: 'CNoteError', msg: 'no such function' }] }),
    }))
    const user = userEvent.setup()
    render(<Providers><AppBarSearch /></Providers>)

    await user.type(screen.getByRole('combobox'), 'utils')

    await waitFor(() => expect(global.fetch).toHaveBeenCalled())
    expect(screen.queryByText('no such function')).not.toBeInTheDocument()
    expect(screen.queryByRole('listbox')).not.toBeInTheDocument()
  })
})
