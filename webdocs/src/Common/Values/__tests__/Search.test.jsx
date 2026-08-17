import React from 'react'
import { render, screen, waitFor, within } from '@testing-library/react'
import userEvent from '@testing-library/user-event'
import { MemoryRouter, useLocation } from 'react-router-dom'
import { ThemeProvider, createTheme } from '@mui/material/styles'
import Search, { AppBarSearch } from '../Search'
import { ConfigContext } from '../../Common'

const theme = createTheme()

const PAGES = ['/stack/core/main.ct', '/test/id.ct', '/test/utils.ct']

// The annotations that are pages of their own, as the compiler enumerates them: what to run, and
// what to offer it as. These are the entries stack/core/webdoc.ct actually marks, so a case here
// narrows the same strings a reader would be offered — the box does not care which they are, but a
// fixture naming an annotation that is not offered would claim it was.
const DOC_PAGES = [
  { dpAnnot: '#listProgram(mode= /Catln/Doc/Desugar)', dpLabel: '#listProgram(mode= Desugar)' },
  { dpAnnot: '#listProgram(mode= /Catln/Doc/Typecheck)', dpLabel: '#listProgram(mode= Typecheck)' },
]

// The args of a compiled Show/Search: the pages it covers, as a JSON string.
function search(pages = PAGES) {
  return { '/pages': { tag: 'StrVal', contents: JSON.stringify(pages) } }
}

// The args of a compiled Show/DocPages, which carries its list the same way.
function docPages(entries = DOC_PAGES) {
  return { '/pages': { tag: 'StrVal', contents: JSON.stringify(entries) } }
}

// What /api/call answers with: the value the compiler evaluated the call to.
function valueResponse(name, args) {
  return { tag: 'Success', contents: [{ tag: 'TupleVal', name, args }, []] }
}

function callResponse(pages) {
  return valueResponse('/Catln/Doc/Show/Search', search(pages))
}

function docPagesResponse(entries) {
  return valueResponse('/Catln/Doc/Show/DocPages', docPages(entries))
}

function LocationDisplay() {
  const location = useLocation()
  return (
    <>
      <div data-testid="location">{location.pathname}</div>
      {/* An annotation page is named by its query rather than by its path, so a case saying which
          page a pick opened has to be able to read it. */}
      <div data-testid="search">{location.search}</div>
    </>
  )
}

// The page the doc server opens at. A search is called in it, so it has to be there before the box
// asks for anything.
const ROOT_PAGE = '/test/id.ct'

const SEARCH_FUN = '/Catln/Doc/searchWebdocs'
const DOC_PAGES_FUN = '/Catln/Doc/docPages'

function callUrl(fun, query, prgmName = ROOT_PAGE) {
  const args = encodeURIComponent(JSON.stringify({ query }))
  return `/api/call?fun=${encodeURIComponent(fun)}&args=${args}&prgmName=${encodeURIComponent(prgmName)}`
}

// The calls a box makes for what has been typed into it: one for the pages that are files, one for
// the pages that are annotations.
const searchUrl = (query, prgmName) => callUrl(SEARCH_FUN, query, prgmName)
const docPagesUrl = (query, prgmName) => callUrl(DOC_PAGES_FUN, query, prgmName)

const funOf = (url) => decodeURIComponent(url.split('fun=')[1].split('&')[0])
const queryOf = (url) => JSON.parse(decodeURIComponent(url.split('args=')[1].split('&')[0])).query

// Answers each of the box's two calls the way the compiler does: the pages whose path holds the
// query, and the annotation pages whose label holds it. Recorded, so a case can say how many calls a
// burst of typing became, and which of the two it was.
function mockBox({ pages = PAGES, annots = DOC_PAGES } = {}) {
  global.fetch = vi.fn((url) => {
    const query = queryOf(url)
    const holds = (s) => s.toLowerCase().includes(query.toLowerCase())
    const res = funOf(url) === DOC_PAGES_FUN
      ? docPagesResponse(annots.filter(e => holds(e.dpLabel)))
      : callResponse(pages.filter(holds))
    return Promise.resolve({ status: 200, json: () => Promise.resolve(res) })
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
  beforeEach(() => mockBox())

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
    global.fetch = vi.fn((url) => Promise.resolve({
      status: 200,
      json: () => Promise.resolve(funOf(url) === DOC_PAGES_FUN
        ? docPagesResponse([])
        : callResponse(['/test/somewhere/else.ct'])),
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
  // And asks for no pages to do it: a box nobody has typed into matches every page there is, so
  // asking would be the doc server's whole list crossing to draw a control nobody has used. The
  // annotations it does ask for are a handful of entries, and are the one thing a reader cannot
  // search for without being shown that it is there.
  it('starts from the pages the value it was rendered from carried, asking for no more', async () => {
    const user = userEvent.setup()
    render(<Providers><Search {...search()} /></Providers>)

    await user.click(screen.getByRole('combobox'))

    const listbox = await screen.findByRole('listbox')
    expect(within(listbox).getByText('/stack/core/main.ct')).toBeInTheDocument()
    expect(within(listbox).getByText('/test/id.ct')).toBeInTheDocument()
    expect(global.fetch.mock.calls.map(([url]) => url)).toEqual([docPagesUrl('')])
  })

  // A reader cannot search for a page they do not know is there, and an annotation is not a file to
  // come across. So these are offered before anything is typed, which is the whole of what the
  // section is for.
  it('offers the pages that are annotations before anything is typed', async () => {
    const user = userEvent.setup()
    render(<Providers><Search {...search()} /></Providers>)

    await user.click(screen.getByRole('combobox'))

    const listbox = await screen.findByRole('listbox')
    expect(await within(listbox).findByText('#listProgram(mode= Typecheck)')).toBeInTheDocument()
    expect(within(listbox).getByText('#listProgram(mode= Desugar)')).toBeInTheDocument()
  })

  // The two are different kinds of thing to open, asked for separately and answered separately, so a
  // reader picking one is told which they are picking.
  it('draws the two kinds under their own headings', async () => {
    const user = userEvent.setup()
    mockBox({ pages: ['/stack/core/typecheck.ct', '/test/id.ct'] })
    render(<Providers><Search {...search()} /></Providers>)

    await user.type(screen.getByRole('combobox'), 'typecheck')

    // Both sections are answered separately, so both are waited for.
    const listbox = await screen.findByRole('listbox')
    await waitFor(() => expect(within(listbox).getByText('#listProgram(mode= Typecheck)')).toBeInTheDocument())
    await waitFor(() => expect(within(listbox).getByText('/stack/core/typecheck.ct')).toBeInTheDocument())
    expect(within(listbox).getByText('Annotations')).toBeInTheDocument()
    expect(within(listbox).getByText('Pages')).toBeInTheDocument()
    // Narrowed within the section too, not merely present: the other entry does not hold the query.
    expect(within(listbox).queryByText('#listProgram(mode= Desugar)')).not.toBeInTheDocument()
  })

  // A heading over nothing reads as a section that failed to load rather than as one nothing matched.
  it('draws no heading for a kind nothing matched', async () => {
    const user = userEvent.setup()
    render(<Providers><Search {...search()} /></Providers>)

    await user.type(screen.getByRole('combobox'), 'utils')

    // The section is drawn from what was last answered until the next answer arrives, the same way
    // the pages are, so that neither empties under a reader between two letters. What is waited for
    // here is that answer.
    const listbox = await screen.findByRole('listbox')
    await waitFor(() => expect(within(listbox).getByText('/test/utils.ct')).toBeInTheDocument())
    await waitFor(() => expect(within(listbox).queryByText('Annotations')).not.toBeInTheDocument())
    expect(within(listbox).getByText('Pages')).toBeInTheDocument()
  })

  // An annotation is run rather than read, so it opens the page that evaluating it produces. Named
  // with no page, which is what makes it mean the same wherever it was picked from.
  it('runs the annotation that is picked', async () => {
    const user = userEvent.setup()
    render(<Providers><Search {...search()} /></Providers>)

    const input = screen.getByRole('combobox')
    await user.click(input)
    await user.click(await screen.findByText('#listProgram(mode= Typecheck)'))

    expect(screen.getByTestId('location').textContent).toBe('/annot')
    expect(decodeURIComponent(screen.getByTestId('search').textContent))
      .toBe('?annot=#listProgram(mode= /Catln/Doc/Typecheck)')
    expect(input).toHaveValue('')
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
  beforeEach(() => mockBox())

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

  // The navbar's box carries no list to start from, so before this it offered nothing at all until
  // something was typed. What a reader has not got to is exactly what is worth offering, and an
  // annotation page is not something they can come across by reading.
  it('offers the pages that are annotations before anything is typed', async () => {
    const user = userEvent.setup()
    render(<Providers><AppBarSearch /></Providers>)

    await user.click(screen.getByRole('combobox'))

    const listbox = await screen.findByRole('listbox')
    expect(await within(listbox).findByText('#listProgram(mode= Typecheck)')).toBeInTheDocument()
    expect(within(listbox).getByText('Annotations')).toBeInTheDocument()
    // Nothing has been typed, so there is nothing to have found among the pages.
    expect(within(listbox).queryByText('Pages')).not.toBeInTheDocument()
    expect(global.fetch.mock.calls.map(([url]) => url)).toEqual([docPagesUrl('')])
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
