import React from 'react'
import { render, screen, waitFor, fireEvent, act } from '@testing-library/react'
import { MemoryRouter, useLocation, useNavigate } from 'react-router-dom'
import { ThemeProvider, createTheme } from '@mui/material/styles'
import { ConfigContext } from '../Common/Common'
import { initVimMode, VimMode } from 'monaco-vim'
import DocsPage, { AnnotPage } from '../DocsPage'
import pageResponse from '../test/fixtures/page-response.json'
import notePageResponse from '../test/fixtures/note-page-response.json'
import overlayNotePageResponse from '../test/fixtures/overlay-note-page-response.json'

// Collects the keybindings DocsPage registers, so tests can fire Ctrl+S directly.
const mocks = vi.hoisted(() => ({ commands: [], selections: [], reveals: [], options: [] }))

vi.mock('@monaco-editor/react', () => ({
  default: ({ value, onChange, onMount, options }) => {
    mocks.options.push(options)
    React.useEffect(() => {
      onMount(
        {
          onDidDispose: () => {},
          addCommand: (keybinding, handler) => mocks.commands.push({ keybinding, handler }),
          setSelection: range => mocks.selections.push(range),
          revealRangeInCenter: range => mocks.reveals.push(range),
        },
        { KeyMod: { CtrlCmd: 2048 }, KeyCode: { KeyS: 49 } },
      )
      // eslint-disable-next-line react-hooks/exhaustive-deps
    }, [])
    return <textarea data-testid="monaco-editor" value={value} onChange={e => onChange(e.target.value)} />
  }
}))

const theme = createTheme()

const writableConfig = { isLoaded: true, data: { readonly: false }, notes: [] }

// A page response with some of what the server says about where the page sits replaced.
function pageWith(nav) {
  return { ...pageResponse, contents: [{ ...pageResponse.contents[0], ...nav }, []] }
}

function mockFetch(overrides = {}) {
  return vi.fn((url, options) => {
    if (url.startsWith('/api/write')) {
      if (overrides.write) {
        return overrides.write(url, options)
      }
      return Promise.resolve({
        status: 200,
        json: () => Promise.resolve({ tag: 'Success', contents: [[], []] }),
      })
    }
    if (url.startsWith('/api/page') && overrides.page) {
      return overrides.page(url)
    }
    if (url.startsWith('/api/page')) {
      return Promise.resolve({
        status: 200,
        json: () => Promise.resolve(pageResponse),
      })
    }
    return Promise.resolve({
      status: 200,
      json: () => Promise.resolve({ tag: 'Success', contents: [null, []] }),
    })
  })
}

function Providers({ children, config = writableConfig }) {
  return (
    <ThemeProvider theme={theme}>
      <ConfigContext.Provider value={config}>
        <MemoryRouter>{children}</MemoryRouter>
      </ConfigContext.Provider>
    </ThemeProvider>
  )
}

// Renders, waits for the page load, switches to edit mode and types newSource.
async function renderEditing(newSource, props = {}) {
  render(<Providers {...props}><DocsPage prgmName="test%2Fmain.ct" /></Providers>)

  await waitFor(() => {
    expect(screen.queryByText('Loading...')).not.toBeInTheDocument()
  })

  fireEvent.click(screen.getByLabelText('Edit'))

  await waitFor(() => {
    expect(screen.getByTestId('monaco-editor')).toBeInTheDocument()
  })

  fireEvent.change(screen.getByTestId('monaco-editor'), { target: { value: newSource } })
}

describe('DocsPage', () => {
  beforeEach(() => {
    mocks.commands.length = 0
    mocks.selections.length = 0
    mocks.reveals.length = 0
    mocks.options.length = 0
    initVimMode.mockClear()
    initVimMode.mockReturnValue({ dispose: vi.fn() })
    VimMode.Vim.defineEx.mockClear()
  })

  it('shows Loading then content', async () => {
    global.fetch = mockFetch()

    render(<Providers><DocsPage prgmName="test%2Fmain.ct" /></Providers>)

    expect(screen.getByText('Loading...')).toBeInTheDocument()

    await waitFor(() => {
      expect(screen.queryByText('Loading...')).not.toBeInTheDocument()
    })

    expect(global.fetch).toHaveBeenCalledWith('/api/page?prgmName=test%2Fmain.ct')
  })

  // A page is followed rather than asked about once, and a connection which fails says only that it
  // failed -- there is no status behind it to report, and the browser opens another by itself. So a
  // reader who never got the page is told the doc server is not answering. A page which failed to
  // compile is not this: that comes back as an ordinary answer carrying notes, which is the case
  // below.
  it('reports a doc server that never answered', async () => {
    global.fetch = mockFetch({
      page: () => Promise.resolve({ status: 500, statusText: 'Server Error' }),
    })

    render(<Providers><DocsPage prgmName="bad" /></Providers>)

    await waitFor(() => {
      expect(screen.getByText('Error: The doc server is not answering')).toBeInTheDocument()
    })
  })

  it('shows notes on ResFail', async () => {
    global.fetch = mockFetch({
      page: () => Promise.resolve({
        status: 200,
        json: () => Promise.resolve({
          tag: 'ResFail',
          contents: [{ tp: 'CNoteError', msg: 'compilation failed' }],
        }),
      }),
    })

    render(<Providers><DocsPage prgmName="broken" /></Providers>)

    await waitFor(() => {
      expect(screen.getByText('compilation failed')).toBeInTheDocument()
    })
  })

  it('renders edit toggle button', async () => {
    global.fetch = mockFetch()

    render(<Providers><DocsPage prgmName="test%2Fmain.ct" /></Providers>)

    await waitFor(() => {
      expect(screen.queryByText('Loading...')).not.toBeInTheDocument()
    })

    expect(screen.getByLabelText('Edit')).toBeInTheDocument()
  })

  it('clicking edit shows editor with source from page response', async () => {
    global.fetch = mockFetch()

    render(<Providers><DocsPage prgmName="test%2Fmain.ct" /></Providers>)

    await waitFor(() => {
      expect(screen.queryByText('Loading...')).not.toBeInTheDocument()
    })

    fireEvent.click(screen.getByLabelText('Edit'))

    await waitFor(() => {
      expect(screen.getByTestId('monaco-editor')).toBeInTheDocument()
    })

    // Only one fetch call — the combined /api/page endpoint
    expect(global.fetch).toHaveBeenCalledTimes(1)
    expect(global.fetch).toHaveBeenCalledWith('/api/page?prgmName=test%2Fmain.ct')
    expect(screen.getByTestId('monaco-editor')).toHaveValue('main = 42\n> "hello world"\n')
  })

  it('attaches vim mode to the editor with a status bar', async () => {
    global.fetch = mockFetch()

    render(<Providers><DocsPage prgmName="test%2Fmain.ct" /></Providers>)

    await waitFor(() => {
      expect(screen.queryByText('Loading...')).not.toBeInTheDocument()
    })

    fireEvent.click(screen.getByLabelText('Edit'))

    await waitFor(() => {
      expect(initVimMode).toHaveBeenCalled()
    })

    expect(initVimMode).toHaveBeenCalledWith(
      expect.anything(),
      screen.getByTestId('vim-status-bar'),
    )
  })

  it('disposes vim mode when leaving edit mode', async () => {
    global.fetch = mockFetch()

    render(<Providers><DocsPage prgmName="test%2Fmain.ct" /></Providers>)

    await waitFor(() => {
      expect(screen.queryByText('Loading...')).not.toBeInTheDocument()
    })

    fireEvent.click(screen.getByLabelText('Edit'))
    await waitFor(() => {
      expect(initVimMode).toHaveBeenCalled()
    })

    const { dispose } = initVimMode.mock.results[0].value

    fireEvent.click(screen.getByLabelText('View'))
    await waitFor(() => {
      expect(dispose).toHaveBeenCalled()
    })
  })

  it('clicking view returns to AST view', async () => {
    global.fetch = mockFetch()

    render(<Providers><DocsPage prgmName="test%2Fmain.ct" /></Providers>)

    await waitFor(() => {
      expect(screen.queryByText('Loading...')).not.toBeInTheDocument()
    })

    // Switch to edit mode
    fireEvent.click(screen.getByLabelText('Edit'))
    await waitFor(() => {
      expect(screen.getByTestId('monaco-editor')).toBeInTheDocument()
    })

    // Switch back to view mode
    fireEvent.click(screen.getByLabelText('View'))
    await waitFor(() => {
      expect(screen.queryByTestId('monaco-editor')).not.toBeInTheDocument()
    })
  })

  it('shows no source editor when page has no source text', async () => {
    global.fetch = mockFetch({
      page: () => Promise.resolve({
        status: 200,
        json: () => Promise.resolve(pageWith({ pgSource: null })),
      }),
    })

    render(<Providers><DocsPage prgmName="test%2Fmain.ct" /></Providers>)

    await waitFor(() => {
      expect(screen.queryByText('Loading...')).not.toBeInTheDocument()
    })

    fireEvent.click(screen.getByLabelText('Edit'))

    await waitFor(() => {
      expect(screen.queryByText('Loading...')).not.toBeInTheDocument()
    })

    expect(screen.queryByTestId('monaco-editor')).not.toBeInTheDocument()
  })

  it('hides the edit toggle when the server is readonly', async () => {
    global.fetch = mockFetch()

    render(
      <Providers config={{ isLoaded: true, data: { readonly: true }, notes: [] }}>
        <DocsPage prgmName="test%2Fmain.ct" />
      </Providers>
    )

    await waitFor(() => {
      expect(screen.queryByText('Loading...')).not.toBeInTheDocument()
    })

    expect(screen.queryByLabelText('Edit')).not.toBeInTheDocument()
  })

  it('hides the edit toggle until the config has loaded', async () => {
    global.fetch = mockFetch()

    render(
      <Providers config={{ isLoaded: false, data: null, notes: [] }}>
        <DocsPage prgmName="test%2Fmain.ct" />
      </Providers>
    )

    await waitFor(() => {
      expect(screen.queryByText('Loading...')).not.toBeInTheDocument()
    })

    expect(screen.queryByLabelText('Edit')).not.toBeInTheDocument()
  })

  it('marks the buffer dirty once edited', async () => {
    global.fetch = mockFetch()

    await renderEditing('main = 43\n')

    expect(screen.getByTestId('save-status')).toHaveTextContent('unsaved')
  })

  it('Ctrl+S posts the edited source and refetches the page', async () => {
    global.fetch = mockFetch()

    await renderEditing('main = 43\n')

    expect(mocks.commands).toHaveLength(1)
    await act(async () => { await mocks.commands[0].handler() })

    expect(global.fetch).toHaveBeenCalledWith('/api/write?prgmName=test%2Fmain.ct', {
      method: 'POST',
      headers: { 'Content-Type': 'text/plain' },
      body: 'main = 43\n',
    })

    // page load, write, then the post-save refetch
    expect(global.fetch).toHaveBeenCalledTimes(3)
    expect(global.fetch).toHaveBeenLastCalledWith('/api/page?prgmName=test%2Fmain.ct')
    expect(screen.getByTestId('save-status')).toHaveTextContent('saved')
  })

  it('does not post when the buffer is unchanged', async () => {
    global.fetch = mockFetch()

    render(<Providers><DocsPage prgmName="test%2Fmain.ct" /></Providers>)
    await waitFor(() => {
      expect(screen.queryByText('Loading...')).not.toBeInTheDocument()
    })
    fireEvent.click(screen.getByLabelText('Edit'))
    await waitFor(() => {
      expect(screen.getByTestId('monaco-editor')).toBeInTheDocument()
    })

    await act(async () => { await mocks.commands[0].handler() })

    expect(global.fetch).toHaveBeenCalledTimes(1)
  })

  it('surfaces a failed save and keeps the buffer dirty', async () => {
    global.fetch = mockFetch({
      write: () => Promise.resolve({
        status: 200,
        json: () => Promise.resolve({
          tag: 'ResFail',
          contents: [{ tp: 'CNoteError', msg: 'Saving is disabled because the doc server is running with --readonly' }],
        }),
      }),
    })

    await renderEditing('main = 43\n')
    await act(async () => { await mocks.commands[0].handler() })

    expect(screen.getByTestId('save-status')).toHaveTextContent('save failed: Saving is disabled')
    // no refetch on failure
    expect(global.fetch).toHaveBeenCalledTimes(2)

    // still dirty, so a retry is possible
    await act(async () => { await mocks.commands[0].handler() })
    expect(global.fetch).toHaveBeenCalledTimes(3)
  })

  it('registers a vim :w ex-command that saves', async () => {
    global.fetch = mockFetch()

    await renderEditing('main = 43\n')

    expect(VimMode.Vim.defineEx).toHaveBeenCalledWith('write', 'w', expect.any(Function))

    const writeEx = VimMode.Vim.defineEx.mock.calls[0][2]
    await act(async () => { await writeEx() })

    expect(global.fetch).toHaveBeenCalledWith('/api/write?prgmName=test%2Fmain.ct', expect.objectContaining({
      method: 'POST',
      body: 'main = 43\n',
    }))
  })
})

// The pages either side of this one come with the page, so the buttons say what the server said
// rather than what a list of every page held in the app works out.
describe('page navigation', () => {
  async function renderPage(nav) {
    global.fetch = mockFetch({
      page: () => Promise.resolve({ status: 200, json: () => Promise.resolve(pageWith(nav)) }),
    })
    render(<Providers><DocsPage prgmName="test%2Fmain.ct" /></Providers>)
    await waitFor(() => {
      expect(screen.queryByText('Loading...')).not.toBeInTheDocument()
    })
  }

  it('opens the pages either side of the one being read', async () => {
    await renderPage({ pgPrev: '/test/id.ct', pgNext: '/test/utils.ct' })

    expect(screen.getByText('Previous').closest('a')).toHaveAttribute('href', '/docs/%2Ftest%2Fid.ct')
    expect(screen.getByText('Next').closest('a')).toHaveAttribute('href', '/docs/%2Ftest%2Futils.ct')
  })

  it('offers only the side there is a page on', async () => {
    await renderPage({ pgPrev: null, pgNext: '/test/utils.ct' })

    expect(screen.queryByText('Previous')).not.toBeInTheDocument()
    expect(screen.getByText('Next')).toBeInTheDocument()
  })

  it('offers nothing at the ends of the reading order', async () => {
    await renderPage({ pgPrev: null, pgNext: null })

    expect(screen.queryByText('Previous')).not.toBeInTheDocument()
    expect(screen.queryByText('Next')).not.toBeInTheDocument()
  })

  // The buttons arrive with the page rather than ahead of it, so there is nothing to offer while it
  // is still being compiled -- and nothing that could point at the wrong page's neighbours.
  it('offers nothing while the page is loading', () => {
    global.fetch = vi.fn(() => new Promise(() => {}))
    render(<Providers><DocsPage prgmName="test%2Fmain.ct" /></Providers>)

    expect(screen.getByText('Loading...')).toBeInTheDocument()
    expect(screen.queryByText('Previous')).not.toBeInTheDocument()
    expect(screen.queryByText('Next')).not.toBeInTheDocument()
  })
})

describe('PlayButton', () => {
  // The page fixture's one declaration is `main = 42`, whose meta carries an Eval mode, which is
  // what makes the play button render at all.
  const PAGE = '/test/main.ct'

  function LocationDisplay() {
    const location = useLocation()
    return <div data-testid="location">{location.pathname + location.search}</div>
  }

  it('debugs the declaration as an annotation scoped to the page', async () => {
    global.fetch = mockFetch()

    render(
      <Providers>
        <DocsPage prgmName={PAGE} />
        <LocationDisplay />
      </Providers>
    )

    await waitFor(() => {
      expect(screen.queryByText('Loading...')).not.toBeInTheDocument()
    })

    fireEvent.click(screen.getByTestId('PlayArrowIcon'))
    fireEvent.click(screen.getByText('Debug'))

    // The only way to the debugger is this link, and what it names is an annotation evaluated
    // against this page -- not a route that fetches a run for itself.
    expect(decodeURIComponent(screen.getByTestId('location').textContent))
      .toBe(`/annot?annot=#debugger(fun= "main")&prgmName=${PAGE}`)
  })

  // A print is shown as the value it printed, so the computation behind it is nowhere on screen to
  // click. The page carries that computation's source, which is what makes a debugger of it
  // offerable at all -- see printSources in WebDocs.hs.
  it('debugs the computation a print shows the result of', async () => {
    global.fetch = mockFetch()

    render(
      <Providers>
        <DocsPage prgmName={PAGE} />
        <LocationDisplay />
      </Providers>
    )

    await waitFor(() => {
      expect(screen.queryByText('Loading...')).not.toBeInTheDocument()
    })

    fireEvent.contextMenu(screen.getByText('hello world'))
    fireEvent.click(screen.getByText('Debugger'))

    // The print's computation, not the value it rendered to: `"hello world"` is the expression, and
    // `hello world` is what it printed.
    expect(decodeURIComponent(screen.getByTestId('location').textContent))
      .toBe(`/annot?annot=#debugExpr(expr= "\\"hello world\\"")&prgmName=${PAGE}`)
  })

  it('offers no debugger on an annotation that is not a print', async () => {
    global.fetch = mockFetch()

    render(<Providers><DocsPage prgmName={PAGE} /></Providers>)

    await waitFor(() => {
      expect(screen.queryByText('Loading...')).not.toBeInTheDocument()
    })

    // The declaration beside the print carries no print source, so right-clicking it offers the
    // browser's own menu rather than one item that could only fail.
    fireEvent.contextMenu(screen.getByText('main'))
    expect(screen.queryByText('Debugger')).not.toBeInTheDocument()
  })
})

// A value shown on a page can edit the expression it was compiled from. The page names that
// expression by its meta id and saves it back through the parser which read the file, which is what
// makes a task of an Obsidian note a checkbox to check rather than a declaration to read.
describe('editing a value on a page', () => {
  // A page is named by the path it is opened at, which for a note is wherever the vault put it --
  // spaces and all. So the name is encoded into the request rather than pasted into it.
  const NOTE = '/vault/Read these/Tasks.md'
  const TASK_ID = '22222222-2222-5222-9222-222222222222'
  const CHECKED = '/Data/Note/Task/Simple(contents= "Read it", state= /Data/Primitive/True)'

  function noteFetch(overrides = {}) {
    return mockFetch({ ...overrides, page: () => Promise.resolve({
      status: 200,
      json: () => Promise.resolve(notePageResponse),
    }) })
  }

  async function renderNote(props = {}) {
    const rendered = render(<Providers {...props}><DocsPage prgmName={NOTE} /></Providers>)
    await waitFor(() => {
      expect(screen.getByRole('checkbox')).toBeInTheDocument()
    })
    return rendered
  }

  // The page's own declaration is a value like its tasks are, and displays as the markup the note
  // library says a page looks like: a heading of the title it is known by. The absent link is what
  // says the syntax is gone -- a type name is otherwise rendered as a link to its page -- and the
  // absent frame is what says the display is the program's own writing rather than somebody else's.
  it('heads the note with the title it is titled by', async () => {
    global.fetch = noteFetch()

    const { container } = await renderNote()

    expect(screen.getByRole('heading', { level: 1, name: 'Tasks' })).toBeInTheDocument()
    expect(screen.queryByRole('link', { name: '/Data/Note/Page' })).toBeNull()
    expect(container.querySelector('iframe')).toBeNull()
  })

  // The task is a sub-statement of the page it is declared in, so this is also what proves an
  // update reaches one.
  it('writes the expression the value names back to the page', async () => {
    global.fetch = noteFetch()

    await renderNote()
    fireEvent.click(screen.getByRole('checkbox'))

    await waitFor(() => {
      expect(global.fetch).toHaveBeenCalledWith(
        `/api/write-expr?prgmName=${encodeURIComponent(NOTE)}&metaId=${TASK_ID}`,
        expect.objectContaining({ method: 'POST', body: CHECKED }),
      )
    })
  })

  it('reads the page again once the value is written', async () => {
    global.fetch = noteFetch()

    await renderNote()
    const before = global.fetch.mock.calls.filter(([url]) => url.startsWith('/api/page')).length
    fireEvent.click(screen.getByRole('checkbox'))

    await waitFor(() => {
      const after = global.fetch.mock.calls.filter(([url]) => url.startsWith('/api/page')).length
      expect(after).toBe(before + 1)
    })
    expect(screen.getByTestId('save-status')).toHaveTextContent('saved')
  })

  it('reports a write the server refused', async () => {
    global.fetch = noteFetch({
      write: () => Promise.resolve({
        status: 200,
        json: () => Promise.resolve({
          tag: 'ResFail',
          contents: [{ tp: 'CNoteError', msg: 'The page can not be written back as obsidian' }],
        }),
      }),
    })

    await renderNote()
    fireEvent.click(screen.getByRole('checkbox'))

    await waitFor(() => {
      expect(screen.getByTestId('save-status'))
        .toHaveTextContent('save failed: The page can not be written back as obsidian')
    })
  })

  // A doc server nobody may write to shows the task, and does not offer to change it.
  it('shows the value but does not edit it when the server is readonly', async () => {
    global.fetch = noteFetch()

    await renderNote({ config: { isLoaded: true, data: { readonly: true }, notes: [] } })

    expect(screen.getByRole('textbox')).toHaveValue('Read it')
    expect(screen.getByRole('checkbox')).toBeDisabled()
    fireEvent.click(screen.getByRole('checkbox'))
    expect(global.fetch).not.toHaveBeenCalledWith(
      expect.stringContaining('/api/write-expr'), expect.anything())
  })
})

// A page which says it is written over another one is laid out around it: the writing above, the
// page it is written over below. The page here is an Obsidian note, which is where an overlay comes
// from without anyone writing one -- read out of the properties the note opens with.
describe('a page written over another page', () => {
  const NOTE = '/vault/Overlay.md'
  const URL = 'https://catln.dev'

  function overlayFetch() {
    return mockFetch({ page: () => Promise.resolve({
      status: 200,
      json: () => Promise.resolve(overlayNotePageResponse),
    }) })
  }

  async function renderOverlay() {
    render(<Providers><DocsPage prgmName={NOTE} /></Providers>)
    await waitFor(() => {
      expect(screen.getByTestId('overlay-split')).toBeInTheDocument()
    })
  }

  beforeEach(() => {
    window.localStorage.clear()
  })

  it('frames the page the note is written over', async () => {
    global.fetch = overlayFetch()

    await renderOverlay()

    expect(screen.getByTitle(`Overlay of ${URL}`)).toHaveAttribute('src', URL)
    expect(screen.getByRole('rowheader', { name: 'url' })).toBeInTheDocument()
  })

  // The annotation is a statement of the page as well as the shape the page is laid out in, so it
  // is shown twice -- but only one of the two is a frame.
  it('frames it once, though the annotation is shown in the writing too', async () => {
    global.fetch = overlayFetch()

    const { container } = render(<Providers><DocsPage prgmName={NOTE} /></Providers>)
    await waitFor(() => {
      expect(screen.getByTestId('overlay-split')).toBeInTheDocument()
    })

    expect(container.querySelectorAll('iframe')).toHaveLength(1)
    expect(screen.getAllByRole('link', { name: URL }).length).toBeGreaterThan(0)
  })

  // The point of settling the mode before the layout: the frame is a sibling of the writing rather
  // than a part of it, so writing notes against a page does not reload the page they are about.
  it('keeps the very same frame when the writing is opened for editing', async () => {
    global.fetch = overlayFetch()

    await renderOverlay()
    const framed = screen.getByTitle(`Overlay of ${URL}`)

    fireEvent.click(screen.getByLabelText('Edit'))

    await waitFor(() => {
      expect(screen.getByTestId('monaco-editor')).toBeInTheDocument()
    })
    expect(screen.getByTitle(`Overlay of ${URL}`)).toBe(framed)
  })

  it('opens the divider where the reader last left it', async () => {
    window.localStorage.setItem('catln.webdocs.overlaySplit', '0.7')
    global.fetch = overlayFetch()

    await renderOverlay()

    expect(screen.getByRole('separator')).toHaveAttribute('aria-valuenow', '70')
  })

  it('lays out a page which is written over nothing as it always was', async () => {
    global.fetch = mockFetch()

    const { container } = render(<Providers><DocsPage prgmName="test%2Fmain.ct" /></Providers>)
    await waitFor(() => {
      expect(screen.queryByText('Loading...')).not.toBeInTheDocument()
    })

    expect(screen.queryByTestId('overlay-split')).not.toBeInTheDocument()
    expect(container.querySelector('iframe')).toBeNull()
  })
})

describe('AnnotPage', () => {
  // What /api/annot returns: a one-statement program whose annotation carries the value the
  // compiler evaluated it to, on the expression meta's third slot.
  function annotResponse(modeName) {
    const value = {
      tag: 'TupleVal',
      name: '/Catln/Doc/Show/ListProgram',
      args: {
        '/mode': { tag: 'TupleVal', name: modeName, args: {} },
        '/prgm': { tag: 'StrVal', contents: JSON.stringify({ prgmObjMap: { omItems: {} }, prgmCg: [{}, {}], prgmAnnots: [] }) },
      },
    }
    const expr = { tag: 'RawValue', rvMeta: { getMetaPos: null, getMetaDat: [null, null, value], getMetaId: 1 }, rvName: '#listProgram',
    }
    return { tag: 'Success', contents: [{ rprgmImports: [], rprgmStatements: [{ rstStatement: { tag: 'RawAnnot', ranAnnot: expr }, rstChildren: [] }] }, []] }
  }

  beforeEach(() => {
    global.fetch = vi.fn(url => Promise.resolve({
      status: 200,
      json: () => Promise.resolve(
        url.startsWith('/api/annot')
          ? annotResponse(url.includes('TreeBuild') ? '/Catln/Doc/TreeBuild' : '/Catln/Doc/Typecheck')
          : { tag: 'Success', contents: [null, []] }
      ),
    }))
  })

  it('recompiles the annotation when the value updates it', async () => {
    render(<Providers><AnnotPage annot="#listProgram(mode= /Catln/Doc/Typecheck)" /></Providers>)

    await waitFor(() => {
      expect(screen.getByLabelText('List')).toBeChecked()
    })

    fireEvent.click(screen.getByLabelText('TreeBuild'))

    await waitFor(() => {
      expect(global.fetch).toHaveBeenCalledWith(
        '/api/annot?annot=%23listProgram(mode%3D%20%2FCatln%2FDoc%2FTreeBuild)')
    })
    // The radio follows the recompiled annotation, not a click on itself.
    await waitFor(() => {
      expect(screen.getByLabelText('TreeBuild')).toBeChecked()
    })
  })

  // An annotation asked for on its own is shown as what it compiled to. An overlay compiled on its
  // own has no writing to be written over anything, so there is nothing to lay out around a frame
  // and what it names is shown as the address it is.
  it('shows an overlay asked for on its own as the address it names', async () => {
    const value = {
      tag: 'TupleVal',
      name: '/Catln/Doc/Show/Overlay',
      args: { '/url': { tag: 'StrVal', contents: 'https://catln.dev' } },
    }
    const expr = { tag: 'RawValue', rvMeta: { getMetaPos: null, getMetaDat: [null, null, value], getMetaId: 1 }, rvName: '#overlay',
    }
    global.fetch = vi.fn(() => Promise.resolve({
      status: 200,
      json: () => Promise.resolve({ tag: 'Success', contents: [{ rprgmImports: [], rprgmStatements: [{ rstStatement: { tag: 'RawAnnot', ranAnnot: expr }, rstChildren: [] }] }, []] }),
    }))

    const { container } = render(
      <Providers><AnnotPage annot={'#overlay(url= "https://catln.dev")'} /></Providers>,
    )

    await waitFor(() => {
      expect(screen.getByRole('link', { name: 'https://catln.dev' })).toBeInTheDocument()
    })
    expect(container.querySelector('iframe')).toBeNull()
    expect(screen.queryByTestId('overlay-split')).not.toBeInTheDocument()
  })
})

// A link to a place in a page's source, which is what a debugger's ctrl-click on a call's book is:
// the page opens on its source rather than on what it reads as, with the place selected.
describe('DocsPage opened at a place in its source', () => {
  beforeEach(() => {
    mocks.selections.length = 0
    mocks.reveals.length = 0
    mocks.options.length = 0
    initVimMode.mockReturnValue({ dispose: vi.fn() })
    global.fetch = mockFetch()
  })

  // Returns a navigate, for following a second link without leaving the page.
  function renderAt(entry, config = writableConfig) {
    let navigate
    function Navigator() {
      navigate = useNavigate()
      return null
    }
    render(
      <ThemeProvider theme={theme}>
        <ConfigContext.Provider value={config}>
          <MemoryRouter initialEntries={[entry]}>
            <DocsPage prgmName="test%2Fmain.ct" />
            <Navigator />
          </MemoryRouter>
        </ConfigContext.Provider>
      </ThemeProvider>
    )
    return path => act(() => navigate(path))
  }

  const PLACE = { startLineNumber: 2, startColumn: 3, endLineNumber: 2, endColumn: 9 }

  it('opens on the source with the place selected and in view', async () => {
    renderAt('/docs/test%2Fmain.ct?line=2&col=3&endLine=2&endCol=9')

    expect(await screen.findByTestId('monaco-editor')).toBeInTheDocument()
    await waitFor(() => expect(mocks.selections).toContainEqual(PLACE))
    expect(mocks.reveals).toContainEqual(PLACE)
  })

  it('selects the whole line when only the line is named', async () => {
    renderAt('/docs/test%2Fmain.ct?line=2')

    await screen.findByTestId('monaco-editor')
    await waitFor(() => expect(mocks.selections).toContainEqual({ startLineNumber: 2, startColumn: 1, endLineNumber: 2, endColumn: 1 }))
  })

  // The source was opened by the link, so it is the link's: arriving at the page without a place goes
  // back to what it reads as.
  it('goes back to reading the page when it is arrived at without a place', async () => {
    const go = renderAt('/docs/test%2Fmain.ct?line=2')
    await screen.findByTestId('monaco-editor')

    await go('/docs/test%2Fmain.ct')
    await waitFor(() => expect(screen.queryByTestId('monaco-editor')).not.toBeInTheDocument())
  })

  it('opens on what the page reads as when no place is named', async () => {
    renderAt('/docs/test%2Fmain.ct')

    await waitFor(() => expect(screen.queryByText('Loading...')).not.toBeInTheDocument())
    expect(screen.queryByTestId('monaco-editor')).not.toBeInTheDocument()
  })

  // A server that cannot write has no edit mode to offer, and a link to a place still opens the
  // source: read, since there is nowhere for an edit to go, and with a way back to the page.
  it('opens the source read-only on a server that cannot write', async () => {
    renderAt('/docs/test%2Fmain.ct?line=2', { isLoaded: true, data: { readonly: true }, notes: [] })

    await screen.findByTestId('monaco-editor')
    expect(mocks.options[mocks.options.length - 1].readOnly).toBe(true)
    expect(screen.getByLabelText('View')).toBeInTheDocument()
  })
})
