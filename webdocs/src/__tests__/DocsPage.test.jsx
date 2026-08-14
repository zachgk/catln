import React from 'react'
import { render, screen, waitFor, fireEvent, act } from '@testing-library/react'
import { MemoryRouter, useLocation } from 'react-router-dom'
import { ThemeProvider, createTheme } from '@mui/material/styles'
import { ConfigContext } from '../Common/Common'
import { initVimMode, VimMode } from 'monaco-vim'
import DocsPage, { AnnotPage } from '../DocsPage'
import pageResponse from '../test/fixtures/page-response.json'
import notePageResponse from '../test/fixtures/note-page-response.json'

// Collects the keybindings DocsPage registers, so tests can fire Ctrl+S directly.
const mocks = vi.hoisted(() => ({ commands: [] }))

vi.mock('@monaco-editor/react', () => ({
  default: ({ value, onChange, onMount }) => {
    React.useEffect(() => {
      onMount(
        {
          onDidDispose: () => {},
          addCommand: (keybinding, handler) => mocks.commands.push({ keybinding, handler }),
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
    expect(screen.getByTestId('monaco-editor')).toHaveValue('main = 42\n')
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
})

// A value shown on a page can edit the expression it was compiled from. The page names that
// expression by its meta id and saves it back through the parser which read the file, which is what
// makes a task of an Obsidian note a checkbox to check rather than a declaration to read.
describe('editing a value on a page', () => {
  // A page is named by the path it is opened at, which for a note is wherever the vault put it --
  // spaces and all. So the name is encoded into the request rather than pasted into it.
  const NOTE = '/vault/Read these/Tasks.md'
  const TASK_ID = '22222222-2222-5222-9222-222222222222'
  const CHECKED = '/Data/Note/Task(contents= "Read it", state= /Data/Primitive/True)'

  function noteFetch(overrides = {}) {
    return mockFetch({ ...overrides, page: () => Promise.resolve({
      status: 200,
      json: () => Promise.resolve(notePageResponse),
    }) })
  }

  async function renderNote(props = {}) {
    render(<Providers {...props}><DocsPage prgmName={NOTE} /></Providers>)
    await waitFor(() => {
      expect(screen.getByRole('checkbox')).toBeInTheDocument()
    })
  }

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

describe('AnnotPage', () => {
  // What /api/annot returns: a one-statement program whose annotation carries the value the
  // compiler evaluated it to, on the expression meta's third slot.
  function annotResponse(modeName) {
    const value = {
      tag: 'TupleVal',
      name: '/Catln/Doc/Show/ListProgram',
      args: {
        '/mode': { tag: 'TupleVal', name: modeName, args: {} },
        '/prgm': { tag: 'StrVal', contents: JSON.stringify({ prgmObjMap: {}, prgmCG: [{}, {}] }) },
      },
    }
    const expr = {
      tag: 'RawValue',
      contents: [{ getMetaPos: null, getMetaDat: [null, null, value], getMetaID: 1 }, '#listProgram'],
    }
    return { tag: 'Success', contents: [[[], [[{ tag: 'RawAnnot', contents: expr }, []]]], []] }
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
})
