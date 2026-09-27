import React from 'react'
import { cleanup, fireEvent, render, screen, waitFor, within } from '@testing-library/react'
import userEvent from '@testing-library/user-event'
import { MemoryRouter } from 'react-router-dom'
import { ThemeProvider, createTheme } from '@mui/material/styles'
import Debugger from '../Debugger'
import debuggerResponse from '../../../test/fixtures/debugger-response.json'
import fmapResponse from '../../../test/fixtures/debugger-fmap-response.json'
import mmapResponse from '../../../test/fixtures/debugger-mmap-response.json'
import foldlResponse from '../../../test/fixtures/debugger-foldl-response.json'
import foldrResponse from '../../../test/fixtures/debugger-foldr-response.json'
import mapValuesResponse from '../../../test/fixtures/debugger-mapvalues-response.json'
import foldEntriesResponse from '../../../test/fixtures/debugger-foldentries-response.json'
import syntaxResponse from '../../../test/fixtures/debugger-syntax-response.json'

const theme = createTheme()

// What starts the generic label of a call beneath another: the book behind which its definition is.
// Beneath, because a call with a display of its own heads its display with a book too.
const GENERIC_LABEL = '[role=group] .catln-debug-source'

// The real payload the compiler produced for a page whose main prints "Hello, world" and exits 0,
// narrowed to the fields the page reads: erCoverage is real but unread, and was half the bytes.
const FUN = '/main'

// The args of a compiled Show/Debugger: the run, as a JSON string, and the function it ran.
function debug(run = debuggerResponse) {
  return {
    '/run': { tag: 'StrVal', contents: JSON.stringify(run) },
    '/fun': { tag: 'StrVal', contents: FUN },
  }
}

function Providers({ children }) {
  return (
    <ThemeProvider theme={theme}>
      <MemoryRouter>{children}</MemoryRouter>
    </ThemeProvider>
  )
}

describe('Debugger', () => {
  beforeEach(() => {
    global.fetch = vi.fn(() => Promise.reject(new Error('Debugger must not fetch')))
  })

  it('renders the run carried by the value without fetching', () => {
    render(<Providers><Debugger {...debug()} /></Providers>)

    expect(screen.getByRole('heading', { name: FUN })).toBeInTheDocument()
    // The root of the evaluation tree, which is what the page exists to show.
    expect(screen.getByRole('treeitem')).toBeInTheDocument()
    expect(global.fetch).not.toHaveBeenCalled()
  })

  it('renders the mock IO handle the run threads as ordinary IO', () => {
    render(<Providers><Debugger {...debug()} /></Providers>)

    // Running under a mock is how the output above was captured, so every /io in the tree is a
    // MockIOVal. Left unhandled it renders as "Unknown Val tag" and logs an error.
    expect(screen.queryByText(/Unknown Val tag/)).not.toBeInTheDocument()
    expect(screen.getAllByText(/IOVal/).length).toBeGreaterThan(0)
  })

  it('shows what the run printed and exited with', () => {
    render(<Providers><Debugger {...debug()} /></Providers>)

    // Captured through the mock IO handle rather than written to the doc server's stdout, which is
    // the only reason there is anything here to show.
    expect(screen.getByText('Hello, world')).toBeInTheDocument()
    expect(screen.getByText(/Exited with 0/)).toBeInTheDocument()
  })

  it('says so when the run printed nothing and entered no arrow', () => {
    const empty = { drExit: 1, drOutput: '', drValue: null, drResult: { erCoverage: {}, erTreebug: [], erDebugs: [] } }
    render(<Providers><Debugger {...debug(empty)} /></Providers>)

    expect(screen.getByText(/Evaluated no arrows/)).toBeInTheDocument()
    expect(screen.getByText(/Printed nothing/)).toBeInTheDocument()
    // A run records only what a #debug asks for, so a run without one has no Debug section at all.
    expect(screen.queryByRole('heading', { name: 'Debug' })).not.toBeInTheDocument()
    expect(screen.getByText(/Exited with 1/)).toBeInTheDocument()
    expect(screen.queryByRole('treeitem')).not.toBeInTheDocument()
  })

  // The #debug records of the run, which is the other direction through it: the tree says what was
  // evaluated, and these say what a value was at a point the program named.
  it('lists what the run recorded, in the order it recorded them', () => {
    render(<Providers><Debugger {...debug()} /></Providers>)

    expect(screen.getByRole('heading', { name: 'Debug' })).toBeInTheDocument()
    // Written on greet as `#debug(d= name)` then `#debug(d= "greeting")`, so they read down the
    // definition rather than back up it.
    const records = screen.getAllByRole('listitem')
    expect(records).toHaveLength(2)
    expect(records[0]).toHaveTextContent('scratch-debugger.ct:5')
    expect(records[0]).toHaveTextContent('world')
    expect(records[1]).toHaveTextContent('scratch-debugger.ct:6')
    expect(records[1]).toHaveTextContent('greeting')
  })

  it('reveals the call a record was recorded in', async () => {
    const user = userEvent.setup()
    render(<Providers><Debugger {...debug()} /></Providers>)

    // Only the root is on screen to begin with: the call the records came from is inside it, which
    // is the whole reason a record leads anywhere rather than naming a node the reader must find.
    expect(screen.getAllByRole('treeitem')).toHaveLength(1)

    const record = screen.getAllByRole('listitem')[0]
    await user.click(within(record).getByRole('button'))

    const items = screen.getAllByRole('treeitem')
    expect(items).toHaveLength(2)
    expect(items[1]).toHaveAttribute('aria-selected', 'true')
  })

  // A call's line is what it was given and what it gave back. The definition it was a call of is
  // where it is written, behind a book at the start of the line.
  it('shows a call as its input and output, with its definition behind a book', () => {
    const { container } = render(<Providers><Debugger {...debug()} /></Providers>)

    const label = container.querySelector('.MuiTreeItem-label')
    expect(label.textContent).not.toContain('|::|')
    expect(label.textContent).toMatch(/^\/ContextIn\(.*\) => /)
    expect(within(label).getByRole('link', { name: 'Source at scratch-debugger.ct:9' })).toBeInTheDocument()
  })

  // What a ctrl-click follows, which is a link's own: the page, opened on its source at the call.
  it('links the book to the page the definition is written in, at where it is written', () => {
    const { container } = render(<Providers><Debugger {...debug()} /></Providers>)

    const book = within(container.querySelector('.MuiTreeItem-label')).getByRole('link')
    const [path, query] = book.getAttribute('href').split('?')
    expect(decodeURIComponent(path)).toBe('/docs//data/projects/catln/.claude/worktrees/web/scratch-debugger.ct')
    expect(Object.fromEntries(new URLSearchParams(query))).toEqual({ line: '9', col: '1', endLine: '9', endCol: '67' })
  })

  describe('the source behind a book', () => {
    // The page the fixture's run was made in, as far down as the call the root is of. Only what the
    // panel reads is here: the lines the root's span is on.
    // Line 9 is written out to the width the root's span has -- columns 1 up to 67 -- and something
    // after, so that where the mark ends is what is being checked rather than where the line does.
    const CALL = 'main{io}= ContextIn(value= greet(name= "world"), io= io).exit(v=0)'
    const SOURCE = [1, 2, 3, 4, 5, 6, 7, 8].map(n => `# line ${n}`).join('\n') + `\n${CALL} # after\n`

    const FILE = '/data/projects/catln/.claude/worktrees/web/scratch-debugger.ct'

    // The run as the server sends it, with the text of the file it was written in. The fixture was
    // captured before runs carried their sources.
    const withSources = (sources = { [FILE]: SOURCE }) => ({ ...debuggerResponse, drSources: sources })

    async function openRootSource(user, run = withSources()) {
      const { container } = render(<Providers><Debugger {...debug(run)} /></Providers>)
      const label = container.querySelector('.MuiTreeItem-label')
      await user.click(within(label).getByRole('link'))
      return label
    }

    it('opens the lines the call is written on, with the call marked', async () => {
      const user = userEvent.setup()
      await openRootSource(user)

      const panel = await screen.findByRole('dialog', { name: 'Source' })
      expect(panel).toHaveTextContent('scratch-debugger.ct:9')
      expect(CALL).toHaveLength(66)
      expect(panel.querySelector('mark').textContent).toBe(CALL)
      expect(panel).toHaveTextContent('# after')
      // Out of the run, which is why the Debugger's beforeEach rejecting every fetch still passes.
      expect(global.fetch).not.toHaveBeenCalled()
    })

    // A file the run came without is said to be missing, and the call is shown as the definition it
    // was a call of -- not as a source the panel went and guessed at.
    it('says when the run came without the source of a call', async () => {
      const user = userEvent.setup()
      await openRootSource(user, withSources({}))

      const panel = await screen.findByRole('dialog', { name: 'Source' })
      expect(panel).toHaveTextContent(`No source for ${FILE} came with the run.`)
      expect(panel.querySelector('mark')).toBeNull()
      expect(panel).toHaveTextContent('/main')
    })

    // Opened over the rows rather than over the page: the tree, and the line it came from, are still
    // there to read, and a plain click does not follow the book's link or toggle its row.
    it('leaves the tree and the line it was opened from as they were', async () => {
      const user = userEvent.setup()
      const label = await openRootSource(user)

      await screen.findByRole('dialog', { name: 'Source' })
      expect(label).toHaveTextContent('=> ')
      expect(screen.getAllByRole('treeitem')).toHaveLength(1)
      expect(screen.getByRole('treeitem')).toHaveAttribute('aria-expanded', 'false')
    })

    it('closes from its own button, and from the book that opened it', async () => {
      const user = userEvent.setup()
      const label = await openRootSource(user)

      await user.click(await screen.findByRole('button', { name: 'Close source' }))
      await waitFor(() => expect(screen.queryByRole('dialog')).not.toBeInTheDocument())

      const book = within(label).getByRole('link')
      await user.click(book)
      expect(await screen.findByRole('dialog', { name: 'Source' })).toBeInTheDocument()
      await user.click(book)
      await waitFor(() => expect(screen.queryByRole('dialog')).not.toBeInTheDocument())
    })

    it('leaves a ctrl-click to the link rather than opening the panel', async () => {
      const { container } = render(<Providers><Debugger {...debug()} /></Providers>)
      const book = within(container.querySelector('.MuiTreeItem-label')).getByRole('link')

      const followed = fireEvent.click(book, { ctrlKey: true })
      expect(followed).toBe(true)
      expect(screen.queryByRole('dialog')).not.toBeInTheDocument()
    })
  })

  // A run of one expression (#debugExpr) rather than of a program: it was given no IO handle to
  // print with, so what it ended with is a value and there is no exit code to report.
  it('shows the value an expression run produced instead of an exit code', () => {
    const expr = {
      drExit: null,
      drOutput: '',
      drValue: { tag: 'IntVal', contents: 3 },
      drResult: { erCoverage: {}, erTreebug: [], erDebugs: [] },
    }
    render(<Providers><Debugger {...debug(expr)} /></Providers>)

    // Inline in the heading, as input => output the way each call in the tree is labelled, rather
    // than under a heading of its own.
    expect(screen.queryByRole('heading', { name: 'Value' })).not.toBeInTheDocument()
    const heading = screen.getByRole('heading', { level: 2 })
    expect(heading).toHaveTextContent(/=>\s*3$/)
    expect(screen.queryByText(/Exited with/)).not.toBeInTheDocument()
    expect(screen.queryByText(/Printed nothing/)).not.toBeInTheDocument()
  })

  // A program run has no value to put in its heading; what it printed and how it exited comes straight
  // under the heading instead, before the tree of how the run got there.
  it("shows a program run's output under the heading, above the tree", () => {
    render(<Providers><Debugger {...debug()} /></Providers>)
    expect(screen.getByRole('heading', { level: 2 })).toHaveTextContent(/^\/main$/)
    const output = screen.getByRole('heading', { name: 'Output' })
    const tree = screen.getByRole('tree')
    expect(output.compareDocumentPosition(tree) & Node.DOCUMENT_POSITION_FOLLOWING).toBeTruthy()
  })
})

// A real run of a mapping over a list, as the compiler produced it, with each arrow reduced to the
// name it is written under and each value to a small one -- the shape of the tree is the whole point
// of this fixture and the metas were most of its bytes.
//
// It is `incAll(lst= [1, 2, 3])` where `incAll(lst)= fmap(lst= lst, fn(v)= v + 1)`, so the run holds
// four nested calls of fmap (one per element and the Nil that ends it), each with the one call of the
// mapped function beside it, and the outermost carries the Show/Steps that
// `stack/core/list-webdocs.ct` builds for it.
describe('Debugger over a call with a display of its own', () => {
  beforeEach(() => {
    global.fetch = vi.fn(() => Promise.reject(new Error('Debugger must not fetch')))
  })

  function fmapRun() {
    return {
      '/run': { tag: 'StrVal', contents: JSON.stringify(fmapResponse) },
      '/fun': { tag: 'StrVal', contents: 'incAll(lst= [1, 2, 3])' },
    }
  }

  // The mapping is the second call of the run, and a collapsed item keeps nothing in the page, so
  // every assertion below opens it first.
  async function openFmap() {
    render(<Providers><Debugger {...fmapRun()} /></Providers>)
    const roots = screen.getAllByRole('treeitem')
    await userEvent.click(within(roots[1]).getByText('/Data/Functor/fmap'))
    return roots[1]
  }

  it('renders every call the run entered at the top, not only the first', () => {
    render(<Providers><Debugger {...fmapRun()} /></Providers>)

    // The expression was built as two calls in sequence, so the run has two roots. Showing one of
    // them would hide the mapping entirely, which is the whole of what this run is about.
    const roots = screen.getAllByRole('treeitem')
    expect(roots).toHaveLength(2)
    expect(roots[0].textContent).toContain('/Test/incAll')
    expect(roots[1].textContent).toContain('/Data/Functor/fmap')
  })

  it('heads a shown call with its name rather than the whole definition', async () => {
    render(<Providers><Debugger {...fmapRun()} /></Providers>)

    // The display below says what the call did, so the definition it was is a heading. The call
    // beside it has nothing below it and keeps the generic label, of what it was given and gave back.
    const roots = screen.getAllByRole('treeitem')
    expect(within(roots[1]).getByText('/Data/Functor/fmap')).toBeInTheDocument()
    expect(roots[1].querySelector(GENERIC_LABEL)).toBeNull()
  })

  it('shows the call as its table rather than as the generic label', async () => {
    const fmap = await openFmap()

    // Every row of what the mapping did, which is what the display exists to say.
    expect(fmap.textContent).toContain('1')
    expect(fmap.textContent).toContain('4')
    expect(screen.getAllByText('↦').length).toBe(3)
    // The generic label of a node -- source, input, result -- is what the table replaced, for this
    // call. The call beside it has no display of its own and keeps that label, which is the whole of
    // what "a call with no arm is shown the way everything else is" means.
    expect(fmap.querySelector(GENERIC_LABEL)).toBeNull()
    const others = screen.getAllByRole('treeitem').filter(item => !item.contains(fmap) && !fmap.contains(item))
    expect(others.some(item => /=> /.test(item.querySelector('.MuiTreeItem-label').textContent))).toBe(true)
  })

  it('folds the chain of recursive calls into the one table', async () => {
    const fmap = await openFmap()

    // Four calls of fmap and one table: the inner three are folded into it. A table of a shorter tail
    // of the same mapping is not a second display, it is the same one said again.
    expect(screen.getAllByText('↦').length).toBe(3)
    expect(within(fmap).queryAllByText('/Data/Functor/fmap')).toHaveLength(1)
  })

  it('opens each row onto the call that produced it', async () => {
    const fmap = await openFmap()

    // One row per element, and EVERY one leads to the call of the mapped function for that element
    // rather than to the recursion the run happens to be written as. Every one, because a chain walk
    // that loses the thread still finds something for the first row and nothing for the rest -- which
    // is what a row-by-row check catches and a check of the first row does not.
    const rows = within(fmap).getAllByRole('treeitem')
    expect(rows).toHaveLength(3)
    for (const row of rows) {
      expect(row.querySelector('.MuiTreeItem-iconContainer svg')).not.toBeNull()
    }
    await userEvent.click(within(rows[2]).getByText('↦'))
    expect(within(rows[2]).getAllByRole('treeitem').length).toBeGreaterThan(0)
  })
})

// A real run of a binding over a list, reduced the same way as the mapping above. It is
// `/Data/Monad/mmap(m= [1, 2], fn(v)= [v, v])`, so the table is `1 ↦ [1, 1]` and `2 ↦ [2, 2]`: each
// element beside the list it contributed, which the joined result no longer says.
//
// Each level of the ConsList instance also calls append, and the run does not put every one of those
// under the level that asked for it: the last element's append is reduced after the binding has
// closed, so it is a root of its own beside the binding. What a row opens onto is whatever the level
// holds besides the next level, which is why only the first row has an append in it.
describe('Debugger over a binding, which shows as the same table', () => {
  beforeEach(() => {
    global.fetch = vi.fn(() => Promise.reject(new Error('Debugger must not fetch')))
  })

  function mmapRun() {
    return {
      '/run': { tag: 'StrVal', contents: JSON.stringify(mmapResponse) },
      '/fun': { tag: 'StrVal', contents: '/Data/Monad/mmap(m= [1, 2], fn(v)= [v, v])' },
    }
  }

  async function openMmap() {
    render(<Providers><Debugger {...mmapRun()} /></Providers>)
    const roots = screen.getAllByRole('treeitem')
    await userEvent.click(within(roots[0]).getByText('/Data/Monad/mmap'))
    return roots[0]
  }

  it('shows each element beside the list it contributed', async () => {
    const mmap = await openMmap()

    expect(within(mmap).getByText('[1, 1]')).toBeInTheDocument()
    expect(within(mmap).getByText('[2, 2]')).toBeInTheDocument()
    expect(screen.getAllByText('↦').length).toBe(2)
  })

  it('folds the chain of recursive calls into the one table', async () => {
    const mmap = await openMmap()

    expect(within(mmap).queryAllByText('/Data/Monad/mmap')).toHaveLength(1)
    expect(mmap.querySelector(GENERIC_LABEL)).toBeNull()
  })

  it('opens each row onto the call of the function for its element', async () => {
    const mmap = await openMmap()

    const rows = within(mmap).getAllByRole('treeitem')
    expect(rows).toHaveLength(2)
    for (const row of rows) {
      await userEvent.click(within(row).getByText('↦'))
      expect(within(row).getAllByText(/\/fn/).length).toBeGreaterThan(0)
    }
  })
})

// Real runs of a fold from each end, reduced as the mapping's is:
// `/Data/List/foldl(lst= [1, 2, 3], fn(acc, v)= acc + v, init= 0)`, whose rows are 0+1=1, 1+2=3 and
// 3+3=6, and `/Data/List/foldr(lst= ["a", "b", "c"], fn(acc, v)= acc ++ v, init= "")`, whose rows are
// cb+a=cba, c+b=cb and +c=c. Both are a chain of calls of the fold with one call of the step beside
// each link: after the next link in a left fold, and around it in a right one, where the step is
// handed what the rest of the list folded to.
describe('Debugger over a fold, which shows as its accumulator threaded through the rows', () => {
  beforeEach(() => {
    global.fetch = vi.fn(() => Promise.reject(new Error('Debugger must not fetch')))
  })

  function run(response, fun) {
    return {
      '/run': { tag: 'StrVal', contents: JSON.stringify(response) },
      '/fun': { tag: 'StrVal', contents: fun },
    }
  }

  async function openFold(response, name) {
    render(<Providers><Debugger {...run(response, name)} /></Providers>)
    const [root] = screen.getAllByRole('treeitem')
    await userEvent.click(within(root).getByText(name))
    return root
  }

  it('shows a left fold as its rows from init down to the result', async () => {
    const fold = await openFold(foldlResponse, '/Data/List/foldl')

    const rows = within(fold).getAllByRole('treeitem')
    expect(rows).toHaveLength(3)
    expect(rows[1].textContent).toMatch(/^11⊕2↦3↙/)
    expect(rows[2].textContent).toMatch(/^23⊕3↦6↓/)
    const lines = Array.from(fold.querySelectorAll('.catln-steps-row')).map(l => l.textContent)
    expect(lines[1]).toMatch(/^init.*0↙$/)
    expect(lines[lines.length - 1]).toMatch(/=6$/)
  })

  it('shows a right fold as its rows from init up to the result', async () => {
    const fold = await openFold(foldrResponse, '/Data/List/foldr')

    const rows = within(fold).getAllByRole('treeitem')
    expect(rows).toHaveLength(3)
    expect(rows[0].textContent).toMatch(/^0cb⊕a↦cba↑/)
    expect(rows[2].textContent).toMatch(/^2⊕c↦c↖/)
    const lines = Array.from(fold.querySelectorAll('.catln-steps-row')).map(l => l.textContent)
    expect(lines[1]).toMatch(/=cba$/)
    expect(lines[lines.length - 1]).toMatch(/^init.*↖$/)
  })

  it('folds the chain of recursive calls into the one table', async () => {
    const fold = await openFold(foldrResponse, '/Data/List/foldr')

    expect(within(fold).queryAllByText('/Data/List/foldr')).toHaveLength(1)
    expect(fold.querySelector(GENERIC_LABEL)).toBeNull()
  })

  it('opens each row onto the call of the step for its element', async () => {
    for (const [response, name] of [[foldlResponse, '/Data/List/foldl'], [foldrResponse, '/Data/List/foldr']]) {
      const fold = await openFold(response, name)
      const rows = within(fold).getAllByRole('treeitem')
      for (const row of rows) {
        await userEvent.click(within(row).getByText('↦'))
        expect(within(row).getAllByText(/\/fn/).length).toBeGreaterThan(0)
      }
      cleanup()
    }
  })
})

// Real runs over a map, reduced as the list's are. The map is {1: 10, 2: 20, 3: 30}, inserted 2
// first so that the root has an entry on each side:
// `/Data/Map/mapValues(m= sample, fn(v)= v + 1)`, whose rows are 10 ↦ 11, 20 ↦ 21 and 30 ↦ 31 in key
// order, and `/Data/Map/foldEntries(m= sample, fn(acc, k, v)= acc + v, init= 0)`, whose rows are
// 0+30=30, 30+20=50 and 50+10=60 in the descending order the fold visits the keys in.
//
// Neither is a chain. Each level calls itself on both subtrees -- mapValues on the left, then the
// function, then the right; foldEntries on the right, then the step, then the left -- so which call a
// row opens onto depends on the table reading the recursion as a tree, in the order it was made.
describe('Debugger over a call over a map, whose recursion is a tree', () => {
  beforeEach(() => {
    global.fetch = vi.fn(() => Promise.reject(new Error('Debugger must not fetch')))
  })

  function run(response, fun) {
    return {
      '/run': { tag: 'StrVal', contents: JSON.stringify(response) },
      '/fun': { tag: 'StrVal', contents: fun },
    }
  }

  async function openCall(response, name) {
    render(<Providers><Debugger {...run(response, name)} /></Providers>)
    const [root] = screen.getAllByRole('treeitem')
    await userEvent.click(within(root).getByText(name))
    return root
  }

  // What a row opens onto, without the row's own label.
  function opened(row) {
    const group = row.querySelector('[role=group]')
    return group ? group.textContent : ''
  }

  it('shows a mapping as a row per entry in key order, with the key in its own column', async () => {
    const call = await openCall(mapValuesResponse, '/Data/Map/mapValues')

    const rows = within(call).getAllByRole('treeitem')
    expect(rows).toHaveLength(3)
    expect(rows[0].textContent).toMatch(/^0110↦11/)
    expect(rows[2].textContent).toMatch(/^2330↦31/)
    expect(within(call).getByText('key')).toBeInTheDocument()
  })

  it('shows a fold over the entries from init down to the result, in the order it visited them', async () => {
    const call = await openCall(foldEntriesResponse, '/Data/Map/foldEntries')

    const rows = within(call).getAllByRole('treeitem')
    expect(rows).toHaveLength(3)
    expect(rows[0].textContent).toMatch(/^030⊕30↦30↙/)
    expect(rows[2].textContent).toMatch(/^2150⊕10↦60↓/)
    const lines = Array.from(call.querySelectorAll('.catln-steps-row')).map(l => l.textContent)
    expect(lines[1]).toMatch(/^init.*0↙$/)
    expect(lines[lines.length - 1]).toMatch(/=60$/)
  })

  it('folds both sides of the recursion into the one table', async () => {
    for (const [response, name] of [[mapValuesResponse, '/Data/Map/mapValues'], [foldEntriesResponse, '/Data/Map/foldEntries']]) {
      const call = await openCall(response, name)
      expect(within(call).queryAllByText(name)).toHaveLength(1)
      expect(call.querySelector(GENERIC_LABEL)).toBeNull()
      cleanup()
    }
  })

  it('opens each row onto the call of the function for its own entry', async () => {
    // What each call of the function returned, in the order of the rows. A walk that followed one
    // side of the tree would open the rows onto the wrong entries' calls, or onto none past the
    // first, so each row is opened and read for its own.
    for (const [response, name, outputs] of [
      [mapValuesResponse, '/Data/Map/mapValues', ['11', '21', '31']],
      [foldEntriesResponse, '/Data/Map/foldEntries', ['30', '50', '60']],
    ]) {
      const call = await openCall(response, name)
      const rows = within(call).getAllByRole('treeitem')
      for (const [i, row] of rows.entries()) {
        await userEvent.click(within(row).getAllByText('↦')[0])
        expect(opened(row)).toMatch(/\/fn/)
        expect(opened(row)).toContain(`=> ${outputs[i]}`)
        for (const other of outputs.filter(o => o !== outputs[i])) {
          expect(opened(row)).not.toContain(`=> ${other}`)
        }
      }
      cleanup()
    }
  })

  // A run read by the functions its page writes: debugNested.ct's `total(a= 1, b= 2)`, whose `total` is
  // written as intermediate lines (`sum`, `unused`), a nested `helper`, an #assert and a #debug, and a
  // result calling `scale`, `helper` and `sign`.
  describe('read by the functions its page writes', () => {
    function syntaxRun() {
      return {
        '/run': { tag: 'StrVal', contents: JSON.stringify(syntaxResponse) },
        '/fun': { tag: 'StrVal', contents: 'total(a= 1, b= 2)' },
      }
    }

    // The label of an item, without the items under it.
    function labelOf(item) {
      return item.querySelector(':scope > .MuiTreeItem-content .MuiTreeItem-label')
    }

    // What a label says, without where it is written: the file heading a block and the line number
    // beside each statement.
    function labelText(item) {
      const label = labelOf(item).cloneNode(true)
      label.querySelectorAll('.catln-debug-file, .catln-debug-lineno').forEach(e => e.remove())
      return label.textContent
    }

    // The item on screen whose label starts as given, or matches the given pattern.
    function itemOf(text) {
      const matches = label => typeof text === 'string' ? label.startsWith(text) : text.test(label)
      const found = screen.getAllByRole('treeitem').filter(item => matches(labelText(item)))
      expect(found).toHaveLength(1)
      return found[0]
    }

    async function open(user, item) {
      await user.click(labelOf(item))
    }

    it('opens on the function that was called, line by line with what each came to', () => {
      render(<Providers><Debugger {...syntaxRun()} /></Providers>)

      // One tree, starting from the call: the function as written, and what the call came to rather than
      // the operator+ its arrow rewrote to.
      expect(labelOf(itemOf('total(a'))).toHaveTextContent(/=> 63/)
      // Used five times, by the #assert, the #debug, the helper and the result, and 3 every time.
      expect(labelOf(itemOf('sum = a + b'))).toHaveTextContent(/=> 3 ×5$/)
      // Never used, so desugar never evaluated it.
      expect(labelOf(itemOf('unused'))).toHaveTextContent('not evaluated')
      // The helper is a function of its own, whose lines are shown in its calls rather than here.
      expect(labelOf(itemOf('helper(y'))).not.toHaveTextContent('not evaluated')
      expect(labelOf(itemOf('scale(v = helper'))).toHaveTextContent(/=> 63$/)
    })

    // The parts of the result line, as written, each under the part it is part of.
    function parts() {
      return [...document.querySelectorAll('.catln-debug-part')].map(p => ({
        text: p.textContent,
        depth: parseFloat(p.style.paddingLeft) || 0,
      }))
    }

    // The button a part of a line opens the call it made with.
    function opener(text) {
      const part = [...document.querySelectorAll('.catln-debug-part')].find(p => p.textContent.startsWith(text))
      return within(part).getByRole('button')
    }

    it('opens a line onto its parts as written, with what each came to', async () => {
      const user = userEvent.setup()
      render(<Providers><Debugger {...syntaxRun()} /></Providers>)

      await open(user, itemOf('scale(v = helper'))

      // `scale(v = helper(y = sum)) + sum * sign(x = a)`: the two sides of the +, and within each
      // what it is made of. The + itself is the line's own label.
      expect(parts()).toEqual([
        {text: expect.stringMatching(/^scale\(v = helper\(y = sum\)\)\s*=> 60/), depth: 0},
        {text: expect.stringMatching(/^helper\(y = sum\)\s*=> 6/), depth: 1.5},
        {text: expect.stringMatching(/^sum\s*=> 3\s*↑ line 15$/), depth: 3},
        {text: expect.stringMatching(/^sum \* sign\(x = a\)\s*=> 3$/), depth: 0},
        {text: expect.stringMatching(/^sum\s*=> 3\s*↑ line 15$/), depth: 1.5},
        {text: expect.stringMatching(/^sign\(x = a\)\s*=> 1/), depth: 1.5},
      ])
      // The functions called are closed until asked for, each named by where it is written.
      expect(opener('scale(v = helper')).toHaveTextContent('debugNested.ct:7')
      expect(screen.queryByText(/^scale\(v -> /)).not.toBeInTheDocument()
    })

    it('opens a call a part made in place, under the part', async () => {
      const user = userEvent.setup()
      render(<Providers><Debugger {...syntaxRun()} /></Providers>)

      await open(user, itemOf('scale(v = helper'))
      await user.click(opener('helper(y = sum)'))

      // The helper's block, with its line, straight after the part that called it.
      const helper = itemOf(/^helper\(y.*=> 6/)
      expect(helper).toHaveClass('catln-debug-call')
      const helperPart = [...document.querySelectorAll('.catln-debug-part')].find(p => p.textContent.startsWith('helper(y = sum)'))
      expect(helperPart.nextElementSibling.contains(helper)).toBe(true)

      await user.click(opener('helper(y = sum)'))
      expect(screen.queryAllByRole('treeitem').filter(i => i.classList.contains('catln-debug-call') && /^helper\(y/.test(labelText(i)))).toHaveLength(0)
    })

    it('leads from a use of a line back to the line', async () => {
      const user = userEvent.setup()
      render(<Providers><Debugger {...syntaxRun()} /></Providers>)

      await open(user, itemOf('scale(v = helper'))
      await user.click(screen.getAllByRole('button', { name: '↑ line 15' })[0])

      expect(itemOf('sum = a + b')).toHaveAttribute('aria-selected', 'true')
    })

    it('shows the arm a branch took, and the one it did not as not evaluated', async () => {
      const user = userEvent.setup()
      render(<Providers><Debugger {...syntaxRun()} /></Providers>)

      await open(user, itemOf('scale(v = helper'))
      await user.click(opener('sign(x = a)'))

      expect(labelOf(itemOf('Then'))).toHaveTextContent(/=> 1$/)
      expect(labelOf(itemOf('Else'))).toHaveTextContent('not evaluated')
    })

    it('leads from a #debug record to the call it was recorded in', async () => {
      const user = userEvent.setup()
      render(<Providers><Debugger {...syntaxRun()} /></Providers>)

      await user.click(within(screen.getAllByRole('listitem')[0]).getByRole('button'))

      expect(itemOf('total(a')).toHaveAttribute('aria-selected', 'true')
    })

    // A call of a function is a block of source, which says where it is written: the file and line of
    // the function heading it, and the line of each statement beside it. The steps between are not.
    it('heads each function with where it is written, and numbers its lines', async () => {
      const user = userEvent.setup()
      render(<Providers><Debugger {...syntaxRun()} /></Providers>)

      const total = itemOf('total(a')
      expect(total).toHaveClass('catln-debug-call')
      expect(labelOf(total).querySelector('.catln-debug-file')).toHaveTextContent('debugNested.ct:14')
      expect(labelOf(itemOf('sum = a + b')).querySelector('.catln-debug-lineno')).toHaveTextContent('15')

      await user.click(labelOf(itemOf('scale(v = helper')))
      await user.click(opener('scale(v = helper'))
      expect(labelOf(itemOf(/^scale\(v -> /)).querySelector('.catln-debug-file')).toHaveTextContent('debugNested.ct:7')
    })

    // The whole run is this tree: there is no second tree of calls, nor of arrows.
    it('shows one tree', () => {
      const { container } = render(<Providers><Debugger {...syntaxRun()} /></Providers>)

      expect(container.querySelectorAll('[role=tree]')).toHaveLength(1)
      expect(screen.queryByText('Evaluation tree')).not.toBeInTheDocument()
    })
  })
})
