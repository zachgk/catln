import React from 'react'
import { cleanup, render, screen, within } from '@testing-library/react'
import userEvent from '@testing-library/user-event'
import { MemoryRouter } from 'react-router-dom'
import { ThemeProvider, createTheme } from '@mui/material/styles'
import Debugger from '../Debugger'
import debuggerResponse from '../../../test/fixtures/debugger-response.json'
import fmapResponse from '../../../test/fixtures/debugger-fmap-response.json'
import mmapResponse from '../../../test/fixtures/debugger-mmap-response.json'
import foldlResponse from '../../../test/fixtures/debugger-foldl-response.json'
import foldrResponse from '../../../test/fixtures/debugger-foldr-response.json'

const theme = createTheme()

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
    expect(screen.getByText(/Recorded nothing/)).toBeInTheDocument()
    expect(screen.getByText(/Exited with 1/)).toBeInTheDocument()
    expect(screen.queryByRole('treeitem')).not.toBeInTheDocument()
  })

  // The #debug records of the run, which is the other direction through it: the tree says what was
  // evaluated, and these say what a value was at a point the program named.
  it('lists what the run recorded, in the order it recorded them', () => {
    render(<Providers><Debugger {...debug()} /></Providers>)

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

  // A run of one expression (#debugExpr) rather than of a program: it was given no IO handle to
  // print with, so what it ended with is a value and there is no exit code to report.
  // A call is read for what it was called on. Its types are still there, one hover away on the name
  // each is the type of, rather than written after every argument of every call.
  it('keeps the types of a call out of its label and on hover instead', async () => {
    const { container } = render(<Providers><Debugger {...debug()} /></Providers>)

    const label = container.querySelector('.MuiTreeItem-label')
    expect(label.textContent).not.toContain('->')
    const typed = label.querySelector('.catln-typed')
    expect(typed).toBeTruthy()

    await userEvent.hover(typed)
    expect(await screen.findByRole('tooltip')).toHaveTextContent(/\S/)
  })

  it('shows the value an expression run produced instead of an exit code', () => {
    const expr = {
      drExit: null,
      drOutput: '',
      drValue: { tag: 'IntVal', contents: 3 },
      drResult: { erCoverage: {}, erTreebug: [], erDebugs: [] },
    }
    render(<Providers><Debugger {...debug(expr)} /></Providers>)

    expect(screen.getByRole('heading', { name: 'Value' })).toBeInTheDocument()
    expect(screen.getByText('3')).toBeInTheDocument()
    expect(screen.queryByText(/Exited with/)).not.toBeInTheDocument()
    expect(screen.queryByText(/Printed nothing/)).not.toBeInTheDocument()
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
    // beside it has nothing below it and keeps the generic label, which spells the definition out.
    const roots = screen.getAllByRole('treeitem')
    expect(within(roots[1]).getByText('/Data/Functor/fmap')).toBeInTheDocument()
    expect(within(roots[1]).queryByText(/\|::\|/)).not.toBeInTheDocument()
  })

  it('shows the call as its table rather than as the generic label', async () => {
    const fmap = await openFmap()

    // Every row of what the mapping did, which is what the display exists to say.
    expect(fmap.textContent).toContain('1')
    expect(fmap.textContent).toContain('4')
    expect(screen.getAllByText('↦').length).toBe(3)
    // The generic label of a node -- definition, input, result -- is what the table replaced, for this
    // call. The call beside it has no display of its own and keeps that label, which is the whole of
    // what "a call with no arm is shown the way everything else is" means.
    expect(within(fmap).queryByText(/\|::\|/)).not.toBeInTheDocument()
    expect(screen.getAllByText(/\|::\|/).length).toBeGreaterThan(0)
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
    expect(within(mmap).queryByText(/\|::\|/)).not.toBeInTheDocument()
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
    expect(within(fold).queryByText(/\|::\|/)).not.toBeInTheDocument()
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
