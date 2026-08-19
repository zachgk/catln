import React from 'react'
import { render, screen, fireEvent } from '@testing-library/react'
import { MemoryRouter } from 'react-router-dom'
import { ThemeProvider, createTheme } from '@mui/material/styles'
import { PageDirContext } from '../Common'
import { Value, tryValue } from '../Value'

const theme = createTheme()

// dir is the directory the page being shown was imported relative to, which arrives with the page.
function Providers({ children, dir = '' }) {
  return (
    <ThemeProvider theme={theme}>
      <PageDirContext.Provider value={dir}>
        <MemoryRouter>{children}</MemoryRouter>
      </PageDirContext.Provider>
    </ThemeProvider>
  )
}

describe('Value', () => {
  it('renders IntVal', () => {
    const { container } = render(<Providers><Value data={{ tag: 'IntVal', contents: 42 }} /></Providers>)
    expect(container.textContent).toBe('42')
  })

  it('renders FloatVal', () => {
    const { container } = render(<Providers><Value data={{ tag: 'FloatVal', contents: 3.14 }} /></Providers>)
    expect(container.textContent).toBe('3.14')
  })

  it('renders StrVal', () => {
    const { container } = render(<Providers><Value data={{ tag: 'StrVal', contents: 'hello' }} /></Providers>)
    expect(container.textContent).toBe('hello')
  })

  it('renders IOVal', () => {
    const { container } = render(<Providers><Value data={{ tag: 'IOVal' }} /></Providers>)
    expect(container.textContent).toBe('IOVal')
  })

  it('renders NoVal', () => {
    const { container } = render(<Providers><Value data={{ tag: 'NoVal' }} /></Providers>)
    expect(container.textContent).toBe('NoVal')
  })

  it('renders unknown TupleVal with name and args', () => {
    const val = {
      tag: 'TupleVal',
      name: '/MyTuple',
      args: {
        '/x': { tag: 'IntVal', contents: 1 },
      },
    }
    const { container } = render(<Providers><Value data={val} /></Providers>)
    expect(container.textContent).toContain('/MyTuple')
    expect(container.textContent).toContain('/x')
    expect(container.textContent).toContain('1')
  })

  it('renders TupleVal with no args without parens', () => {
    const val = { tag: 'TupleVal', name: '/Unit', args: {} }
    const { container } = render(<Providers><Value data={val} /></Providers>)
    expect(container.textContent).toBe('/Unit')
  })
})

// A build result, as the values `stack/core/result.ct` declares. A list is a /Data/Cons chain
// ending in a /Data/Nil, which is what the [ ] sugar desugars to, so these build one the way the
// evaluator hands one over.
const nil = { tag: 'TupleVal', name: '/Data/Nil', args: {} }
const list = entries => entries.reduceRight(
  (tail, head) => ({ tag: 'TupleVal', name: '/Data/Cons', args: { '/head': head, '/tail': tail } }),
  nil
)
const file = (path, contents) => ({
  tag: 'TupleVal',
  name: '/Catln/BuildFile',
  args: {
    '/path': { tag: 'StrVal', contents: path },
    '/contents': { tag: 'StrVal', contents },
  },
})
const result = files => ({
  tag: 'TupleVal',
  name: '/Catln/CatlnResult',
  args: { '/files': list(files) },
})

// A result of one file is shown as that file's writing alone, so these check the rendering of
// contents through the shape a single-file build arrives in.
describe('CatlnResult of one file via Value', () => {
  it('renders .ll files with syntax highlighting', () => {
    const { container } = render(<Providers><Value data={result([file('output.ll', 'define i32 @main()')])} /></Providers>)
    // SyntaxHighlighter renders a <pre> with code
    expect(container.querySelector('pre')).toBeTruthy()
    expect(container.textContent).toContain('define i32 @main()')
  })

  it('renders .html files in an iframe', () => {
    const { container } = render(<Providers><Value data={result([file('page.html', '<h1>Hello</h1>')])} /></Providers>)
    const iframe = container.querySelector('iframe')
    expect(iframe).toBeTruthy()
    expect(iframe.getAttribute('srcdoc')).toBe('<h1>Hello</h1>')
  })

  it('renders other files in a <pre> tag', () => {
    const { container } = render(<Providers><Value data={result([file('output.txt', 'plain text')])} /></Providers>)
    expect(container.querySelector('pre')).toHaveTextContent('plain text')
  })

  it('does not head a single file with its own path', () => {
    const { container } = render(<Providers><Value data={result([file('output.txt', 'plain text')])} /></Providers>)
    expect(container.querySelectorAll('li')).toHaveLength(0)
    expect(container.textContent).not.toContain('output.txt')
  })
})

describe('CatlnResult of several files via Value', () => {
  it('renders each file under the path it is built at', () => {
    const { container } = render(<Providers><Value data={result([file('a.txt', 'first'), file('b.txt', 'second')])} /></Providers>)
    expect(container.querySelectorAll('li')).toHaveLength(2)
    expect(container.textContent).toContain('a.txt')
    expect(container.textContent).toContain('first')
    expect(container.textContent).toContain('b.txt')
    expect(container.textContent).toContain('second')
  })

  it('shows a path with directories in it as the path it is', () => {
    const { container } = render(<Providers><Value data={result([file('sub/deep.txt', 'buried'), file('top.txt', 'shallow')])} /></Providers>)
    expect(container.textContent).toContain('sub/deep.txt')
    expect(container.textContent).toContain('buried')
  })

  it('says so when a result holds no files', () => {
    const { container } = render(<Providers><Value data={result([])} /></Providers>)
    expect(container.querySelector('em')).toHaveTextContent('no files')
  })

  it('shows a malformed tail rather than dropping it', () => {
    // A list whose end is not a Nil is not a list. It is shown rather than ending the walk, so a
    // result quietly missing its end does not read as a result that ended there.
    const broken = {
      tag: 'TupleVal',
      name: '/Catln/CatlnResult',
      args: {
        '/files': {
          tag: 'TupleVal',
          name: '/Data/Cons',
          args: { '/head': file('a.txt', 'kept'), '/tail': { tag: 'StrVal', contents: 'not a list' } },
        },
      },
    }
    const { container } = render(<Providers><Value data={broken} /></Providers>)
    expect(container.querySelectorAll('li')).toHaveLength(2)
    expect(container.textContent).toContain('not a list')
  })
})

// A documentation comment is markdown, and a relative link in it is written relative to the page it
// is on. Which directory that is comes with the page, so the link is resolved from what the server
// said rather than from a listing of every page the app has.
describe('documentation links', () => {
  function md(text) {
    return { tag: 'TupleVal', name: '/Catln/Doc/Show/MD', args: { '/text': { tag: 'StrVal', contents: text } } }
  }

  it('resolves a relative link against the page it is on', () => {
    render(<Providers dir="/test/sub"><Value data={md('see [other](other.ct)')} /></Providers>)

    expect(screen.getByText('other')).toHaveAttribute('href', '/docs/%2Ftest%2Fsub%2Fother.ct')
  })

  it('leaves a link alone where there is no page to resolve it against', () => {
    render(<Providers><Value data={md('see [other](other.ct)')} /></Providers>)

    expect(screen.getByText('other')).toHaveAttribute('href', 'other.ct')
  })

  it('leaves absolute links and urls alone', () => {
    render(<Providers dir="/test/sub"><Value data={md('[a](/test/id.ct) [b](https://catln.dev)')} /></Providers>)

    expect(screen.getByText('a')).toHaveAttribute('href', '/test/id.ct')
    expect(screen.getByText('b')).toHaveAttribute('href', 'https://catln.dev')
  })

  it('resolves a link written down into a folder', () => {
    render(<Providers dir="/test"><Value data={md('see [Notes/Nested](Notes/Nested.md)')} /></Providers>)

    expect(screen.getByText('Notes/Nested')).toHaveAttribute('href', '/docs/%2Ftest%2FNotes%2FNested.md')
  })

  // A file's name may hold a space, which an address cannot, so such a name is written escaped.
  it('resolves an address escaping a name which holds a space', () => {
    render(<Providers dir="/test"><Value data={md('see [test link](test%20link.md)')} /></Providers>)

    expect(screen.getByText('test link')).toHaveAttribute('href', '/docs/%2Ftest%2Ftest%20link.md')
  })

  // A page is shown as the one run of markdown it is, with no heading to arrive at, so the place
  // within a page an address goes on to name is dropped rather than carried into the route.
  it('drops the place within a page an address names', () => {
    render(<Providers dir="/test"><Value data={md('see [Welcome#Getting started](Welcome.md#Getting%20started)')} /></Providers>)

    expect(screen.getByText('Welcome#Getting started')).toHaveAttribute('href', '/docs/%2Ftest%2FWelcome.md')
  })
})

describe('note and overlay values', () => {
  // An overlay dispatches to the link rather than to a frame: the frame belongs to whatever is
  // laying the page out around it, so this and the layout are one frame and one link.
  it('shows an overlay as the address it names, and not as a frame', () => {
    const val = {
      tag: 'TupleVal',
      name: '/Catln/Doc/Show/Overlay',
      args: { '/url': { tag: 'StrVal', contents: 'https://example.com' } },
    }
    const { container } = render(<Providers><Value data={val} /></Providers>)

    expect(screen.getByRole('link', { name: 'https://example.com' })).toBeInTheDocument()
    expect(container.querySelector('iframe')).toBeNull()
  })

  it('shows the properties a note opens with as a table', () => {
    const val = {
      tag: 'TupleVal',
      name: '/Data/Note/Show/Properties',
      args: { '/text': { tag: 'StrVal', contents: 'title: A page' } },
    }
    render(<Providers><Value data={val} /></Providers>)

    expect(screen.getByRole('rowheader', { name: 'title' })).toBeInTheDocument()
    expect(screen.getByText('A page')).toBeInTheDocument()
  })

  // An entry of a feed is the value the page declares, like a task, so it dispatches to the entry
  // itself rather than to the `/Name(arg = ...)` every other value falls back to.
  it('shows an entry of a feed as the entry it is', () => {
    const val = {
      tag: 'TupleVal',
      name: '/Data/RSS/Item',
      args: {
        '/title': { tag: 'StrVal', contents: 'Accretionary Arc' },
        '/link': { tag: 'StrVal', contents: 'https://xkcd.com/3285/' },
        '/guid': { tag: 'StrVal', contents: 'https://xkcd.com/3285/' },
        '/published': { tag: 'StrVal', contents: 'Fri, 14 Aug 2026 04:00:00 -0000' },
        '/description': { tag: 'StrVal', contents: '<img src="comic.png" alt="A comic" />' },
      },
    }
    const { container } = render(<Providers><Value data={val} /></Providers>)

    expect(screen.getByRole('link', { name: 'Accretionary Arc' })).toHaveAttribute('href', 'https://xkcd.com/3285/')
    expect(container.querySelector('iframe').getAttribute('srcdoc')).toContain('comic.png')
    expect(screen.queryByText(/\/Data\/RSS\/Item/)).toBeNull()
  })
})

// The display components take update alongside the value's own args, which is what lets an
// interactive one edit the expression it was compiled from.
const listProgramVal = {
  tag: 'TupleVal',
  name: '/Catln/Doc/Show/ListProgram',
  args: {
    '/mode': { tag: 'TupleVal', name: '/Catln/Doc/Typecheck', args: {} },
    '/prgm': { tag: 'StrVal', contents: JSON.stringify({ prgmObjMap: {}, prgmCG: [{}, {}] }) },
  },
}

describe('update forwarding', () => {
  it('passes update to the value display component', () => {
    const update = vi.fn()
    render(<Providers><Value data={listProgramVal} update={update} /></Providers>)

    fireEvent.click(screen.getByLabelText('TreeBuild'))

    expect(update).toHaveBeenCalledWith('#listProgram(mode= /Catln/Doc/TreeBuild)')
  })

  it('renders without an update', () => {
    render(<Providers><Value data={listProgramVal} /></Providers>)
    expect(screen.getByLabelText('List')).toBeChecked()
  })
})

describe('tryValue', () => {
  it('returns null for falsy input', () => {
    expect(tryValue(null)).toBeNull()
    expect(tryValue(undefined)).toBeNull()
  })

  it('returns rendered Value for valid data', () => {
    const result = tryValue({ tag: 'IntVal', contents: 7 })
    expect(result).not.toBeNull()
  })

  it('passes update through to the Value', () => {
    const update = vi.fn()
    render(<Providers>{tryValue(listProgramVal, update)}</Providers>)

    fireEvent.click(screen.getByLabelText('No Typecheck'))

    expect(update).toHaveBeenCalledWith('#listProgram(mode= /Catln/Doc/Desugar)')
  })
})
