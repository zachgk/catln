import React from 'react'
import { render, screen, renderHook, waitFor, act } from '@testing-library/react'
import { MemoryRouter } from 'react-router-dom'
import { ThemeProvider, createTheme } from '@mui/material/styles'
import {
  tagJoin,
  isTopType,
  useApi,
  useLiveApi,
  Loading,
  Notes,
  Type,
  PartialName,
  PartialKey,
} from '../Common'

const theme = createTheme()

function Providers({ children }) {
  return (
    <ThemeProvider theme={theme}>
      <MemoryRouter>{children}</MemoryRouter>
    </ThemeProvider>
  )
}

// --- tagJoin ---

describe('tagJoin', () => {
  it('returns null for empty array', () => {
    expect(tagJoin([], ', ')).toBeNull()
  })

  it('returns single element without separator', () => {
    const result = tagJoin(['a'], ', ')
    expect(result).toEqual(['a'])
  })

  it('joins multiple elements with separator', () => {
    const { container } = render(<div>{tagJoin([<span key="a">a</span>, <span key="b">b</span>], ' | ')}</div>)
    expect(container.textContent).toBe('a | b')
  })
})

// --- isTopType ---

const predsNone = { tag: 'PredsAnd', contents: [] }
const argExact = { ptArgLb: true, ptArgUb: true }
const leaf = { '/Integer': [[[], [], predsNone, argExact]] }

describe('isTopType', () => {
  it('returns true for bare TopType', () => {
    const t = { tag: 'UnionType', contents: [[predsNone, {}], {}, []] }
    expect(isTopType(t)).toBe(true)
  })

  it('returns false when the negative partials are non-empty', () => {
    const t = { tag: 'UnionType', contents: [[predsNone, leaf], {}, []] }
    expect(isTopType(t)).toBe(false)
  })

  it('returns false when preds are non-empty', () => {
    const preds = { tag: 'PredsAnd', contents: [{ tag: 'PredsOne', contents: {} }] }
    const t = { tag: 'UnionType', contents: [[preds, {}], {}, []] }
    expect(isTopType(t)).toBe(false)
  })

  it('returns false for non-PredsAnd preds', () => {
    const t = { tag: 'UnionType', contents: [[{ tag: 'PredsOne', contents: {} }, {}], {}, []] }
    expect(isTopType(t)).toBe(false)
  })

  it('returns false when there are partials or constants', () => {
    expect(isTopType({ tag: 'UnionType', contents: [[predsNone, {}], leaf, []] })).toBe(false)
    expect(isTopType({ tag: 'UnionType', contents: [[predsNone, {}], {}, [{ tag: 'CInt', contents: 5 }]] })).toBe(false)
  })

  it('returns false without a top part, and for other tags', () => {
    expect(isTopType({ tag: 'UnionType', contents: [null, {}, []] })).toBe(false)
    expect(isTopType({ tag: 'TypeVar', contents: [] })).toBe(false)
  })
})

// --- useApi ---

describe('useApi', () => {
  it('returns isLoaded false initially', () => {
    global.fetch = vi.fn(() => new Promise(() => {})) // never resolves
    const { result } = renderHook(() => useApi('/api/test'), { wrapper: Providers })
    expect(result.current.isLoaded).toBe(false)
    expect(result.current.data).toBeNull()
  })

  it('sets data and notes on Success response', async () => {
    global.fetch = vi.fn(() =>
      Promise.resolve({
        status: 200,
        json: () => Promise.resolve({ tag: 'Success', contents: [{ foo: 'bar' }, [{ tp: 'CNoteWarning', msg: 'warn' }]] }),
      })
    )
    const { result } = renderHook(() => useApi('/api/test'), { wrapper: Providers })
    await waitFor(() => expect(result.current.isLoaded).toBe(true))
    expect(result.current.data).toEqual({ foo: 'bar' })
    expect(result.current.notes).toEqual([{ tp: 'CNoteWarning', msg: 'warn' }])
    expect(result.current.error).toBeUndefined()
  })

  it('sets notes only on ResFail response', async () => {
    global.fetch = vi.fn(() =>
      Promise.resolve({
        status: 200,
        json: () => Promise.resolve({ tag: 'ResFail', contents: [{ tp: 'CNoteError', msg: 'fail' }] }),
      })
    )
    const { result } = renderHook(() => useApi('/api/test'), { wrapper: Providers })
    await waitFor(() => expect(result.current.isLoaded).toBe(true))
    expect(result.current.data).toBeUndefined()
    expect(result.current.notes).toEqual([{ tp: 'CNoteError', msg: 'fail' }])
  })

  it('sets error on non-200 status', async () => {
    global.fetch = vi.fn(() =>
      Promise.resolve({ status: 500, statusText: 'Internal Server Error' })
    )
    const { result } = renderHook(() => useApi('/api/test'), { wrapper: Providers })
    await waitFor(() => expect(result.current.isLoaded).toBe(true))
    expect(result.current.error).toBeTruthy()
    expect(result.current.error.message).toBe('Internal Server Error')
  })

  it('sets error with "Unknown result type" on unknown tag', async () => {
    global.fetch = vi.fn(() =>
      Promise.resolve({
        status: 200,
        json: () => Promise.resolve({ tag: 'SomethingElse', contents: [] }),
      })
    )
    const { result } = renderHook(() => useApi('/api/test'), { wrapper: Providers })
    await waitFor(() => expect(result.current.isLoaded).toBe(true))
    expect(result.current.error).toBe('Unknown result type')
  })
})

// A display which follows what it is showing, rather than asking about it once. The server holds the
// question open and answers it again whenever a file the answer was compiled from is written to, so
// what these cases drive is answers arriving without anything having asked for them.
describe('useLiveApi', () => {
  const success = data => ({ tag: 'Success', contents: [data, []] })

  it('follows the path it was given, and stops when the display goes', async () => {
    const { result, unmount } = renderHook(() => useLiveApi('/api/page?prgmName=a'), { wrapper: Providers })
    await waitFor(() => expect(result.current.isLoaded).toBe(true))

    const source = eventSourceFor('/api/page?prgmName=a')
    expect(source).toBeTruthy()
    expect(source.readyState).toBe(1)

    // Closed when the display goes, which is what takes it off the list the server is following. One
    // left open is a question the server answers again on every change, for nobody.
    unmount()
    expect(source.readyState).toBe(2)
  })

  it('takes an answer that arrives with nothing having asked', async () => {
    global.fetch = vi.fn(() => Promise.resolve({ status: 200, json: () => Promise.resolve(success({ v: 'first' })) }))
    const { result } = renderHook(() => useLiveApi('/api/page?prgmName=a'), { wrapper: Providers })
    await waitFor(() => expect(result.current.data).toEqual({ v: 'first' }))

    act(() => eventSourceFor('/api/page').frame(success({ v: 'second' })))
    expect(result.current.data).toEqual({ v: 'second' })
  })

  it('shows the notes of a page that failed to compile, and keeps following it', async () => {
    const { result } = renderHook(() => useLiveApi('/api/page?prgmName=a'), { wrapper: Providers })
    await waitFor(() => expect(result.current.isLoaded).toBe(true))

    // A page that will not compile is an ordinary answer carrying notes, not a connection that
    // failed -- so it is shown, and the page is still followed for the edit which fixes it.
    act(() => eventSourceFor('/api/page').frame({ tag: 'ResFail', contents: [{ tp: 'CNoteError', msg: 'nope' }] }))
    expect(result.current.notes).toEqual([{ tp: 'CNoteError', msg: 'nope' }])
    expect(eventSourceFor('/api/page').readyState).toBe(1)

    act(() => eventSourceFor('/api/page').frame(success({ v: 'fixed' })))
    expect(result.current.data).toEqual({ v: 'fixed' })
    expect(result.current.notes).toEqual([])
  })

  it('keeps what is on screen when the connection drops, and takes the next answer', async () => {
    const { result } = renderHook(() => useLiveApi('/api/page?prgmName=a'), { wrapper: Providers })
    await waitFor(() => expect(result.current.isLoaded).toBe(true))
    act(() => eventSourceFor('/api/page').frame(success({ v: 'shown' })))

    // The browser opens the connection again by itself, so a page already on screen stays there
    // rather than blinking to an error for a connection which is about to come back.
    act(() => eventSourceFor('/api/page').fail())
    expect(result.current.error).toBeFalsy()
    expect(result.current.data).toEqual({ v: 'shown' })

    act(() => eventSourceFor('/api/page').frame(success({ v: 'after' })))
    expect(result.current.data).toEqual({ v: 'after' })
  })

  // The answer to a page a reader has left cannot land on the page they went to. Closing is what a
  // display does when its path changes, and a closed connection hands over nothing -- so unlike a
  // request asked once, whose answer arrives whenever it arrives, this cannot show the wrong page.
  it('takes no answer meant for the page it has left', async () => {
    const { result, rerender } = renderHook(({ path }) => useLiveApi(path), {
      wrapper: Providers,
      initialProps: { path: '/api/page?prgmName=a' },
    })
    await waitFor(() => expect(result.current.isLoaded).toBe(true))
    const first = eventSourceFor('prgmName=a')

    rerender({ path: '/api/page?prgmName=b' })
    act(() => eventSourceFor('prgmName=b').frame(success({ v: 'b' })))
    expect(result.current.data).toEqual({ v: 'b' })

    // The page left over answers late. It was closed when the display moved, so nothing comes of it.
    act(() => first.frame(success({ v: 'a, at last' })))
    expect(result.current.data).toEqual({ v: 'b' })
  })

  it('follows nothing when there is no path', () => {
    const { result } = renderHook(() => useLiveApi(null), { wrapper: Providers })
    expect(result.current.isLoaded).toBe(false)
    expect(eventSources()).toEqual([])
  })

  it('re-fetches when path changes', async () => {
    let callCount = 0
    global.fetch = vi.fn(() => {
      callCount++
      return Promise.resolve({
        status: 200,
        json: () => Promise.resolve({ tag: 'Success', contents: [callCount, []] }),
      })
    })
    const { result, rerender } = renderHook(
      ({ path }) => useApi(path),
      { wrapper: Providers, initialProps: { path: '/api/a' } }
    )
    await waitFor(() => expect(result.current.isLoaded).toBe(true))
    expect(result.current.data).toBe(1)

    rerender({ path: '/api/b' })
    await waitFor(() => expect(result.current.data).toBe(2))
    expect(global.fetch).toHaveBeenCalledTimes(2)
  })
})

// --- Loading ---

describe('Loading', () => {
  it('shows error message when error is present', () => {
    render(<Loading status={{ error: { message: 'fail' }, isLoaded: true }}><div>ok</div></Loading>)
    expect(screen.getByText('Error: fail')).toBeInTheDocument()
  })

  it('shows Loading... when not loaded', () => {
    render(<Loading status={{ isLoaded: false, error: null, data: null, notes: [] }}><div>ok</div></Loading>)
    expect(screen.getByText('Loading...')).toBeInTheDocument()
  })

  it('shows Notes when loaded but no data', () => {
    render(
      <Providers>
        <Loading status={{ isLoaded: true, data: null, notes: [{ tp: 'CNoteError', msg: 'something failed' }] }}>
          <div>ok</div>
        </Loading>
      </Providers>
    )
    expect(screen.getByText('something failed')).toBeInTheDocument()
    expect(screen.queryByText('ok')).not.toBeInTheDocument()
  })

  it('renders children when loaded with data', () => {
    render(
      <Loading status={{ isLoaded: true, data: { some: 'data' }, notes: [] }}>
        <div>content here</div>
      </Loading>
    )
    expect(screen.getByText('content here')).toBeInTheDocument()
  })

  // A response that arrived with error notes is a partial answer, not a complete one: /api/annot
  // reports an annotation it could not evaluate as a note and returns the rest. Showing only the
  // rest would present it as though nothing had gone wrong.
  it('shows Notes alongside the children when data arrives with notes', () => {
    render(
      <Providers>
        <Loading status={{ isLoaded: true, data: { some: 'data' }, notes: [{ tp: 'CNoteError', msg: 'something failed' }] }}>
          <div>content here</div>
        </Loading>
      </Providers>
    )
    expect(screen.getByText('something failed')).toBeInTheDocument()
    expect(screen.getByText('content here')).toBeInTheDocument()
  })
})

// --- Notes ---

describe('Notes', () => {
  it('renders error notes', () => {
    const { container } = render(
      <Providers>
        <Notes notes={[{ tp: 'CNoteError', msg: 'an error' }]} />
      </Providers>
    )
    expect(container.querySelector('pre')).toHaveTextContent('an error')
  })

  it('renders warning notes', () => {
    const { container } = render(
      <Providers>
        <Notes notes={[{ tp: 'CNoteWarning', msg: 'a warning' }]} />
      </Providers>
    )
    expect(container.querySelector('pre')).toHaveTextContent('a warning')
  })

  it('filters position-only notes when noPosOnly is true', () => {
    const notes = [
      { tp: 'CNoteError', msg: 'real error', pos: null },
      { tp: 'CNoteWarning', msg: 'pos only', pos: { line: 1, col: 1 } },
    ]
    const { container } = render(
      <Providers>
        <Notes notes={notes} noPosOnly={true} />
      </Providers>
    )
    expect(container).toHaveTextContent('real error')
    expect(container).not.toHaveTextContent('pos only')
  })
})

// --- Type ---

describe('Type', () => {
  it('renders empty string for bare TopType', () => {
    const t = { tag: 'UnionType', contents: [[predsNone, {}], {}, []] }
    const { container } = render(<Providers><Type data={t} /></Providers>)
    expect(container.textContent).toBe('')
  })

  it('renders TopType with its negative partials', () => {
    const t = { tag: 'UnionType', contents: [[predsNone, leaf], {}, []] }
    const { container } = render(<Providers><Type data={t} /></Providers>)
    expect(container.textContent).toContain('TopType')
    expect(container.textContent).toContain('- ')
    expect(container.textContent).toContain('Integer')
  })

  it('renders a lone class predicate as ∀', () => {
    const cls = { ptName: '/Eq', ptVars: [], ptArgs: [], ptPreds: predsNone, ptArgMode: argExact }
    const preds = { tag: 'PredsOne', contents: { tag: 'PredClass', contents: cls } }
    const t = { tag: 'UnionType', contents: [[preds, {}], {}, []] }
    const { container } = render(<Providers><Type data={t} /></Providers>)
    expect(container.textContent).toBe('∀/Eq')
  })

  const key = (name) => ({ pkName: name, pkArgs: [], pkVars: [] })

  it('renders a TVVar by location', () => {
    const int = { tag: 'TypeVar', contents: [{ tag: 'TVVar', contents: key('a') }, 'TVInt'] }
    expect(render(<Providers><Type data={int} /></Providers>).container.textContent).toBe('$a')

    const ext = { tag: 'TypeVar', contents: [{ tag: 'TVVar', contents: key('a') }, 'TVExt'] }
    expect(render(<Providers><Type data={ext} /></Providers>).container.textContent).toBe('$_a')
  })

  it('renders a TVArg by location', () => {
    const int = { tag: 'TypeVar', contents: [{ tag: 'TVArg', contents: key('v') }, 'TVInt'] }
    expect(render(<Providers><Type data={int} /></Providers>).container.textContent).toBe('TVArg v')

    const ext = { tag: 'TypeVar', contents: [{ tag: 'TVArg', contents: key('v') }, 'TVExt'] }
    expect(render(<Providers><Type data={ext} /></Providers>).container.textContent).toBe('TVArg(ext) v')
  })

  it('renders a TVApplied with its overrides', () => {
    const base = { tag: 'TVVar', contents: key('F') }
    const t = {
      tag: 'TypeVar',
      contents: [
        { tag: 'TVApplied', contents: [base, [[key('T'), { tag: 'UnionType', contents: [null, leaf, []] }]]] },
        'TVInt',
      ],
    }
    const { container } = render(<Providers><Type data={t} /></Providers>)
    expect(container.textContent).toBe('$F[T=/Integer]')
  })

  it('renders several partials joined with " + " in parentheses', () => {
    const t = {
      tag: 'UnionType',
      contents: [
        null,
        {
          '/Integer': [[[], [], predsNone, argExact]],
          '/String': [[[], [], predsNone, argExact]],
        },
        [],
      ],
    }
    const { container } = render(<Providers><Type data={t} /></Providers>)
    expect(container.textContent).toContain('Integer')
    expect(container.textContent).toContain('String')
    expect(container.textContent).toContain(' + ')
  })

  it('renders a lone partial bare and no partials as ∅', () => {
    const one = { tag: 'UnionType', contents: [null, leaf, []] }
    expect(render(<Providers><Type data={one} /></Providers>).container.textContent).toBe('/Integer')

    const none = { tag: 'UnionType', contents: [null, {}, []] }
    expect(render(<Providers><Type data={none} /></Providers>).container.textContent).toBe('∅')
  })
})

// --- PartialName ---

describe('PartialName', () => {
  it('renders PTypeName as a link to its #type annotation', () => {
    render(<Providers><PartialName name={{ tag: 'PTypeName', contents: '/Integer' }} /></Providers>)
    const link = screen.getByRole('link', { name: '/Integer' })
    expect(link).toHaveAttribute('href', '/annot?annot=%23type(name%3D%20%22%2FInteger%22)')
  })

  it('renders PClassName as a link to its #type annotation', () => {
    render(<Providers><PartialName name={{ tag: 'PClassName', contents: '/Show' }} /></Providers>)
    const link = screen.getByRole('link', { name: '/Show' })
    expect(link).toHaveAttribute('href', '/annot?annot=%23type(name%3D%20%22%2FShow%22)')
  })

  it('renders PRelativeName as link to /relative/...', () => {
    render(<Providers><PartialName name={{ tag: 'PRelativeName', contents: 'localName' }} /></Providers>)
    const link = screen.getByRole('link', { name: 'localName' })
    expect(link).toHaveAttribute('href', '/relative/localName')
  })
})

// --- PartialKey ---

describe('PartialKey', () => {
  it('renders pkName when no args or vars', () => {
    const { container } = render(
      <Providers>
        <PartialKey data={{ pkName: 'x', pkArgs: [], pkVars: [] }} />
      </Providers>
    )
    expect(container.textContent).toBe('x')
  })

  it('renders "PartialKeyWith" when has args', () => {
    const { container } = render(
      <Providers>
        <PartialKey data={{ pkName: 'x', pkArgs: [1], pkVars: [] }} />
      </Providers>
    )
    expect(container.textContent).toBe('PartialKeyWith')
  })
})
