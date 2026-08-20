import React from 'react'
import { render, screen, renderHook, waitFor, act } from '@testing-library/react'
import { MemoryRouter } from 'react-router-dom'
import { ThemeProvider, createTheme } from '@mui/material/styles'
import {
  tagJoin,
  isTopType,
  useApi,
  useLiveApi,
  useDebounced,
  callApi,
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

  // Asking again does not cancel what was already asked, and the two are not answered in the order
  // they were asked: a search re-asked at each keystroke asks for more work for a shorter query than
  // for a longer one. So the answer to the question being asked has to win however late the answer
  // to the one before it arrives.
  it('shows the answer to what it is asking, not the one that arrives last', async () => {
    // Held open per path, so that the two can be answered in the other order. Only these two: the
    // mock outlives the case, and a request left waiting forever is one the next case waits for.
    const answers = {}
    global.fetch = vi.fn((path) => Promise.resolve({
      status: 200,
      json: () => (path === '/api/first' || path === '/api/second')
        ? new Promise(resolve => { answers[path] = resolve })
        : Promise.resolve({ tag: 'Success', contents: [null, []] }),
    }))
    const { result, rerender } = renderHook(({ path }) => useApi(path), {
      wrapper: Providers,
      initialProps: { path: '/api/first' },
    })
    await waitFor(() => expect(answers['/api/first']).toBeDefined())

    rerender({ path: '/api/second' })
    await waitFor(() => expect(answers['/api/second']).toBeDefined())

    await act(async () => {
      answers['/api/second']({ tag: 'Success', contents: ['second', []] })
      answers['/api/first']({ tag: 'Success', contents: ['first', []] })
    })

    expect(result.current.data).toBe('second')
  })
})

// What is asked of the server is what a reader is asking rather than each step of them typing it.
describe('useDebounced', () => {
  afterEach(() => vi.useRealTimers())

  it('settles on what it was last given once it stops changing', async () => {
    vi.useFakeTimers()
    const { result, rerender } = renderHook(({ value }) => useDebounced(value, 150), {
      initialProps: { value: 'u' },
    })
    expect(result.current).toBe('u')

    for (const value of ['ut', 'uti', 'util', 'utils']) {
      rerender({ value })
      await act(async () => { vi.advanceTimersByTime(50) })
      // Still what it was: each change starts the wait again, so none of the letters between is
      // ever what anything is asked about.
      expect(result.current).toBe('u')
    }

    await act(async () => { vi.advanceTimersByTime(150) })
    expect(result.current).toBe('utils')
  })
})

// The call that evaluates a function of the program with what to compute over sent as its arguments.
describe('callApi', () => {
  it('names the function, its arguments and the page to call it in', () => {
    expect(callApi('/Catln/Doc/searchWebdocs', { query: 'a b' }, '/test/id.ct'))
      .toBe('/api/call?fun=%2FCatln%2FDoc%2FsearchWebdocs&args=%7B%22query%22%3A%22a%20b%22%7D&prgmName=%2Ftest%2Fid.ct')
  })

  // The arguments are JSON rather than Catln source, so a query holding what would be syntax is a
  // query rather than the start of a parse error.
  it('sends what was typed rather than something to be parsed', () => {
    const url = callApi('/f', { query: '") + #print(' })
    expect(url).not.toMatch(/prgmName/)
    expect(JSON.parse(decodeURIComponent(url.split('args=')[1]))).toEqual({ query: '") + #print(' })
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

// --- Loading progress ---
//
// What a reader is shown while they wait. The compiler is asked separately from whatever is being
// waited for, because what a page costs is paid before the request for it answers at all — so these
// drive the progress stream by hand rather than through any display's own request.

describe('Loading progress', () => {
  const waiting = { isLoaded: false, error: null, data: null, notes: [] }

  function progressFrame(p) {
    act(() => eventSourceFor('/api/progress').frame({ tag: 'Success', contents: [p, []] }))
  }

  function renderWaiting(count = 1) {
    const bars = Array.from({ length: count }, (_, i) => (
      <Loading key={i} status={waiting}><div>ok</div></Loading>
    ))
    const rendered = render(<Providers>{bars}</Providers>)
    // Nothing is asked until a wait has gone on long enough to be worth reporting.
    act(() => vi.advanceTimersByTime(600))
    return rendered
  }

  const progressSources = () => eventSources().filter(s => s.url.includes('/api/progress'))

  beforeEach(() => vi.useFakeTimers())
  afterEach(() => vi.useRealTimers())

  it('shows a bar while it is waiting', () => {
    renderWaiting()
    expect(screen.getByText('Loading...')).toBeInTheDocument()
    expect(screen.getAllByRole('progressbar').length).toBe(1)
  })

  // A page that arrives at once should arrive at once. A bar shown for a wait too short to read is
  // the display flickering rather than reporting.
  it('asks nothing for a wait that is over before it is worth reporting', () => {
    const { unmount } = render(<Providers><Loading status={waiting}><div>ok</div></Loading></Providers>)
    act(() => vi.advanceTimersByTime(200))
    unmount()
    act(() => vi.advanceTimersByTime(1000))
    expect(progressSources()).toEqual([])
  })

  // A browser allows six connections to a server, and the app already holds two open for itself.
  it('asks once however many displays are waiting', () => {
    renderWaiting(3)
    expect(progressSources().length).toBe(1)
    expect(screen.getAllByRole('progressbar').length).toBe(3)
  })

  it('says what the compiler is doing, and how far through it is', () => {
    renderWaiting()
    progressFrame({ prWorking: true, prStep: 'StepBuilding', prDone: 3, prTotal: 12, prAt: '/a.ct' })
    expect(screen.getByTestId('loading-progress')).toHaveTextContent('Compiled 3 of 12 pages')
    expect(screen.getByRole('progressbar')).toHaveAttribute('aria-valuenow', '25')
  })

  // A type check is what a build spends its time inside, so the pages it has left to build are still
  // the pages it has left. Losing the count there would lose it for all but an instant of the build.
  it('keeps the count of a build through the type check inside it', () => {
    renderWaiting()
    progressFrame({ prWorking: true, prStep: 'StepTypechecking', prDone: 6, prTotal: 12, prAt: '/b.ct' })
    expect(screen.getByTestId('loading-progress')).toHaveTextContent('Type checking /b.ct')
    expect(screen.getByRole('progressbar')).toHaveAttribute('aria-valuenow', '50')
  })

  // A step with nothing to count says so rather than inventing a fraction.
  it('counts nothing for an evaluation, which has no count', () => {
    renderWaiting()
    progressFrame({ prWorking: true, prStep: 'StepEvaluating', prDone: 0, prTotal: 0, prAt: '/c.ct' })
    expect(screen.getByTestId('loading-progress')).toHaveTextContent('Evaluating /c.ct')
    expect(screen.getByRole('progressbar')).not.toHaveAttribute('aria-valuenow')
  })

  // A count that has reached its total is a step that has FINISHED, not a wait that is over: the
  // compiler can start a build from inside an evaluation and leave the pages it built at their total.
  it('goes back to counting nothing once a step has finished', () => {
    renderWaiting()
    progressFrame({ prWorking: true, prStep: 'StepBuilding', prDone: 12, prTotal: 12, prAt: '/a.ct' })
    expect(screen.getByRole('progressbar')).not.toHaveAttribute('aria-valuenow')
  })

  // A compiler working for nobody is not a wait to report on. The bar is drawn from this display's
  // own waiting; what the compiler says is only used when it says it is working for somebody.
  it('says nothing about a compiler that is not working', () => {
    renderWaiting()
    progressFrame({ prWorking: false, prStep: 'StepIdle', prDone: 0, prTotal: 0, prAt: null })
    expect(screen.queryByTestId('loading-progress')).not.toBeInTheDocument()
    expect(screen.getByRole('progressbar')).toBeInTheDocument()
  })

  it('keeps waiting when the doc server has nothing to say', () => {
    renderWaiting()
    act(() => eventSourceFor('/api/progress').fail())
    expect(screen.getByText('Loading...')).toBeInTheDocument()
    expect(screen.getByRole('progressbar')).toBeInTheDocument()
    expect(screen.queryByTestId('loading-progress')).not.toBeInTheDocument()
  })

  it('passes over a frame it cannot read rather than taking the display down', () => {
    renderWaiting()
    act(() => eventSourceFor('/api/progress').onmessage({data: 'not json'}))
    expect(screen.getByText('Loading...')).toBeInTheDocument()
    expect(screen.queryByTestId('loading-progress')).not.toBeInTheDocument()
  })

  // The stream is what a display waiting opened, so it goes when the waiting does. One left open is
  // the compiler answering a question nobody is reading.
  it('stops asking once nothing is waiting', () => {
    const { unmount } = renderWaiting()
    expect(progressSources()[0].readyState).toBe(1)
    unmount()
    expect(progressSources()[0].readyState).toBe(2)
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
