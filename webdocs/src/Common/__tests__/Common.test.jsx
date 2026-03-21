import React from 'react'
import { render, screen, renderHook, waitFor, act } from '@testing-library/react'
import { MemoryRouter } from 'react-router-dom'
import { ThemeProvider, createTheme } from '@mui/material/styles'
import {
  tagJoin,
  isTopType,
  useApi,
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

describe('isTopType', () => {
  it('returns true for bare TopType', () => {
    const t = { tag: 'TopType', contents: [{}, { tag: 'PredsAnd', contents: [] }] }
    expect(isTopType(t)).toBe(true)
  })

  it('returns false when topNeg is non-empty', () => {
    const t = { tag: 'TopType', contents: [{ '/Integer': [[]] }, { tag: 'PredsAnd', contents: [] }] }
    expect(isTopType(t)).toBe(false)
  })

  it('returns false when preds are non-empty', () => {
    const t = { tag: 'TopType', contents: [{}, { tag: 'PredsAnd', contents: [{ tag: 'PredsOne', contents: {} }] }] }
    expect(isTopType(t)).toBe(false)
  })

  it('returns false for non-PredsAnd preds', () => {
    const t = { tag: 'TopType', contents: [{}, { tag: 'PredsOne', contents: {} }] }
    expect(isTopType(t)).toBe(false)
  })

  it('returns false for non-TopType tags', () => {
    expect(isTopType({ tag: 'TypeVar', contents: [] })).toBe(false)
    expect(isTopType({ tag: 'UnionType', contents: {} })).toBe(false)
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
    const t = { tag: 'TopType', contents: [{}, { tag: 'PredsAnd', contents: [] }] }
    const { container } = render(<Providers><Type data={t} /></Providers>)
    expect(container.textContent).toBe('')
  })

  it('renders "Any" with negation for TopType with topNeg', () => {
    const t = {
      tag: 'TopType',
      contents: [
        { '/Integer': [[[], [], { tag: 'PredsAnd', contents: [] }, { tag: 'PtArgExact' }]] },
        { tag: 'PredsAnd', contents: [] },
      ],
    }
    const { container } = render(<Providers><Type data={t} /></Providers>)
    expect(container.textContent).toContain('Any')
    expect(container.textContent).toContain('Integer')
  })

  it('renders TypeVar with PartialKey', () => {
    const t = {
      tag: 'TypeVar',
      contents: [{ contents: { pkName: 'a', pkArgs: [], pkVars: [] } }],
    }
    const { container } = render(<Providers><Type data={t} /></Providers>)
    expect(container.textContent).toBe('a')
  })

  it('renders UnionType joined with " || "', () => {
    const t = {
      tag: 'UnionType',
      contents: {
        '/Integer': [[[], [], { tag: 'PredsAnd', contents: [] }, { tag: 'PtArgExact' }]],
        '/String': [[[], [], { tag: 'PredsAnd', contents: [] }, { tag: 'PtArgExact' }]],
      },
    }
    const { container } = render(<Providers><Type data={t} /></Providers>)
    expect(container.textContent).toContain('Integer')
    expect(container.textContent).toContain('String')
    expect(container.textContent).toContain(' || ')
  })
})

// --- PartialName ---

describe('PartialName', () => {
  it('renders PTypeName as link to /type/...', () => {
    render(<Providers><PartialName name={{ tag: 'PTypeName', contents: '/Integer' }} /></Providers>)
    const link = screen.getByRole('link', { name: '/Integer' })
    expect(link).toHaveAttribute('href', '/type/%2FInteger')
  })

  it('renders PClassName as link to /type/...', () => {
    render(<Providers><PartialName name={{ tag: 'PClassName', contents: '/Show' }} /></Providers>)
    const link = screen.getByRole('link', { name: '/Show' })
    expect(link).toHaveAttribute('href', '/type/%2FShow')
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
