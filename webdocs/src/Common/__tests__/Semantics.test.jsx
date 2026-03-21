import React from 'react'
import { render } from '@testing-library/react'
import { MemoryRouter } from 'react-router-dom'
import { ThemeProvider, createTheme } from '@mui/material/styles'
import { Expr, ObjArr } from '../Semantics'

const theme = createTheme()

function Providers({ children }) {
  return (
    <ThemeProvider theme={theme}>
      <MemoryRouter>{children}</MemoryRouter>
    </ThemeProvider>
  )
}

const baseMeta = {
  getMetaPos: null,
  getMetaType: { tag: 'TopType', contents: [{}, { tag: 'PredsAnd', contents: [] }] },
  getMetaDat: [null, null, null],
  getMetaID: 1,
}

const intMeta = {
  getMetaPos: null,
  getMetaType: { tag: 'UnionType', contents: { '/Integer': [[[], [], { tag: 'PredsAnd', contents: [] }, { tag: 'PtArgExact' }]] } },
  getMetaDat: [null, null, null],
  getMetaID: 2,
}

function NoopMeta() {
  return null
}

describe('Expr', () => {
  it('renders CExpr as constant value', () => {
    const expr = { tag: 'CExpr', contents: [baseMeta, { tag: 'IntVal', contents: 42 }] }
    const { container } = render(<Providers><Expr expr={expr} Meta={NoopMeta} /></Providers>)
    expect(container.textContent).toBe('42')
  })

  it('renders Value with empty name as "()"', () => {
    const expr = { tag: 'Value', contents: [baseMeta, ''] }
    const { container } = render(<Providers><Expr expr={expr} Meta={NoopMeta} /></Providers>)
    expect(container.textContent).toBe('()')
  })

  it('renders Value with name as a link', () => {
    const expr = { tag: 'Value', contents: [baseMeta, 'myVal'] }
    const { container } = render(<Providers><Expr expr={expr} Meta={NoopMeta} /></Providers>)
    expect(container.textContent).toBe('myVal')
    expect(container.querySelector('a')).toBeTruthy()
  })

  it('renders HoleExpr as "_"', () => {
    const expr = { tag: 'HoleExpr', contents: [baseMeta] }
    const { container } = render(<Providers><Expr expr={expr} Meta={NoopMeta} /></Providers>)
    expect(container.textContent).toBe('_')
  })

  it('renders TupleApply with anonymous base', () => {
    const expr = {
      tag: 'TupleApply',
      contents: [
        baseMeta,
        [baseMeta, { tag: 'Value', contents: [baseMeta, ''] }],
        {
          tag: 'EAppArg',
          contents: {
            oaObj: { tag: 'CExpr', contents: [baseMeta, { tag: 'IntVal', contents: 1 }] },
            oaArr: null,
            oaAnnots: [],
          },
        },
      ],
    }
    const { container } = render(<Providers><Expr expr={expr} Meta={NoopMeta} /></Providers>)
    expect(container.textContent).toContain('(')
    expect(container.textContent).toContain('1')
  })

  it('renders TupleApply with named base', () => {
    const expr = {
      tag: 'TupleApply',
      contents: [
        baseMeta,
        [baseMeta, { tag: 'Value', contents: [baseMeta, 'fn'] }],
        {
          tag: 'EAppArg',
          contents: {
            oaObj: { tag: 'CExpr', contents: [baseMeta, { tag: 'IntVal', contents: 5 }] },
            oaArr: null,
            oaAnnots: [],
          },
        },
      ],
    }
    const { container } = render(<Providers><Expr expr={expr} Meta={NoopMeta} /></Providers>)
    expect(container.textContent).toContain('fn')
    expect(container.textContent).toContain('5')
  })
})

describe('ObjArr', () => {
  it('renders obj and arr parts', () => {
    const oa = {
      oaObj: { tag: 'Value', contents: [baseMeta, 'x'] },
      oaArr: [
        { tag: 'CExpr', contents: [baseMeta, { tag: 'IntVal', contents: 10 }] },
        intMeta,
      ],
      oaAnnots: [],
    }
    const { container } = render(<Providers><ObjArr oa={oa} Meta={NoopMeta} /></Providers>)
    expect(container.textContent).toContain('x')
    expect(container.textContent).toContain('10')
  })

  it('shows type annotation when arr type is not TopType', () => {
    const oa = {
      oaObj: { tag: 'Value', contents: [baseMeta, 'x'] },
      oaArr: [null, intMeta],
      oaAnnots: [],
    }
    const { container } = render(<Providers><ObjArr oa={oa} Meta={NoopMeta} /></Providers>)
    expect(container.textContent).toContain('Integer')
    expect(container.textContent).toContain('->')
  })

  it('hides type annotation when arr type is TopType', () => {
    const oa = {
      oaObj: { tag: 'Value', contents: [baseMeta, 'x'] },
      oaArr: [null, baseMeta],
      oaAnnots: [],
    }
    const { container } = render(<Providers><ObjArr oa={oa} Meta={NoopMeta} /></Providers>)
    expect(container.textContent).not.toContain('->')
  })
})
