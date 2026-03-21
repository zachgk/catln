import React from 'react'
import { render } from '@testing-library/react'
import { MemoryRouter } from 'react-router-dom'
import { ThemeProvider, createTheme } from '@mui/material/styles'
import { rawExprMeta, RawExpr } from '../Syntax'

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

// --- rawExprMeta ---

describe('rawExprMeta', () => {
  it('extracts meta from RawTheExpr (recursive)', () => {
    const inner = { tag: 'RawCExpr', contents: [baseMeta, { tag: 'IntVal', contents: 1 }] }
    const expr = { tag: 'RawTheExpr', contents: inner }
    expect(rawExprMeta(expr)).toEqual(baseMeta)
  })

  it('extracts meta from RawAliasExpr', () => {
    const inner = { tag: 'RawCExpr', contents: [baseMeta, { tag: 'IntVal', contents: 1 }] }
    const alias = { tag: 'RawAliasExpr', contents: [inner, inner] }
    expect(rawExprMeta(alias)).toEqual(baseMeta)
  })

  it('extracts meta from RawParen', () => {
    const inner = { tag: 'RawCExpr', contents: [baseMeta, { tag: 'IntVal', contents: 1 }] }
    const paren = { tag: 'RawParen', contents: inner }
    expect(rawExprMeta(paren)).toEqual(baseMeta)
  })

  it('returns contents[0] for leaf expressions', () => {
    const expr = { tag: 'RawCExpr', contents: [baseMeta, { tag: 'IntVal', contents: 42 }] }
    expect(rawExprMeta(expr)).toEqual(baseMeta)
  })
})

// --- RawExpr ---

describe('RawExpr', () => {
  it('renders RawCExpr as its constant value', () => {
    const expr = { tag: 'RawCExpr', contents: [baseMeta, { tag: 'IntVal', contents: 42 }] }
    const { container } = render(<Providers><RawExpr expr={expr} /></Providers>)
    expect(container.textContent).toBe('42')
  })

  it('renders RawValue with empty string as "()"', () => {
    const expr = { tag: 'RawValue', contents: [baseMeta, ''] }
    const { container } = render(<Providers><RawExpr expr={expr} /></Providers>)
    expect(container.textContent).toBe('()')
  })

  it('renders RawValue with name as a link', () => {
    const expr = { tag: 'RawValue', contents: [baseMeta, '/Integer'] }
    const { container } = render(<Providers><RawExpr expr={expr} /></Providers>)
    expect(container.textContent).toBe('/Integer')
    expect(container.querySelector('a')).toHaveAttribute('href', '/type/%2FInteger')
  })

  it('renders RawHoleExpr as "_"', () => {
    const expr = { tag: 'RawHoleExpr', contents: [baseMeta] }
    const { container } = render(<Providers><RawExpr expr={expr} /></Providers>)
    expect(container.textContent).toBe('_')
  })

  it('renders RawTupleApply with operator as infix', () => {
    const mkVal = (name) => ({ tag: 'RawValue', contents: [baseMeta, name] })
    const mkArg = (expr) => [false, { roaObj: null, roaArr: [expr, null], roaDef: null }]

    const expr = {
      tag: 'RawTupleApply',
      contents: [baseMeta, [baseMeta, mkVal('/operator+')], [mkArg(mkVal('a')), mkArg(mkVal('b'))]],
    }
    const { container } = render(<Providers><RawExpr expr={expr} /></Providers>)
    expect(container.textContent).toContain('+')
    expect(container.textContent).toContain('a')
    expect(container.textContent).toContain('b')
  })

  it('renders RawTupleApply anonymous tuple as (args)', () => {
    const mkArg = (expr) => [false, { roaObj: null, roaArr: [expr, null], roaDef: null }]
    const val = { tag: 'RawCExpr', contents: [baseMeta, { tag: 'IntVal', contents: 1 }] }

    const expr = {
      tag: 'RawTupleApply',
      contents: [baseMeta, [baseMeta, { tag: 'RawValue', contents: [baseMeta, ''] }], [mkArg(val)]],
    }
    const { container } = render(<Providers><RawExpr expr={expr} /></Providers>)
    expect(container.textContent).toContain('(')
    expect(container.textContent).toContain('1')
    expect(container.textContent).toContain(')')
  })

  it('renders RawTupleApply with named function as f(args)', () => {
    const mkArg = (expr) => [false, { roaObj: null, roaArr: [expr, null], roaDef: null }]
    const val = { tag: 'RawCExpr', contents: [baseMeta, { tag: 'IntVal', contents: 1 }] }
    const fn = { tag: 'RawValue', contents: [baseMeta, 'myFunc'] }

    const expr = {
      tag: 'RawTupleApply',
      contents: [baseMeta, [baseMeta, fn], [mkArg(val)]],
    }
    const { container } = render(<Providers><RawExpr expr={expr} /></Providers>)
    expect(container.textContent).toContain('myFunc')
    expect(container.textContent).toContain('(')
    expect(container.textContent).toContain('1')
  })

  it('renders RawParen with parens', () => {
    const inner = { tag: 'RawCExpr', contents: [baseMeta, { tag: 'IntVal', contents: 5 }] }
    const expr = { tag: 'RawParen', contents: inner }
    const { container } = render(<Providers><RawExpr expr={expr} /></Providers>)
    expect(container.textContent).toBe('(5)')
  })

  it('renders RawList as [items]', () => {
    const item1 = { tag: 'RawCExpr', contents: [baseMeta, { tag: 'IntVal', contents: 1 }] }
    const item2 = { tag: 'RawCExpr', contents: [baseMeta, { tag: 'IntVal', contents: 2 }] }
    const expr = { tag: 'RawList', contents: [baseMeta, [item1, item2]] }
    const { container } = render(<Providers><RawExpr expr={expr} /></Providers>)
    expect(container.textContent).toBe('[1, 2]')
  })
})
