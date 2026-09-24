import React from 'react'
import { render, screen } from '@testing-library/react'
import userEvent from '@testing-library/user-event'
import { MemoryRouter } from 'react-router-dom'
import { ThemeProvider, createTheme } from '@mui/material/styles'
import { Expr, ObjArr, TypesOnHoverContext } from '../Semantics'

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
  getMetaType: { tag: 'UnionType', utTop: [{ tag: 'PredsAnd', paPreds: []}, {}, []], utLeafs: {}, utConsts: []},
  getMetaDat: [null, null, null],
  getMetaId: 1,
}

const intMeta = {
  getMetaPos: null,
  getMetaType: { tag: 'UnionType', utTop: null, utLeafs: { '/Integer': [[[], [], { tag: 'PredsAnd', paPreds: []}, { ptArgLb: true, ptArgUb: true }]] }, utConsts: []},
  getMetaDat: [null, null, null],
  getMetaId: 2,
}

function NoopMeta() {
  return null
}

describe('Expr', () => {
  it('renders CExpr as constant value', () => {
    const expr = { tag: 'CExpr', ceMeta: baseMeta, ceConst: { tag: 'CInt', ciValue: 42 }}
    const { container } = render(<Providers><Expr expr={expr} Meta={NoopMeta} /></Providers>)
    expect(container.textContent).toBe('42')
  })

  it('renders Value with empty name as "()"', () => {
    const expr = { tag: 'Value', veMeta: baseMeta, veName: ''}
    const { container } = render(<Providers><Expr expr={expr} Meta={NoopMeta} /></Providers>)
    expect(container.textContent).toBe('()')
  })

  it('renders Value with name as a link', () => {
    const expr = { tag: 'Value', veMeta: baseMeta, veName: 'myVal'}
    const { container } = render(<Providers><Expr expr={expr} Meta={NoopMeta} /></Providers>)
    expect(container.textContent).toBe('myVal')
    expect(container.querySelector('a')).toBeTruthy()
  })

  it('renders HoleExpr as "_"', () => {
    const expr = { tag: 'HoleExpr', heMeta: baseMeta, heHole: { tag: 'HoleUndefined' } }
    const { container } = render(<Providers><Expr expr={expr} Meta={NoopMeta} /></Providers>)
    expect(container.textContent).toBe('_')
  })

  it('renders TupleApply with anonymous base', () => {
    const expr = { tag: 'TupleApply', taMeta: baseMeta, taBase: [baseMeta, { tag: 'Value', veMeta: baseMeta, veName: ''}], taApp: { tag: 'EAppArg', eaaArg: {
            oaObj: { tag: 'CExpr', ceMeta: baseMeta, ceConst: { tag: 'CInt', ciValue: 1 }},
            oaArr: null,
            oaAnnots: [],
          },
        },
    }
    const { container } = render(<Providers><Expr expr={expr} Meta={NoopMeta} /></Providers>)
    expect(container.textContent).toContain('(')
    expect(container.textContent).toContain('1')
  })

  it('renders TupleApply with named base', () => {
    const expr = { tag: 'TupleApply', taMeta: baseMeta, taBase: [baseMeta, { tag: 'Value', veMeta: baseMeta, veName: 'fn'}], taApp: { tag: 'EAppArg', eaaArg: {
            oaObj: { tag: 'CExpr', ceMeta: baseMeta, ceConst: { tag: 'CInt', ciValue: 5 }},
            oaArr: null,
            oaAnnots: [],
          },
        },
    }
    const { container } = render(<Providers><Expr expr={expr} Meta={NoopMeta} /></Providers>)
    expect(container.textContent).toContain('fn')
    expect(container.textContent).toContain('5')
  })
})

describe('ObjArr', () => {
  it('renders obj and arr parts', () => {
    const oa = {
      oaObj: { tag: 'Value', veMeta: baseMeta, veName: 'x'},
      oaArr: [
        { tag: 'CExpr', ceMeta: baseMeta, ceConst: { tag: 'CInt', ciValue: 10 }},
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
      oaObj: { tag: 'Value', veMeta: baseMeta, veName: 'x'},
      oaArr: [null, intMeta],
      oaAnnots: [],
    }
    const { container } = render(<Providers><ObjArr oa={oa} Meta={NoopMeta} /></Providers>)
    expect(container.textContent).toContain('Integer')
    expect(container.textContent).toContain('->')
  })

  it('hides type annotation when arr type is TopType', () => {
    const oa = {
      oaObj: { tag: 'Value', veMeta: baseMeta, veName: 'x'},
      oaArr: [null, baseMeta],
      oaAnnots: [],
    }
    const { container } = render(<Providers><ObjArr oa={oa} Meta={NoopMeta} /></Providers>)
    expect(container.textContent).not.toContain('->')
  })
})

describe('ObjArr with types on hover', () => {
  function Hover({ children }) {
    return <Providers><TypesOnHoverContext.Provider value={true}>{children}</TypesOnHoverContext.Provider></Providers>
  }

  const typedX = {
    oaObj: { tag: 'Value', veMeta: baseMeta, veName: 'x'},
    oaArr: [null, intMeta],
    oaAnnots: [],
  }

  it('leaves the type out of the text', () => {
    const { container } = render(<Hover><ObjArr oa={typedX} Meta={NoopMeta} /></Hover>)
    expect(container.textContent).toBe('x')
  })

  it('shows the type when its name is hovered', async () => {
    const { container } = render(<Hover><ObjArr oa={typedX} Meta={NoopMeta} /></Hover>)
    await userEvent.hover(container.querySelector('.catln-typed'))
    expect(await screen.findByRole('tooltip')).toHaveTextContent('/Integer')
  })

  it('puts no hover on a name whose type says nothing', () => {
    const oa = { ...typedX, oaArr: [null, baseMeta] }
    const { container } = render(<Hover><ObjArr oa={oa} Meta={NoopMeta} /></Hover>)
    expect(container.querySelector('.catln-typed')).toBeNull()
  })

  // A call and each of its arguments are hovered separately, so that pointing at an argument shows
  // that argument's type and not the call's as well.
  it('gives a call and its argument a hover each, neither inside the other', async () => {
    const strMeta = {
      ...intMeta,
      getMetaType: { tag: 'UnionType', utTop: null, utLeafs: { '/String': [[[], [], { tag: 'PredsAnd', paPreds: []}, { ptArgLb: true, ptArgUb: true }]] }, utConsts: []},
    }
    const oa = {
      oaObj: {
        tag: 'TupleApply',
        taMeta: baseMeta,
        taBase: [baseMeta, { tag: 'Value', veMeta: baseMeta, veName: 'f'}],
        taApp: { tag: 'EAppArg', eaaArg: typedX },
      },
      oaArr: [null, strMeta],
      oaAnnots: [],
    }
    const { container } = render(<Hover><ObjArr oa={oa} Meta={NoopMeta} /></Hover>)
    expect(container.textContent).toBe('f(x)')

    const typed = container.querySelectorAll('.catln-typed')
    expect([...typed].map(t => t.textContent)).toEqual(['f', 'x'])

    await userEvent.hover(typed[1])
    const tip = await screen.findByRole('tooltip')
    expect(tip).toHaveTextContent('/Integer')
    expect(tip).not.toHaveTextContent('/String')
  })
})
