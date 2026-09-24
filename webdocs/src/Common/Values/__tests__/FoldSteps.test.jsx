import React from 'react'
import { render, screen } from '@testing-library/react'
import { MemoryRouter } from 'react-router-dom'
import { ThemeProvider, createTheme } from '@mui/material/styles'
import { SimpleTreeView } from '@mui/x-tree-view/SimpleTreeView'
import FoldSteps from '../FoldSteps'
import { CallStepsContext } from '../Steps'

const theme = createTheme()

// A compiled /Catln/Doc/Show/FoldSteps, built the way the compiler sends one: every cell but the index
// a String the library already rendered, and the direction a Boolean constructor.
function str(s) {
  return { tag: 'StrVal', contents: s }
}

function bool(b) {
  return { tag: 'TupleVal', name: b ? '/Data/Primitive/True' : '/Data/Primitive/False', args: {} }
}

function step(index, acc, input, output) {
  return {
    tag: 'TupleVal',
    name: '/Catln/Doc/Show/FoldStep',
    args: { '/index': { tag: 'IntVal', contents: index }, '/acc': str(acc), '/input': str(input), '/output': str(output) },
  }
}

function list(items) {
  return items.reduceRight(
    (tail, head) => ({ tag: 'TupleVal', name: '/Data/List/Cons', args: { '/head': head, '/tail': tail } }),
    { tag: 'TupleVal', name: '/Data/List/Nil', args: {} }
  )
}

function fold(init, steps, result, fromRight) {
  return { '/init': str(init), '/steps': list(steps), '/result': str(result), '/fromRight': bool(fromRight) }
}

// foldl(lst= [1, 2, 3], fn(acc, v)= acc + v, init= 0)
const LEFT = fold('0', [step(0, '0', '1', '1'), step(1, '1', '2', '3'), step(2, '3', '3', '6')], '6', false)

// foldr(lst= [1, 2, 3], fn(acc, v)= acc + v, init= 0): the same rows in element order, each handed
// what the rows after it made.
const RIGHT = fold('0', [step(0, '5', '1', '6'), step(1, '3', '2', '5'), step(2, '0', '3', '3')], '6', true)

function Providers({ children, rowNodes, expanded }) {
  return (
    <ThemeProvider theme={theme}>
      <MemoryRouter>
        <CallStepsContext.Provider value={rowNodes || []}>
          <SimpleTreeView defaultExpandedItems={expanded || []}>{children}</SimpleTreeView>
        </CallStepsContext.Provider>
      </MemoryRouter>
    </ThemeProvider>
  )
}

// The lines of the table in the order they are drawn, as the text of each: the heading, the init and
// result lines, and the rows between them.
function lines(container) {
  return Array.from(container.querySelectorAll('.catln-steps-row')).map(row => row.textContent)
}

describe('FoldSteps', () => {
  it('renders a row per step with the accumulator, the element and what they made', () => {
    render(<Providers><FoldSteps {...LEFT} /></Providers>)

    const rows = screen.getAllByRole('treeitem')
    expect(rows).toHaveLength(3)
    expect(rows[1].textContent).toContain('1')
    expect(rows[1].textContent).toContain('2')
    expect(rows[1].textContent).toContain('3')
    expect(screen.getAllByText('⊕').length).toBe(3)
    expect(screen.getAllByText('↦').length).toBe(3)
  })

  it('draws a left fold from its init at the top down to its result at the bottom', () => {
    const { container } = render(<Providers><FoldSteps {...LEFT} /></Providers>)

    const drawn = lines(container)
    expect(drawn[1]).toMatch(/^init.*0↙$/)
    expect(drawn[drawn.length - 1]).toMatch(/=6$/)
    // Each output but the last goes into the accumulator of the row below; the last goes straight
    // down into the result.
    const rows = screen.getAllByRole('treeitem')
    expect(rows[0].textContent).toMatch(/↙$/)
    expect(rows[1].textContent).toMatch(/↙$/)
    expect(rows[2].textContent).toMatch(/↓$/)
  })

  it('draws a right fold from its init at the bottom up to its result at the top', () => {
    const { container } = render(<Providers><FoldSteps {...RIGHT} /></Providers>)

    const drawn = lines(container)
    expect(drawn[1]).toMatch(/=6$/)
    expect(drawn[drawn.length - 1]).toMatch(/^init.*0↖$/)
    // Rows stay in element order; it is the arrows that run the other way.
    const rows = screen.getAllByRole('treeitem')
    expect(rows[0].textContent).toMatch(/^0/)
    expect(rows[0].textContent).toMatch(/↑$/)
    expect(rows[1].textContent).toMatch(/↖$/)
    expect(rows[2].textContent).toMatch(/↖$/)
  })

  it('shows a fold over nothing as its init handed straight to its result', () => {
    const { container } = render(<Providers><FoldSteps {...fold('0', [], '0', false)} /></Providers>)

    expect(screen.queryAllByRole('treeitem')).toHaveLength(0)
    const drawn = lines(container)
    expect(drawn[1]).toMatch(/^init.*0↓$/)
    expect(drawn[2]).toMatch(/=0$/)
  })

  it('opens each row onto what the run recorded for it', () => {
    const rowNodes = [<span key="a">first step ran</span>, <span key="b">second step ran</span>, <span key="c">third step ran</span>]
    render(
      <Providers rowNodes={rowNodes} expanded={['step-0', 'step-1', 'step-2']}>
        <FoldSteps {...RIGHT} />
      </Providers>
    )

    // By element order in a right fold too, since that is the order the run's chain of calls is in.
    const rows = screen.getAllByRole('treeitem')
    expect(rows[0].textContent).toContain('first step ran')
    expect(rows[2].textContent).toContain('third step ran')
    expect(rows[0].textContent).not.toContain('third step ran')
  })
})
