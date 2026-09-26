import React from 'react'
import { render, screen } from '@testing-library/react'
import { MemoryRouter } from 'react-router-dom'
import { ThemeProvider, createTheme } from '@mui/material/styles'
import { SimpleTreeView } from '@mui/x-tree-view/SimpleTreeView'
import Steps, { CallStepsContext } from '../Steps'

const theme = createTheme()

// A compiled /Catln/Doc/Show/Steps: the rows, as the cons chain a /Data/List is. Every cell is a
// String because the library renders them through /Data/toString before handing them over -- see the
// paragraph on Show/Step in stack/core/render.ct.
function str(s) {
  return { tag: 'StrVal', contents: s }
}

function step(index, input, output) {
  return {
    tag: 'TupleVal',
    name: '/Catln/Doc/Show/Step',
    args: { '/index': str(String(index)), '/input': str(input), '/output': str(output) },
  }
}

function list(items) {
  return items.reduceRight(
    (tail, head) => ({ tag: 'TupleVal', name: '/Data/List/Cons', args: { '/head': head, '/tail': tail } }),
    { tag: 'TupleVal', name: '/Data/List/Nil', args: {} }
  )
}

// Rows are TreeItems, so they need a tree around them the way the debugger gives them one.
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

describe('Steps', () => {
  it('draws the whole table, its heading and every row, inside one frame', () => {
    const steps = list([step(0, '1', '2'), step(1, '2', '3')])
    const { container } = render(<Providers><Steps {...{ '/steps': steps }} /></Providers>)

    // Framed so it reads as one display among the tree items around it, not as more of them.
    const frames = container.querySelectorAll('.catln-steps-table')
    expect(frames).toHaveLength(1)
    expect(frames[0].querySelector('.catln-steps-head')).not.toBeNull()
    expect(frames[0].querySelectorAll('[role="treeitem"]')).toHaveLength(2)
  })

  it('renders a row per step, in order, with both ends and the arrow', () => {
    const steps = list([step(0, '1', '2'), step(1, '2', '3'), step(2, '3', '4')])
    render(<Providers><Steps {...{ '/steps': steps }} /></Providers>)

    const rows = screen.getAllByRole('treeitem')
    expect(rows).toHaveLength(3)
    // The index is the row's place in the call rather than anything about the value, so it is shown
    // beside the two ends rather than instead of one of them.
    expect(rows[0].textContent).toContain('0')
    expect(rows[0].textContent).toContain('1')
    expect(rows[0].textContent).toContain('2')
    expect(rows[2].textContent).toContain('3')
    expect(rows[2].textContent).toContain('4')
    // Drawn here rather than carried in the step: a row points from one end to the other by being a
    // row, so the glyph is about the table and not about the call.
    expect(screen.getAllByText('↦').length).toBe(3)
  })

  it('names its columns once, above the rows, rather than per row', () => {
    render(<Providers><Steps {...{ '/steps': list([step(0, 'a', 'b')]) }} /></Providers>)

    // The heading is not a row: a table of one step is one thing that happened, not two.
    expect(screen.getAllByRole('treeitem')).toHaveLength(1)
    expect(screen.getByText('in')).toBeInTheDocument()
    expect(screen.getByText('out')).toBeInTheDocument()
  })

  it('says so for a call that took no steps rather than drawing an empty table', () => {
    render(<Providers><Steps {...{ '/steps': list([]) }} /></Providers>)

    expect(screen.queryAllByRole('treeitem')).toHaveLength(0)
    expect(screen.getByText(/No steps/)).toBeInTheDocument()
  })

  it('opens each row onto what the run recorded for it', async () => {
    const steps = list([step(0, '1', '2'), step(1, '2', '3')])
    const rowNodes = [<span key="a">first step ran</span>, <span key="b">second step ran</span>]
    // Opened, because a closed row keeps nothing in the page: MUI unmounts a collapsed item's
    // children, so what a row leads to can only be read once it is open.
    render(
      <Providers rowNodes={rowNodes} expanded={['step-0', 'step-1']}>
        <Steps {...{ '/steps': steps }} />
      </Providers>
    )

    const rows = screen.getAllByRole('treeitem')
    expect(rows[0].textContent).toContain('first step ran')
    expect(rows[1].textContent).toContain('second step ran')
    expect(rows[0].textContent).not.toContain('second step ran')
  })

  it('leaves rows closed where there is no run behind them', () => {
    // A step table read on a docs page rather than in a debugger: the same component, the same rows,
    // and nothing to open. It must not invent children or fail for want of them.
    render(<Providers><Steps {...{ '/steps': list([step(0, '1', '2')]) }} /></Providers>)

    const row = screen.getByRole('treeitem')
    expect(row.textContent).toContain('1')
    expect(row.querySelector('[role="group"]')).toBeNull()
  })
})
