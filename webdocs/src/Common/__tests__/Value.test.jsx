import React from 'react'
import { render, screen, fireEvent } from '@testing-library/react'
import { MemoryRouter } from 'react-router-dom'
import { ThemeProvider, createTheme } from '@mui/material/styles'
import { TocContext } from '../Common'
import { Value, tryValue } from '../Value'

const theme = createTheme()

const tocData = {
  isLoaded: true,
  data: { 'test/main.ct': { impDir: '/test' } },
  notes: [],
}

function Providers({ children }) {
  return (
    <ThemeProvider theme={theme}>
      <TocContext.Provider value={tocData}>
        <MemoryRouter>{children}</MemoryRouter>
      </TocContext.Provider>
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

describe('CatlnResult via Value', () => {
  it('renders .ll files with syntax highlighting', () => {
    const val = {
      tag: 'TupleVal',
      name: '/Catln/CatlnResult',
      args: {
        '/name': { tag: 'StrVal', contents: 'output.ll' },
        '/contents': { tag: 'StrVal', contents: 'define i32 @main()' },
      },
    }
    const { container } = render(<Providers><Value data={val} /></Providers>)
    // SyntaxHighlighter renders a <pre> with code
    expect(container.querySelector('pre')).toBeTruthy()
    expect(container.textContent).toContain('define i32 @main()')
  })

  it('renders .html files in an iframe', () => {
    const val = {
      tag: 'TupleVal',
      name: '/Catln/CatlnResult',
      args: {
        '/name': { tag: 'StrVal', contents: 'page.html' },
        '/contents': { tag: 'StrVal', contents: '<h1>Hello</h1>' },
      },
    }
    const { container } = render(<Providers><Value data={val} /></Providers>)
    const iframe = container.querySelector('iframe')
    expect(iframe).toBeTruthy()
    expect(iframe.getAttribute('srcdoc')).toBe('<h1>Hello</h1>')
  })

  it('renders other files in a <pre> tag', () => {
    const val = {
      tag: 'TupleVal',
      name: '/Catln/CatlnResult',
      args: {
        '/name': { tag: 'StrVal', contents: 'output.txt' },
        '/contents': { tag: 'StrVal', contents: 'plain text' },
      },
    }
    const { container } = render(<Providers><Value data={val} /></Providers>)
    expect(container.querySelector('pre')).toHaveTextContent('plain text')
  })
})

// The display components take update alongside the value's own args, which is what lets an
// interactive one edit the expression it was compiled from.
const listProgramVal = {
  tag: 'TupleVal',
  name: '/Catln/Doc/Show/ListProgram',
  args: {
    '/mode': { tag: 'TupleVal', name: '/Catln/Doc/Typecheck', args: {} },
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
