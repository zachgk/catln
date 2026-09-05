import React from 'react'
import { render, screen } from '@testing-library/react'
import userEvent from '@testing-library/user-event'
import { ThemeProvider, createTheme } from '@mui/material/styles'
import Json from '../Json'

const theme = createTheme()

// A compiled /Data/Json, built the way the evaluator serialises one: a TupleVal per arm, its
// arguments keyed by the field name with a leading slash.
const jnull = () => ({ tag: 'TupleVal', name: '/Data/Json/Null', args: {} })
const jbool = b => ({ tag: 'TupleVal', name: '/Data/Json/Bool', args: { '/value': { tag: 'TupleVal', name: b ? '/Data/Primitive/True' : '/Data/Primitive/False', args: {} } } })
const jnum = n => ({ tag: 'TupleVal', name: '/Data/Json/Number', args: { '/value': { tag: 'FloatVal', contents: n } } })
const jstr = s => ({ tag: 'TupleVal', name: '/Data/Json/Str', args: { '/value': { tag: 'StrVal', contents: s } } })

// A /Data/List, which is what a container carries its children in: a Cons chain ending in a Nil.
function list(items) {
  return items.reduceRight(
    (tail, head) => ({ tag: 'TupleVal', name: '/Data/List/Cons', args: { '/head': head, '/tail': tail } }),
    { tag: 'TupleVal', name: '/Data/List/Nil', args: {} }
  )
}

const jarr = entries => ({ tag: 'TupleVal', name: '/Data/Json/Array', args: { '/entries': list(entries) } })
const jmember = (key, value) => ({ tag: 'TupleVal', name: '/Data/Json/Member', args: { '/key': { tag: 'StrVal', contents: key }, '/value': value } })
const jobj = members => ({ tag: 'TupleVal', name: '/Data/Json/Object', args: { '/entries': list(members) } })

function Providers({ children }) {
  return <ThemeProvider theme={theme}>{children}</ThemeProvider>
}

function show(value) {
  return render(<Providers><Json data={value} /></Providers>)
}

describe('Json', () => {
  beforeEach(() => {
    global.fetch = vi.fn(() => Promise.reject(new Error('Json must not fetch')))
  })

  it('shows each of the four scalar kinds as what it is', () => {
    show(jobj([
      jmember('s', jstr('hello')),
      jmember('n', jnum(1.5)),
      jmember('b', jbool(true)),
      jmember('z', jnull())
    ]))

    expect(screen.getByText('"hello"')).toBeInTheDocument()
    expect(screen.getByText('1.5')).toBeInTheDocument()
    expect(screen.getByText('true')).toBeInTheDocument()
    expect(screen.getByText('null')).toBeInTheDocument()
    expect(global.fetch).not.toHaveBeenCalled()
  })

  // JSON has one numeric kind, so every number arrives as a float. A whole one is shown as it was
  // written rather than with the `.0` the Catln value carries.
  it('shows a whole number without a decimal point', () => {
    show(jarr([jnum(2020)]))

    expect(screen.getByText('2020')).toBeInTheDocument()
    expect(screen.queryByText('2020.0')).not.toBeInTheDocument()
  })

  it('shows a member under the key it was written with', () => {
    show(jobj([jmember('name', jstr('catln'))]))

    expect(screen.getByText('name')).toBeInTheDocument()
    expect(screen.getByText('"catln"')).toBeInTheDocument()
  })

  it('says what a container holds without it being opened', () => {
    show(jobj([jmember('tags', jarr([jstr('a'), jstr('b'), jstr('c')]))]))

    // The object's own count, and the array's, both on the rows that fold them.
    expect(screen.getByText('{1}')).toBeInTheDocument()
    expect(screen.getByText('[3]')).toBeInTheDocument()
  })

  it('shows an empty container as the empty container it is, not as a scalar', () => {
    show(jobj([jmember('none', jarr([])), jmember('nothing', jobj([]))]))

    expect(screen.getByText('[0]')).toBeInTheDocument()
    expect(screen.getByText('{0}')).toBeInTheDocument()
  })

  // The root and its immediate children are open; everything below is folded. A document opened
  // fully expanded is the thing folding exists to avoid.
  it('folds what is below the first level, and unfolds it when asked', async () => {
    const user = userEvent.setup()
    show(jobj([jmember('author', jobj([jmember('name', jstr('Zach'))]))]))

    expect(screen.getByText('author')).toBeInTheDocument()
    expect(screen.queryByText('"Zach"')).not.toBeInTheDocument()

    await user.click(screen.getByText('author'))

    expect(screen.getByText('"Zach"')).toBeInTheDocument()
  })

  // Two entries of one array are two rows, which they are not if their ids collide.
  it('keeps entries of one array apart', () => {
    show(jarr([jstr('a'), jstr('b')]))

    expect(screen.getByText('"a"')).toBeInTheDocument()
    expect(screen.getByText('"b"')).toBeInTheDocument()
  })

  // Two members written under one key is what a JSON file may hold and a tree must survive: they are
  // told apart by where they sit rather than by what they are called.
  it('keeps two members of one key apart', async () => {
    const user = userEvent.setup()
    show(jobj([
      jmember('dup', jobj([jmember('first', jstr('one'))])),
      jmember('dup', jobj([jmember('second', jstr('two'))]))
    ]))

    expect(screen.getAllByText('dup')).toHaveLength(2)

    await user.click(screen.getAllByText('dup')[0])

    expect(screen.getByText('"one"')).toBeInTheDocument()
    expect(screen.queryByText('"two"')).not.toBeInTheDocument()
  })

  // A member reached on its own -- which the dispatch in Value.jsx allows -- is the row it would be
  // inside its object, rather than a value with no name.
  it('shows a member reached on its own as its key over its value', () => {
    show(jmember('version', jnum(1.5)))

    expect(screen.getByText('version')).toBeInTheDocument()
    expect(screen.getByText('1.5')).toBeInTheDocument()
  })
})
