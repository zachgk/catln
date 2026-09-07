import { render } from '../../../../stack/json/json.js'

// The display `stack/json/json.ct` ships for `/Data/Json`, tested as the library file it is.
//
// Nothing here imports anything of the doc server, because the display does not: it is handed an
// element and a serialized value and builds the DOM itself. That is the whole of the contract a
// `/Catln/Doc/Show/Rendered` names, so this is what proves a library can hold its own display.
//
// These cases are the ones that were written against the React component this replaced. They are
// kept as they were so that what moved is the implementation and not the behaviour.

const nil = { tag: 'TupleVal', name: '/Data/List/Nil', args: {} }
const cons = (head, tail) => ({ tag: 'TupleVal', name: '/Data/List/Cons', args: { '/head': head, '/tail': tail } })
const list = items => items.reduceRight((tail, item) => cons(item, tail), nil)

const jnull = { tag: 'TupleVal', name: '/Data/Json/Null', args: {} }
const jbool = b => ({
  tag: 'TupleVal',
  name: '/Data/Json/Bool',
  args: { '/value': { tag: 'TupleVal', name: b ? '/Data/Primitive/True' : '/Data/Primitive/False', args: {} } }
})
const jnum = n => ({ tag: 'TupleVal', name: '/Data/Json/Number', args: { '/value': { tag: 'FloatVal', contents: n } } })
const jstr = s => ({ tag: 'TupleVal', name: '/Data/Json/Str', args: { '/value': { tag: 'StrVal', contents: s } } })
const jarr = entries => ({ tag: 'TupleVal', name: '/Data/Json/Array', args: { '/entries': list(entries) } })
const jmember = (key, value) => ({ tag: 'TupleVal', name: '/Data/Json/Member', args: { '/key': { tag: 'StrVal', contents: key }, '/value': value } })
const jobj = members => ({ tag: 'TupleVal', name: '/Data/Json/Object', args: { '/entries': list(members) } })

// Render into a fresh element, as the doc server does, and answer with it.
function show(value) {
  const root = document.createElement('div')
  document.body.appendChild(root)
  render(root, value)
  return root
}

// The rows a reader can actually see. A folded row is still in the tree, hidden by the list holding
// it, so "shown" has to mean not sitting under a hidden list rather than merely present.
function visibleText(root) {
  const rows = [...root.querySelectorAll('.catln-json-row')]
  return rows.filter(row => !row.closest('[hidden]')).map(row => row.textContent)
}

function rowsWithin(root) {
  return [...root.querySelectorAll('.catln-json-row')]
}

afterEach(() => {
  document.body.replaceChildren()
})

describe('the JSON display a library ships', () => {
  it('shows each of the four scalar kinds as what it is', () => {
    const root = show(jobj([
      jmember('s', jstr('catln')),
      jmember('n', jnum(1.5)),
      jmember('b', jbool(true)),
      jmember('z', jnull)
    ]))

    const text = visibleText(root).join(' ')
    expect(text).toContain('"catln"')
    expect(text).toContain('1.5')
    expect(text).toContain('true')
    expect(text).toContain('null')
  })

  // JSON has one numeric kind, so the importer writes every number as a float. A whole one must not
  // come back reading as `2.0`, which is not what the file said.
  it('shows a whole number without a point', () => {
    const root = show(jobj([jmember('n', jnum(2))]))

    expect(visibleText(root).join(' ')).toContain('2')
    expect(visibleText(root).join(' ')).not.toContain('2.0')
  })

  it('shows a member under the key it was written at', () => {
    const root = show(jobj([jmember('name', jstr('catln'))]))

    const keys = [...root.querySelectorAll('.catln-json-key')].map(k => k.textContent)
    expect(keys).toContain('name')
  })

  // What a container holds, on the row that folds it, so a reader can decide whether to open it
  // without opening it.
  it('says what a container holds without opening it', () => {
    const root = show(jobj([jmember('tags', jarr([jstr('a'), jstr('b'), jstr('c')]))]))

    const text = visibleText(root).join(' ')
    expect(text).toContain('{1}')
    expect(text).toContain('[3]')
  })

  // Asked rather than inferred from whether there are children: an empty array and an empty object
  // have none and are still containers.
  it('shows an empty container as the empty container it is', () => {
    expect(visibleText(show(jarr([]))).join(' ')).toContain('[0]')
    expect(visibleText(show(jobj([]))).join(' ')).toContain('{0}')
  })

  // The root alone is open to begin with: everything expanded is what folding exists to avoid, and
  // everything folded is one row that says nothing about what is in the file.
  it('opens at the root and folds what is below it', () => {
    const root = show(jobj([jmember('outer', jobj([jmember('inner', jstr('deep'))]))]))

    expect(visibleText(root).join(' ')).toContain('outer')
    expect(visibleText(root).join(' ')).not.toContain('deep')
  })

  it('unfolds a row when it is clicked, and folds it again', () => {
    const root = show(jobj([jmember('outer', jobj([jmember('inner', jstr('deep'))]))]))

    const outer = rowsWithin(root).find(r => r.textContent.includes('outer'))
    outer.querySelector('.catln-json-toggle').click()
    expect(visibleText(root).join(' ')).toContain('deep')

    outer.querySelector('.catln-json-toggle').click()
    expect(visibleText(root).join(' ')).not.toContain('deep')
  })

  // Two nodes sharing an id is a tree that folds one of them when the reader clicks the other, which
  // is why the id is built from where a node sits rather than from what it holds.
  it('keeps two entries of one array apart', () => {
    const root = show(jarr([jarr([jstr('first')]), jarr([jstr('second')])]))

    const containers = rowsWithin(root).filter(r => r.textContent.includes('[1]'))
    expect(containers).toHaveLength(2)

    containers[0].querySelector('.catln-json-toggle').click()
    const text = visibleText(root).join(' ')
    expect(text).toContain('"first"')
    expect(text).not.toContain('"second"')
  })

  it('keeps two members written under one key apart', () => {
    const root = show(jobj([
      jmember('same', jarr([jstr('first')])),
      jmember('same', jarr([jstr('second')]))
    ]))

    const containers = rowsWithin(root).filter(r => r.textContent.includes('[1]'))
    expect(containers).toHaveLength(2)

    containers[0].querySelector('.catln-json-toggle').click()
    const text = visibleText(root).join(' ')
    expect(text).toContain('"first"')
    expect(text).not.toContain('"second"')
  })

  // A member reached on its own is shown as the row it would be inside the object holding it, rather
  // than as a nameless value.
  it('shows a member reached on its own as its key over what is written there', () => {
    const root = show(jmember('title', jstr('catln')))

    const text = visibleText(root).join(' ')
    expect(text).toContain('title')
    expect(text).toContain('"catln"')
  })

  // A value that is not one of the six is a page the compiler built and this did not understand. A
  // reader is better told that than shown an empty row.
  it('shows a value it does not recognise rather than swallowing it', () => {
    const root = show({ tag: 'TupleVal', name: '/Data/Json/Other', args: {} })

    expect(visibleText(root).join(' ')).toContain('/Data/Json/Other')
  })

  // Two documents on one page fold independently, which is what the component's own state gave
  // before and what a store keyed by path alone would have taken away.
  it('folds two documents on one page independently', () => {
    const doc = () => jobj([jmember('outer', jobj([jmember('inner', jstr('deep'))]))])
    const first = show(doc())
    const second = show(doc())

    rowsWithin(first).find(r => r.textContent.includes('outer'))
      .querySelector('.catln-json-toggle').click()

    expect(visibleText(first).join(' ')).toContain('deep')
    expect(visibleText(second).join(' ')).not.toContain('deep')
  })
})
