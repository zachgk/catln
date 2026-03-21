import { buildPageTree, sortPageTree } from '../Docs'

describe('buildPageTree', () => {
  it('builds a flat tree for root-level files', () => {
    const tree = buildPageTree(['/main.ct', '/utils.ct'])
    expect(tree).toEqual({
      'main.ct': 1,
      'utils.ct': 1,
    })
  })

  it('builds nested tree from directory paths', () => {
    const tree = buildPageTree(['/test/main.ct', '/test/sub/nested.ct'])
    expect(tree).toEqual({
      test: {
        'main.ct': 1,
        sub: {
          'nested.ct': 1,
        },
      },
    })
  })

  it('merges files in the same directory', () => {
    const tree = buildPageTree(['/a/b.ct', '/a/c.ct', '/a/d/e.ct'])
    expect(tree).toEqual({
      a: {
        'b.ct': 1,
        'c.ct': 1,
        d: {
          'e.ct': 1,
        },
      },
    })
  })

  it('handles empty input', () => {
    expect(buildPageTree([])).toEqual({})
  })
})

describe('sortPageTree', () => {
  it('puts main.ct first', () => {
    const tree = { 'utils.ct': 1, 'main.ct': 1, 'other.ct': 1 }
    const pageList = []
    const sorted = sortPageTree(tree, pageList)
    expect(sorted[0].key).toBe('main.ct')
    expect(pageList[0]).toContain('main.ct')
  })

  it('populates pageList in order', () => {
    const tree = { 'a.ct': 1, 'b.ct': 1, 'c.ct': 1 }
    const pageList = []
    sortPageTree(tree, pageList)
    expect(pageList).toHaveLength(3)
    expect(pageList).toEqual(['/a.ct', '/b.ct', '/c.ct'])
  })

  it('produces dir and file types correctly', () => {
    const tree = {
      'main.ct': 1,
      sub: {
        'nested.ct': 1,
      },
    }
    const pageList = []
    const sorted = sortPageTree(tree, pageList)

    const mainEntry = sorted.find(e => e.key === 'main.ct')
    const dirEntry = sorted.find(e => e.key === 'sub')

    expect(mainEntry.type).toBe('file')
    expect(dirEntry.type).toBe('dir')
    expect(dirEntry.children).toHaveLength(1)
    expect(dirEntry.children[0].key).toBe('nested.ct')
  })

  it('recursively processes nested directories', () => {
    const tree = {
      a: {
        b: {
          'deep.ct': 1,
        },
      },
    }
    const pageList = []
    sortPageTree(tree, pageList)
    expect(pageList).toHaveLength(1)
    expect(pageList[0]).toContain('deep.ct')
  })
})
