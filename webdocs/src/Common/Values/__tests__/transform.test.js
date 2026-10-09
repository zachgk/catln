import { editBlocks, atLeastMinimal } from '../transform'

const at = (hours, minutes = 0) => { const d = new Date(2026, 0, 3); d.setHours(hours, minutes, 0, 0); return d }
const block = (id, from, to) => ({ id, allDay: false, start: at(...from), end: at(...to) })
const spans = changes => changes.map(c => [c.id, c.start.getHours() * 60 + c.start.getMinutes(), c.end.getHours() * 60 + c.end.getMinutes()])

describe('editBlocks', () => {
  const a = block('a', [9], [10])
  const b = block('b', [10], [11])
  const c = block('c', [11], [12])
  const blocks = [a, b, c]

  it('moves only the edited block in the plain mode', () => {
    expect(spans(editBlocks(blocks, a, { start: at(9, 30), end: at(10, 30) }))).toEqual([['a', 570, 630]])
  })

  // A block moved over the next pushes it, and that one pushes the one after it in turn.
  it('pushes the blocks it now overlaps out of its way', () => {
    expect(spans(editBlocks(blocks, a, { start: at(9, 30), end: at(10, 30) }, 'push'))).toEqual([
      ['a', 570, 630], ['b', 630, 690], ['c', 690, 750],
    ])
  })

  it('pushes a block before it earlier', () => {
    expect(spans(editBlocks(blocks, c, { start: at(10, 30), end: at(11, 30) }, 'push'))).toEqual([
      ['c', 630, 690], ['b', 570, 630], ['a', 510, 570],
    ])
  })

  it('shrinks the block it now overlaps to make room', () => {
    expect(spans(editBlocks(blocks, a, { start: at(9), end: at(10, 20) }, 'shrink'))).toEqual([
      ['a', 540, 620], ['b', 620, 660],
    ])
  })

  // Shrinking never makes a block shorter than the shortest an edit makes one.
  it('does not shrink a block below the shortest a block is', () => {
    expect(spans(editBlocks(blocks, a, { start: at(9), end: at(10, 55) }, 'shrink'))).toEqual([['a', 540, 655]])
  })

  // A block which already overlapped the edited one was written that way, and is left as it was.
  it('leaves a block which already overlapped it alone', () => {
    const overlapping = block('o', [9, 30], [10, 30])
    expect(spans(editBlocks([a, overlapping], a, { start: at(9, 15), end: at(10, 15) }, 'push'))).toEqual([['a', 555, 615]])
  })

  it('makes an edited block no shorter than ten minutes', () => {
    const { end } = atLeastMinimal({ start: at(9), end: at(9, 5) })
    expect(end.getMinutes()).toBe(10)
  })
})
