// What an edit of one block of a day does to the blocks of the day, as the Obsidian Day Planner
// plugin works it out (its src/ui/hooks/use-edit/transform/edit-blocks.ts).
//
// Pure: given the blocks as they are, the one moved or resized and where it was put, it answers with
// every block that changed and where it is now. Nothing is written here; what changed is what the
// planner writes back.

// The shortest a block is made by an edit, which is the plugin's minimalDurationMinutes.
export const MINIMAL_MINUTES = 10;

const MINUTE = 60 * 1000;

// The ways an edit may treat the blocks around the one edited, as the plugin's edit modes do: leave
// them, push them out of its way, or shrink them to make room.
export const MODES = ['move', 'push', 'shrink'];

const overlaps = (a, b) => a.start < b.end && b.start < a.end;
const sameDay = (a, b) => a.start.toDateString() === b.start.toDateString();

// A block no shorter than the shortest an edit makes one: its end moved out where it is.
export function atLeastMinimal({start, end}) {
  const shortest = new Date(start.getTime() + MINIMAL_MINUTES * MINUTE);
  return {start, end: end < shortest ? shortest : end};
}

// Every block an edit changes, with where each is now: the one edited, and in the push and shrink
// modes each block of its day it now overlaps that it did not before.
//
// A block already overlapping the edited one before the edit is left alone, as the plugin leaves it:
// it was written that way, and an edit of something else is no reason to change it. A block is
// pushed or shrunk away from the side of the edited block it is on — one after it is moved or
// shortened later, one before it earlier — and a pushed block pushes the blocks after it in turn.
export function editBlocks(blocks, edited, span, mode = 'move') {
  const moved = {...edited, ...atLeastMinimal(span)};
  const changes = [moved];
  if (mode === 'move') {
    return changes;
  }
  const others = blocks.filter(b => b.id !== edited.id && !b.allDay && sameDay(b, moved) && !overlaps(b, edited));
  const after = others.filter(b => b.start >= edited.start).sort((a, b) => a.start - b.start);
  const before = others.filter(b => b.start < edited.start).sort((a, b) => b.start - a.start);

  if (mode === 'push') {
    let edge = moved.end;
    for (const b of after) {
      if (b.start >= edge) {
        break;
      }
      const shift = edge - b.start;
      const pushed = {...b, start: new Date(b.start.getTime() + shift), end: new Date(b.end.getTime() + shift)};
      changes.push(pushed);
      edge = pushed.end;
    }
    edge = moved.start;
    for (const b of before) {
      if (b.end <= edge) {
        break;
      }
      const shift = b.end - edge;
      const pushed = {...b, start: new Date(b.start.getTime() - shift), end: new Date(b.end.getTime() - shift)};
      changes.push(pushed);
      edge = pushed.start;
    }
    return changes;
  }

  // Shrink: the block is cut back to the edge of the edited one, but never below the shortest a block
  // is made — and one which would be is left as it is rather than made shorter than that.
  for (const b of after.filter(b => overlaps(b, moved))) {
    const start = moved.end;
    if (b.end - start >= MINIMAL_MINUTES * MINUTE) {
      changes.push({...b, start});
    }
  }
  for (const b of before.filter(b => overlaps(b, moved))) {
    const end = moved.start;
    if (end - b.start >= MINIMAL_MINUTES * MINUTE) {
      changes.push({...b, end});
    }
  }
  return changes;
}
