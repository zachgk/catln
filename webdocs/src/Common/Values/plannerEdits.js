// What an edit of a day planner writes back, worked out apart from drawing one: which blocks it
// changes and the edits of the pages they were declared in that say so.
//
// Kept apart from the planner itself so that what an edit writes can be read without a page to draw
// it on — by the planner, and by a test of what the backend makes of it.

import {startOfDay} from 'date-fns';

import {dateValue, nothing, some, timeRangeValue, valueSource, withField} from './source';
import {editBlocks} from './transform';

// How long a block is given where it is given a time and nothing says how long: an entry dragged
// out of the unscheduled strip onto the day. The plugin's defaultDurationMinutes.
export const DEFAULT_MINUTES = 30;

// The value a bullet the Day Planner reads is read into. An event always has a time, which is what
// tells it apart from a task: it is written with its span and not with a span it may not have.
export const NOTE_EVENT = '/Data/Note/Event';

// The value an entry is written back as once it is put somewhere else: given the span of the day it
// now takes up, or none where it was put across the top of a day, and the day it is now on.
//
// Only the planner's fields change. Which of them the line is written back from, and how, is the
// importer's to say — a moved block's time is written over the one its line wrote, and a block moved
// to another day is moved to that day's note — so the value goes back whole with those two changed.
export function placedValue(value, block) {
  const isEvent = value.name === NOTE_EVENT;
  const span = block.allDay ? null : timeRangeValue(block.start, block.end);
  const time = isEvent ? span : (span ? some(span) : nothing);
  return withField(withField(value, '/time', time), '/plannedDay', some(dateValue(block.start)));
}

// What an edit of one block writes back, or why it writes nothing.
//
// The edit is where react-big-calendar says a block was dropped or resized to. A block dropped onto
// the day is given the span it was dropped on, and one dragged there out of the unscheduled strip is
// given the default length, as the plugin gives a block nothing says the length of. A block dropped
// across the top of a day has its time taken off — which an event cannot have, as an event is a block
// of the day.
//
// The mode is the plugin's edit mode: a plain move, a copy left beside the original, or a move which
// pushes or shrinks the blocks it now runs into (see 'editBlocks'). Every block an edit changes is
// written back in the one batch, so pushing a block writes the blocks it pushed too.
export function editOps(events, {event, start, end, isAllDay}, mode) {
  if (isAllDay) {
    if (event.item.ciValue.name === NOTE_EVENT) {
      return {error: 'An event is a block of the day, so it cannot be put on a day without a time'};
    }
    const day = startOfDay(start);
    const changes = [{...event, allDay: true, start: day, end: day}];
    return {changes, ops: changes.map(c => replaceOp(c))};
  }
  const span = event.allDay
    ? {start, end: new Date(start.getTime() + DEFAULT_MINUTES * 60 * 1000)}
    : {start, end};
  const timed = events.filter(e => !e.allDay);
  if (mode === 'copy') {
    const [copy] = editBlocks(timed, event, span, 'move');
    const placed = {...copy, id: `${event.id}-copy`, allDay: false};
    return {changes: [placed], ops: [{tag: 'InsertAfter', page: event.item.ciPage, metaId: event.item.ciExpr, src: valueSource(placedValue(event.item.ciValue, placed))}]};
  }
  const changes = editBlocks(timed, event, span, mode).map(c => ({...c, allDay: false}));
  return {changes, ops: changes.map(c => replaceOp(c))};
}

function replaceOp(block) {
  return {tag: 'Replace', page: block.item.ciPage, metaId: block.item.ciExpr, src: valueSource(placedValue(block.item.ciValue, block))};
}

// What creating a block writes: a new task or a new bullet over the span it was drawn on, written
// into the vault the given page is a note of. Which note it goes into is the importer's to say,
// which puts it in the daily note of its day as the plugin does.
export function createOps(page, {start, end}, contents, kind) {
  const range = timeRangeValue(start, end);
  const day = some(dateValue(start));
  const value = kind === 'bullet'
    ? {tag: 'TupleVal', name: NOTE_EVENT, args: {'/contents': {tag: 'StrVal', contents}, '/time': range, '/plannedDay': day}}
    : {tag: 'TupleVal', name: '/Data/Note/Task/Simple', args: {
      '/contents': {tag: 'StrVal', contents},
      '/state': {tag: 'TupleVal', name: '/Data/Primitive/False', args: {}},
      '/time': some(range),
      '/plannedDay': day
    }};
  return [{tag: 'Create', page, src: valueSource(value)}];
}
