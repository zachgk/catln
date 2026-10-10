import React, {useEffect, useMemo, useRef, useState} from 'react';

import makeStyles from '@mui/styles/makeStyles';
import {Calendar as BigCalendar} from 'react-big-calendar';
import withDragAndDrop from 'react-big-calendar/lib/addons/dragAndDrop';
import {addDays, startOfDay, startOfWeek} from 'date-fns';
import Button from '@mui/material/Button';
import Dialog from '@mui/material/Dialog';
import DialogActions from '@mui/material/DialogActions';
import DialogContent from '@mui/material/DialogContent';
import DialogTitle from '@mui/material/DialogTitle';
import FormControlLabel from '@mui/material/FormControlLabel';
import Radio from '@mui/material/Radio';
import RadioGroup from '@mui/material/RadioGroup';
import TextField from '@mui/material/TextField';
import ToggleButton from '@mui/material/ToggleButton';
import ToggleButtonGroup from '@mui/material/ToggleButtonGroup';

import 'react-big-calendar/lib/css/react-big-calendar.css';
import 'react-big-calendar/lib/addons/dragAndDrop/styles.css';

import {writeExprs} from '../Common';
import {entryEvent} from './Calendar';
import {GoToDay, Undated, asLocalDay, entryStyles, localizer, timeGridStyles, useEntries} from './calendarShared';
import {DEFAULT_MINUTES, createOps, editOps} from './plannerEdits';

const PlannerCalendar = withDragAndDrop(BigCalendar);

// Where a day is opened when nothing on it has a time: six in the morning, which is where the
// Obsidian Day Planner plugin opens a day it has been told nothing about.
const DEFAULT_START_HOUR = 6;

// The grid a block is moved and resized along, which is the plugin's snapStepMinutes: ten minutes,
// six to the hour.
const STEP_MINUTES = 10;

const planner = {
  height: '48em',
  // The answer carries the day it was worked out on and not the time, so there is no now to draw a
  // line at. A line drawn from the reader's own clock would be drawn on a day the answer may not be
  // about -- the reason the calendar marks the answer's day rather than the clock's.
  '& .rbc-current-time-indicator': {
    display: 'none'
  },
  // What sits on a day with no time is drawn in the planner's own strip above the days rather than in
  // the calendar's row across their tops (see 'UnscheduledStrip'), so that row is empty and is not
  // drawn.
  '& .rbc-allday-cell': {
    display: 'none'
  }
};

const useStyles = makeStyles({
  ...entryStyles,
  plannerDay: {...planner, ...timeGridStyles(false)},
  plannerWeek: {...planner, ...timeGridStyles(true)},
  controls: {
    display: 'flex',
    alignItems: 'center',
    gap: 16,
    flexWrap: 'wrap'
  },
  hint: {
    fontSize: '0.85em',
    color: 'gray'
  },
  strip: {
    display: 'flex',
    flexWrap: 'wrap',
    alignItems: 'center',
    gap: 6,
    minHeight: '2.2em',
    padding: 4,
    marginBottom: 4,
    border: '1px dashed #bbb',
    borderRadius: 4
  },
  stripLabel: {
    fontSize: '0.8em',
    color: 'gray',
    marginRight: 4
  },
  stripEntry: {
    display: 'flex',
    alignItems: 'center',
    gap: 4,
    maxWidth: '24em',
    padding: '0 6px',
    borderRadius: 4,
    background: '#3174ad',
    color: 'white',
    cursor: 'grab'
  },
  stripDay: {
    fontSize: '0.75em',
    flex: 'none'
  },
  stripGrip: {
    flex: 'none',
    cursor: 'grab',
    padding: '0 2px',
    userSelect: 'none'
  }
});

// The days drawn: the one showing, or the seven of the week it is in.
function shownRange(date, view) {
  const from = view === 'week' ? startOfWeek(date, {weekStartsOn: 1}) : startOfDay(date);
  return {from, to: addDays(from, view === 'week' ? 7 : 1)};
}

// The time of day a day is opened at: the earliest block on it, or the morning where it has none.
//
// Where a week is drawn, the earliest block of any of its days: a week opened at the first block of
// one day would scroll another day's earlier block out of sight.
export function openingTime(events, date, view) {
  const {from, to} = shownRange(date, view);
  const minutes = events
    .filter(e => !e.allDay && e.start >= from && e.start < to)
    .map(e => e.start.getHours() * 60 + e.start.getMinutes());
  const first = minutes.length > 0 ? Math.min(...minutes) : DEFAULT_START_HOUR * 60;
  const at = new Date(date);
  at.setHours(Math.floor(first / 60), first % 60, 0, 0);
  return at;
}

// A block of the planner: the entry it draws, keyed by the expression it was declared by.
function plannedEvent(item) {
  return {...entryEvent(item), id: item.ciExpr};
}

// The page a block created here is written beside: any note the planner shows something of. A
// planner showing nothing from a note has nowhere to write a new block, which is said rather than
// tried.
function notePage(items) {
  return items.find(item => item.ciValue?.name?.startsWith('/Data/Note/'))?.ciPage ?? null;
}

// What sits on the days drawn with no time of day: entries of the calendar that are no events, to be
// given a time by dragging them onto a day, as the plugin's unscheduled row is.
//
// Drawn by the planner rather than as the calendar's row across the tops of its days, because the
// calendar cannot carry a drag out of that row: a column of its days takes up only a drag that began
// on a block within it (react-big-calendar's EventContainerWrapper refuses any other), so a block
// dragged from the row is left half-dragged. A drag from outside the calendar is one it takes up, so
// the strip is outside it and its entries are dragged the way anything outside it is.
//
// It is also where a block is dragged to have its time taken off: a block let go of over the strip
// is put back on its day with no time.
const UnscheduledStrip = React.forwardRef(function UnscheduledStrip({entries, canEdit, weekly, valueOf, onDragFrom, classes}, ref) {
  if (entries.length === 0 && !canEdit) {
    return null;
  }
  return (
    <div ref={ref} className={classes.strip} data-testid="planner-unscheduled">
      <span className={classes.stripLabel}>Unscheduled</span>
      {entries.map(e => (
        <span
          key={e.id}
          className={classes.stripEntry}
          draggable={canEdit}
          data-testid="planner-unscheduled-entry"
          onDragStart={event => {
            // Something has to be set for a browser to carry the drag at all; what is dropped is
            // the entry the planner is holding, not this.
            event.dataTransfer.setData('text/plain', e.id);
            event.dataTransfer.effectAllowed = 'move';
            onDragFrom(e);
          }}
          onDragEnd={() => onDragFrom(null)}
        >
          {/* A handle to take hold of the entry by. What an entry draws may be writing that can be
              edited, and pressing on an editable field selects its writing rather than dragging
              what holds it. */}
          {canEdit && <span className={classes.stripGrip} aria-label="drag to a time">⠿</span>}
          {weekly && <span className={classes.stripDay}>{e.start.toLocaleDateString(undefined, {weekday: 'short'})}</span>}
          {valueOf(e.item, true)}
        </span>
      ))}
      {entries.length === 0 && <span className={classes.hint}>Drag a block here to take its time off</span>}
    </div>
  );
});

// Asks what a block drawn on the day is to say, and whether it is a task or a bullet. Cancelling
// writes nothing, as closing the plugin's modal cancels its edit.
function CreateDialog({slot, onCancel, onCreate}) {
  const [contents, setContents] = useState('');
  const [kind, setKind] = useState('task');
  useEffect(() => {
    setContents('');
  }, [slot]);
  return (
    <Dialog open={!!slot} onClose={onCancel}>
      <DialogTitle>New block</DialogTitle>
      <DialogContent>
        <TextField
          autoFocus
          fullWidth
          margin="dense"
          label="What it is"
          value={contents}
          onChange={event => setContents(event.target.value)}
          inputProps={{'data-testid': 'new-block-contents'}}
        />
        <RadioGroup row value={kind} onChange={event => setKind(event.target.value)}>
          <FormControlLabel value="task" control={<Radio />} label="Task" />
          <FormControlLabel value="bullet" control={<Radio />} label="Bullet" />
        </RadioGroup>
      </DialogContent>
      <DialogActions>
        <Button onClick={onCancel}>Cancel</Button>
        <Button disabled={!contents.trim()} onClick={() => onCreate(contents.trim(), kind)}>Create</Button>
      </DialogActions>
    </Dialog>
  );
}

// What a calendar found, laid out as the blocks of a day: a timeline of the day the answer was
// worked out on, with each event drawn over the time it takes up and each entry sitting on the day
// without one drawn across the top of it, unscheduled.
//
// It is the calendar's answer read one day at a time, or a week at a time, rather than a question of
// its own. Turning to another day turns over what was already handed over, and asks nothing again.
//
// Where the pages can be written, a block is edited as the Obsidian Day Planner plugin edits one: it
// is dragged to another time or day, resized from either edge along a ten-minute grid, dragged onto
// the top of a day to take its time off or from there onto the day to give it one, and drawn on an
// empty part of the day to create one. The mode chosen beside the toolbar is what a drag does to the
// blocks it runs into. An edit is drawn at once and written back as it is let go of; one which cannot
// be written is drawn back where it was and says why.
function DayPlanner(props) {
  const resultArg = props["/result"];
  const result = useMemo(() => JSON.parse(resultArg.contents), [resultArg]);
  const {error, setError, valueOf, canEdit, reload} = useEntries();
  const classes = useStyles();
  const [date, setDate] = useState(() => asLocalDay(result.crToday));
  const [view, setView] = useState('day');
  const [mode, setMode] = useState('move');
  const [pending, setPending] = useState(null);
  const [creating, setCreating] = useState(null);

  // An edit is drawn until the answer it was written back for arrives, and the answer is drawn then.
  useEffect(() => setPending(null), [result]);

  const events = useMemo(() => result.crItems.filter(item => item.ciDay).map(plannedEvent), [result]);
  const shown = useMemo(() => {
    if (!pending) {
      return events;
    }
    const changed = new Map(pending.map(c => [c.id, c]));
    return [...events.map(e => changed.get(e.id) ?? e), ...pending.filter(c => !events.some(e => e.id === c.id))];
  }, [events, pending]);
  const scrollTo = useMemo(() => openingTime(events, date, view), [events, date, view]);
  const createPage = notePage(result.crItems);

  // The blocks drawn over the days, and what sits on the days drawn with no time, which the strip
  // draws instead of the calendar.
  const timed = shown.filter(e => !e.allDay);
  const {from, to} = shownRange(date, view);
  const unscheduled = shown.filter(e => e.allDay && e.start >= from && e.start < to);

  // The entry of the strip being dragged, which the calendar is told of as a drag from outside it.
  const [dragged, setDragged] = useState(null);
  // The block of the calendar being moved, which is put back on its day with no time where it is let
  // go of over the strip. The calendar's own drag ends in nothing there, being outside it.
  const stripRef = useRef(null);
  const calendarRef = useRef(null);
  const moving = useRef(null);
  // The calendar drawn afresh, which is how a drag it began and lost is put away: a column of its
  // days keeps drawing the block it was dragging when it is let go of outside the column, as it
  // ignores a release outside itself without clearing what it was drawing.
  const [calendarKey, setCalendarKey] = useState(0);
  useEffect(() => {
    const letGo = e => {
      const block = moving.current;
      moving.current = null;
      // Where it is let go of is asked of the element let go of on rather than of where the pointer
      // is: a click on a block's own box is a drag begun and ended on the block, and must not read
      // as one let go of anywhere else.
      if (!block || calendarRef.current?.contains(e.target)) {
        return;
      }
      setCalendarKey(k => k + 1);
      if (stripRef.current?.contains(e.target)) {
        edit({event: block, start: block.start, end: block.start, isAllDay: true});
      }
    };
    window.addEventListener('mouseup', letGo, true);
    return () => window.removeEventListener('mouseup', letGo, true);
  });

  const undated = result.crItems.filter(item => !item.ciDay);

  const write = async (changes, ops) => {
    setError(null);
    setPending(changes);
    const res = await writeExprs(ops);
    if (res.ok) {
      reload();
    } else {
      setPending(null);
      setError(res.message);
    }
  };

  const edit = drop => {
    const planned = editOps(events, drop, mode);
    if (planned.error) {
      setError(planned.error);
    } else {
      write(planned.changes, planned.ops);
    }
  };

  const entry = item => (
    <span className={classes.entry}>
      <span className={classes.entryValue}>{valueOf(item, true)}</span>
    </span>
  );

  if (result.crError) {
    return <div className={classes.error} data-testid="planner-error">{result.crError}</div>;
  }

  return (
    <div data-testid="planner">
      {error && <div className={classes.error} data-testid="planner-error">save failed: {error}</div>}
      <div className={classes.controls}>
        <GoToDay date={date} onChange={setDate} />
        {canEdit && (
          <ToggleButtonGroup
            exclusive
            size="small"
            value={mode}
            onChange={(_, chosen) => chosen && setMode(chosen)}
            aria-label="what a drag does"
            sx={{mb: 1}}
          >
            <ToggleButton value="move">Move</ToggleButton>
            <ToggleButton value="copy">Copy</ToggleButton>
            <ToggleButton value="push">Push neighbors</ToggleButton>
            <ToggleButton value="shrink">Shrink neighbors</ToggleButton>
          </ToggleButtonGroup>
        )}
        {canEdit && !createPage && <span className={classes.hint} data-testid="planner-no-create">Nothing here is from a note, so there is no note to create a block in</span>}
      </div>
      <UnscheduledStrip
        ref={stripRef}
        entries={unscheduled}
        canEdit={canEdit}
        weekly={view === 'week'}
        valueOf={valueOf}
        onDragFrom={setDragged}
        classes={classes}
      />
      <div ref={calendarRef}>
      <PlannerCalendar
        key={calendarKey}
        className={view === 'week' ? classes.plannerWeek : classes.plannerDay}
        localizer={localizer}
        events={timed}
        date={date}
        onNavigate={setDate}
        view={view}
        onView={setView}
        getNow={() => asLocalDay(result.crToday)}
        views={['day', 'week']}
        scrollToTime={scrollTo}
        step={STEP_MINUTES}
        timeslots={60 / STEP_MINUTES}
        startAccessor="start"
        endAccessor="end"
        draggableAccessor={() => canEdit}
        resizableAccessor={e => canEdit && !e.allDay}
        resizable={canEdit}
        onEventDrop={drop => {
          moving.current = null;
          edit(drop);
        }}
        onEventResize={drop => edit({...drop, isAllDay: false})}
        onDragStart={({event, action}) => {
          moving.current = action === 'move' ? event : null;
        }}
        // An entry of the strip dropped on a day, which is given the default length from where it was
        // dropped; it is drawn that long while it is dragged, as the plugin draws one.
        dragFromOutsideItem={() => dragged && {...dragged, allDay: false, start: new Date(0), end: new Date(DEFAULT_MINUTES * 60 * 1000)}}
        onDropFromOutside={({start}) => {
          const entry = dragged;
          setDragged(null);
          if (entry) {
            edit({event: entry, start, end: start, isAllDay: false});
          }
        }}
        selectable={canEdit && !!createPage}
        onSelectSlot={slot => {
          if (slot.action === 'select' && !slot.slots?.every(s => s.getHours() === 0 && s.getMinutes() === 0)) {
            setCreating(slot);
          }
        }}
        components={{event: ({event}) => entry(event.item)}}
      />
      </div>
      <Undated items={undated} valueOf={valueOf} classes={classes} />
      <CreateDialog
        slot={creating}
        onCancel={() => setCreating(null)}
        onCreate={(contents, kind) => {
          const slot = creating;
          setCreating(null);
          write([], createOps(createPage, slot, contents, kind));
        }}
      />
    </div>
  );
}

export default DayPlanner;
export {createOps, editOps, placedValue} from './plannerEdits';
