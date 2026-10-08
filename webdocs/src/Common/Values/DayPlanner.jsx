import React, {useMemo, useState} from 'react';

import makeStyles from '@mui/styles/makeStyles';
import {Calendar as BigCalendar} from 'react-big-calendar';
import {addDays, startOfDay, startOfWeek} from 'date-fns';

import 'react-big-calendar/lib/css/react-big-calendar.css';

import {entryEvent} from './Calendar';
import {GoToDay, Undated, asLocalDay, entryStyles, localizer, timeGridStyles, useEntries} from './calendarShared';

// Where a day is opened when nothing on it has a time: six in the morning, which is where the
// Obsidian Day Planner plugin opens a day it has been told nothing about.
const DEFAULT_START_HOUR = 6;

const planner = {
  height: '48em',
  // The answer carries the day it was worked out on and not the time, so there is no now to draw a
  // line at. A line drawn from the reader's own clock would be drawn on a day the answer may not be
  // about -- the reason the calendar marks the answer's day rather than the clock's.
  '& .rbc-current-time-indicator': {
    display: 'none'
  }
};

const useStyles = makeStyles({
  ...entryStyles,
  plannerDay: {...planner, ...timeGridStyles(false)},
  plannerWeek: {...planner, ...timeGridStyles(true)}
});

// The time of day a day is opened at: the earliest block on it, or the morning where it has none.
//
// Where a week is drawn, the earliest block of any of its days: a week opened at the first block of
// one day would scroll another day's earlier block out of sight.
export function openingTime(events, date, view) {
  const from = view === 'week' ? startOfWeek(date, {weekStartsOn: 1}) : startOfDay(date);
  const to = addDays(from, view === 'week' ? 7 : 1);
  const minutes = events
    .filter(e => !e.allDay && e.start >= from && e.start < to)
    .map(e => e.start.getHours() * 60 + e.start.getMinutes());
  const first = minutes.length > 0 ? Math.min(...minutes) : DEFAULT_START_HOUR * 60;
  const at = new Date(date);
  at.setHours(Math.floor(first / 60), first % 60, 0, 0);
  return at;
}

// What a calendar found, laid out as the blocks of a day: a timeline of the day the answer was
// worked out on, with each entry given a time of day drawn over that time and each one sitting on
// the day without one drawn across the top of it, unscheduled.
//
// It is the calendar's answer read one day at a time, or a week at a time, rather than a question of
// its own. Turning to another day turns over what was already handed over, and asks nothing again.
function DayPlanner(props) {
  const resultArg = props["/result"];
  const result = useMemo(() => JSON.parse(resultArg.contents), [resultArg]);
  const {error, valueOf} = useEntries();
  const classes = useStyles();
  const [date, setDate] = useState(() => asLocalDay(result.crToday));
  const [view, setView] = useState('day');

  const events = useMemo(() => result.crItems.filter(item => item.ciDay).map(entryEvent), [result]);
  const scrollTo = useMemo(() => openingTime(events, date, view), [events, date, view]);

  const undated = result.crItems.filter(item => !item.ciDay);

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
      <GoToDay date={date} onChange={setDate} />
      <BigCalendar
        className={view === 'week' ? classes.plannerWeek : classes.plannerDay}
        localizer={localizer}
        events={events}
        date={date}
        onNavigate={setDate}
        view={view}
        onView={setView}
        getNow={() => asLocalDay(result.crToday)}
        views={['day', 'week']}
        scrollToTime={scrollTo}
        startAccessor="start"
        endAccessor="end"
        components={{event: ({event}) => entry(event.item)}}
      />
      <Undated items={undated} valueOf={valueOf} classes={classes} />
    </div>
  );
}

export default DayPlanner;
