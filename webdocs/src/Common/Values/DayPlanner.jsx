import React, {useMemo, useState} from 'react';

import makeStyles from '@mui/styles/makeStyles';
import {Calendar as BigCalendar} from 'react-big-calendar';

import 'react-big-calendar/lib/css/react-big-calendar.css';

import {entryEvent} from './Calendar';
import {Undated, asLocalDay, entryStyles, localizer, useEntries} from './calendarShared';

// Where a day is opened when nothing on it has a time: six in the morning, which is where the
// Obsidian Day Planner plugin opens a day it has been told nothing about.
const DEFAULT_START_HOUR = 6;

const useStyles = makeStyles({
  ...entryStyles,
  planner: {
    height: '48em',
    // The answer carries the day it was worked out on and not the time, so there is no now to draw a
    // line at. A line drawn from the reader's own clock would be drawn on a day the answer may not be
    // about -- the reason the calendar marks the answer's day rather than the clock's.
    '& .rbc-current-time-indicator': {
      display: 'none'
    }
  }
});

// The time of day a day is opened at: the earliest block on it, or the morning where it has none.
function openingTime(events, date) {
  const onDay = events.filter(e => !e.allDay && e.start.toDateString() === date.toDateString());
  const first = onDay.reduce((earliest, e) => (earliest && earliest <= e.start ? earliest : e.start), null);
  const at = new Date(date);
  if (first) {
    at.setHours(first.getHours(), first.getMinutes());
  } else {
    at.setHours(DEFAULT_START_HOUR, 0);
  }
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

  const events = useMemo(() => result.crItems.filter(item => item.ciDay).map(entryEvent), [result]);
  const scrollTo = useMemo(() => openingTime(events, date), [events, date]);

  const undated = result.crItems.filter(item => !item.ciDay);

  const entry = item => (
    <span className={classes.entry}>
      <span className={classes.entryValue}>{valueOf(item)}</span>
    </span>
  );

  if (result.crError) {
    return <div className={classes.error} data-testid="planner-error">{result.crError}</div>;
  }

  return (
    <div data-testid="planner">
      {error && <div className={classes.error} data-testid="planner-error">save failed: {error}</div>}
      <BigCalendar
        className={classes.planner}
        localizer={localizer}
        events={events}
        date={date}
        onNavigate={setDate}
        getNow={() => asLocalDay(result.crToday)}
        defaultView="day"
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
