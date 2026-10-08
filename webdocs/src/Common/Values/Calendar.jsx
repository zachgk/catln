import React, {useMemo} from 'react';

import makeStyles from '@mui/styles/makeStyles';
import {Calendar as BigCalendar} from 'react-big-calendar';

import 'react-big-calendar/lib/css/react-big-calendar.css';

import {Undated, asLocalDay, entryStyles, localizer, timedSpan, useEntries} from './calendarShared';

const useStyles = makeStyles({
  ...entryStyles,
  calendar: {
    height: '36em'
  }
});

// The event an entry is drawn as: across the whole of its day, or over the time of day it was given
// where it was given one.
export function entryEvent(item) {
  if (item.ciTime) {
    return {title: item.ciExpr, ...timedSpan(item), allDay: false, item};
  }
  const at = asLocalDay(item.ciDay);
  return {title: item.ciExpr, start: at, end: at, allDay: true, item};
}

// What a calendar found, laid out on the days it found things on, a day, a week, a month or a list
// at a time.
//
// Which of those four is showing is decided here and asked of nobody. The answer holds every entry
// there is, so turning over to another way of reading it is a reader turning over what they have
// already been handed rather than a second question for the compiler.
//
// Today is the day the answer carries and never the browser's own clock. Every day in the answer was
// worked out against the day the evaluation was given, so a calendar marking any other day as today
// would mark a day the answer it is drawing was not about -- and a reader in another zone, or a test
// run on another date, would see a different calendar of the same answer.
//
// An entry given a time of day is drawn over that time in the day and week views, and one without
// is drawn across its whole day.
function Calendar(props) {
  const resultArg = props["/result"];
  const result = useMemo(() => JSON.parse(resultArg.contents), [resultArg]);
  const {error, valueOf} = useEntries();
  const classes = useStyles();

  const events = useMemo(() => result.crItems.filter(item => item.ciDay).map(entryEvent), [result]);

  const undated = result.crItems.filter(item => !item.ciDay);

  const entry = item => (
    <span className={classes.entry}>
      <span className={classes.entryValue}>{valueOf(item)}</span>
    </span>
  );

  if (result.crError) {
    return <div className={classes.error} data-testid="calendar-error">{result.crError}</div>;
  }

  return (
    <div data-testid="calendar">
      {error && <div className={classes.error} data-testid="calendar-error">save failed: {error}</div>}
      <BigCalendar
        className={classes.calendar}
        localizer={localizer}
        events={events}
        defaultDate={asLocalDay(result.crToday)}
        getNow={() => asLocalDay(result.crToday)}
        defaultView="month"
        views={['day', 'week', 'month', 'agenda']}
        startAccessor="start"
        endAccessor="end"
        components={{event: ({event}) => entry(event.item)}}
      />
      <Undated items={undated} valueOf={valueOf} classes={classes} />
    </div>
  );
}

export default Calendar;
