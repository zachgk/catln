import React, {useContext, useMemo, useState} from 'react';

import makeStyles from '@mui/styles/makeStyles';
import {Calendar as BigCalendar, dateFnsLocalizer} from 'react-big-calendar';
import {format, getDay, parse, startOfWeek} from 'date-fns';
import {enUS} from 'date-fns/locale';
import {Link} from 'react-router-dom';

import 'react-big-calendar/lib/css/react-big-calendar.css';

import {ConfigContext, ReloadContext, writeExpr} from '../Common';
import {Value} from '../Value';

// A day is written as a YYYY-MM-DD string everywhere a note is read, and the arithmetic on one goes
// through UTC. new Date('2026-01-03') is midnight UTC, so reading it back with getDate() anywhere
// behind UTC gives the second -- which would put an entry on the wrong day for half the world and
// would make what a test sees depend on where it runs.
function dayToDate(day) {
  const [year, month, date] = day.split('-').map(Number);
  return new Date(Date.UTC(year, month - 1, date));
}

// The calendar draws in the reader's own zone, so a day that came in as UTC midnight is handed over
// as local midnight of the same date rather than as the instant it names.
function asLocalDay(day) {
  const utc = dayToDate(day);
  return new Date(utc.getUTCFullYear(), utc.getUTCMonth(), utc.getUTCDate());
}

const localizer = dateFnsLocalizer({
  format,
  parse,
  // Monday, which is the week Syntax.Obsidian.Query counts, so "this week" in a tasks query and a
  // week of this calendar are the same seven days.
  startOfWeek: () => startOfWeek(new Date(), {weekStartsOn: 1}),
  getDay,
  locales: {'en-US': enUS}
});

const useStyles = makeStyles({
  calendar: {
    height: '36em'
  },
  error: {
    color: 'red'
  },
  entry: {
    display: 'flex',
    alignItems: 'center',
    fontSize: '0.85em'
  },
  entryValue: {
    flexGrow: 1,
    overflow: 'hidden'
  },
  undated: {
    marginTop: '1em'
  },
  undatedHeading: {
    fontWeight: 'bold',
    marginBottom: '0.25em'
  },
  page: {
    marginLeft: '1em',
    fontSize: '0.85em'
  }
});

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
// An entry is drawn as whatever draws a value of its kind, which is what lets a calendar show a kind
// of thing it has never heard of. A change to one is written back to the page it came from and not
// to the page showing the calendar, as a row of a task list is.
function Calendar(props) {
  const resultArg = props["/result"];
  const result = useMemo(() => JSON.parse(resultArg.contents), [resultArg]);
  const reload = useContext(ReloadContext);
  const config = useContext(ConfigContext);
  const canEdit = config.isLoaded && !config.data?.readonly;
  const [error, setError] = useState(null);
  const classes = useStyles();

  const events = useMemo(() => result.crItems
    .filter(item => item.ciDay)
    .map(item => {
      const at = asLocalDay(item.ciDay);
      return {title: item.ciExpr, start: at, end: at, allDay: true, item};
    }), [result]);

  const undated = result.crItems.filter(item => !item.ciDay);

  const save = async (item, src) => {
    setError(null);
    const res = await writeExpr(item.ciPage, item.ciExpr, src);
    if (res.ok) {
      reload();
    } else {
      setError(res.message);
    }
  };

  const entry = item => (
    <span className={classes.entry}>
      <span className={classes.entryValue}>
        <Value data={item.ciValue} update={canEdit ? (src => save(item, src)) : undefined} />
      </span>
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
      {undated.length > 0 && (
        <div className={classes.undated} data-testid="calendar-undated">
          {/* Shown rather than dropped: the gather found these, and a calendar quietly showing less
              than it was handed would be answering a narrower question than the one asked. */}
          <div className={classes.undatedHeading}>On no day</div>
          {undated.map(item => (
            <div key={item.ciExpr} className={classes.entry}>
              <span className={classes.entryValue}>
                <Value data={item.ciValue} update={canEdit ? (src => save(item, src)) : undefined} />
              </span>
              <Link className={classes.page} to={`/docs/${encodeURIComponent(item.ciPage)}`}>{pageName(item.ciPage)}</Link>
            </div>
          ))}
        </div>
      )}
    </div>
  );
}

// The name a page is known by, which is the file it is written in without the path to it. The whole
// path is what the link goes to; a list gathered from a few pages is read by which page each came
// from rather than by where those pages sit.
function pageName(page) {
  return page.split('/').pop();
}

export default Calendar;
