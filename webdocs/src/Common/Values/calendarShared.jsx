import React, {useContext, useState} from 'react';

import {dateFnsLocalizer} from 'react-big-calendar';
import {format, getDay, parse, startOfWeek} from 'date-fns';
import {enUS} from 'date-fns/locale';
import {Link} from 'react-router-dom';

import {ConfigContext, ReloadContext, writeExpr} from '../Common';
import {Value} from '../Value';

// What the calendar and the day planner share: how a day and a time of day are read out of an
// answer, how the calendar library is told about weeks, and how an entry is drawn and written back.

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
export function asLocalDay(day) {
  const utc = dayToDate(day);
  return new Date(utc.getUTCFullYear(), utc.getUTCMonth(), utc.getUTCDate());
}

// A time of day on a day, as the local instant it is. A time of day is the time on the reader's own
// clock, as a day is the reader's own date, so it is laid on the day in the reader's zone.
export function atTime(day, time) {
  const [hours, minutes] = time.split(':').map(Number);
  const at = asLocalDay(day);
  at.setHours(hours, minutes);
  return at;
}

// The span an entry with a time of day takes up. An end at or before its start runs on past
// midnight, as /Data/Time/TimeRange says one does.
export function timedSpan(item) {
  const start = atTime(item.ciDay, item.ciTime.ctStart);
  const end = atTime(item.ciDay, item.ciTime.ctEnd);
  if (end <= start) {
    end.setDate(end.getDate() + 1);
  }
  return {start, end};
}

// Monday, which is the week Syntax.Obsidian.Query counts, so "this week" in a tasks query and a
// week of a calendar are the same seven days.
export const localizer = dateFnsLocalizer({
  format,
  parse,
  startOfWeek: () => startOfWeek(new Date(), {weekStartsOn: 1}),
  getDay,
  locales: {'en-US': enUS}
});

// The name a page is known by, which is the file it is written in without the path to it. The whole
// path is what the link goes to; a list gathered from a few pages is read by which page each came
// from rather than by where those pages sit.
export function pageName(page) {
  return page.split('/').pop();
}

// How an entry is drawn and how a change to it is written back. An entry is drawn as whatever draws
// a value of its kind, which is what lets a calendar show a kind of thing it has never heard of. A
// change to one is written back to the page it came from and not to the page showing it, as a row
// of a task list is.
export function useEntries() {
  const reload = useContext(ReloadContext);
  const config = useContext(ConfigContext);
  const canEdit = config.isLoaded && !config.data?.readonly;
  const [error, setError] = useState(null);

  const save = async (item, src) => {
    setError(null);
    const res = await writeExpr(item.ciPage, item.ciExpr, src);
    if (res.ok) {
      reload();
    } else {
      setError(res.message);
    }
  };

  const valueOf = item => <Value data={item.ciValue} update={canEdit ? (src => save(item, src)) : undefined} />;

  return {error, valueOf};
}

// The entries which sit on no day, shown apart from the days rather than dropped: the gather found
// these, and a calendar quietly showing less than it was handed would be answering a narrower
// question than the one asked.
export function Undated({items, valueOf, classes}) {
  if (items.length === 0) {
    return null;
  }
  return (
    <div className={classes.undated} data-testid="calendar-undated">
      <div className={classes.undatedHeading}>On no day</div>
      {items.map(item => (
        <div key={item.ciExpr} className={classes.entry}>
          <span className={classes.entryValue}>{valueOf(item)}</span>
          <Link className={classes.page} to={`/docs/${encodeURIComponent(item.ciPage)}`}>{pageName(item.ciPage)}</Link>
        </div>
      ))}
    </div>
  );
}

// The styles both share, for the entries and for what sits on no day.
export const entryStyles = {
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
};
