import React, {useContext, useEffect, useState} from 'react';

import {dateFnsLocalizer} from 'react-big-calendar';
import {format, getDay, parse, startOfWeek} from 'date-fns';
import {enUS} from 'date-fns/locale';
import {Link} from 'react-router-dom';
import TextField from '@mui/material/TextField';

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
  const utc = new Date(Date.UTC(2000, month - 1, date));
  utc.setUTCFullYear(year);
  return utc;
}

// The calendar draws in the reader's own zone, so a day that came in as UTC midnight is handed over
// as local midnight of the same date rather than as the instant it names.
//
// The year is set apart rather than passed to the Date constructor, which reads a year below 100 as
// one of the 1900s.
export function asLocalDay(day) {
  const utc = dayToDate(day);
  const local = new Date(2000, utc.getUTCMonth(), utc.getUTCDate());
  local.setFullYear(utc.getUTCFullYear());
  return local;
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

  // Dense where it is drawn into a block of the calendar itself, which is a small space, and at full
  // size where it is listed apart from the days.
  const valueOf = (item, dense = false) => <Value data={item.ciValue} dense={dense} update={canEdit ? (src => save(item, src)) : undefined} />;

  return {error, setError, valueOf, canEdit, reload};
}

// A day as the YYYY-MM-DD a date field holds, in the reader's own zone, as a day is read everywhere
// else here.
export function dayText(date) {
  const pad = n => String(n).padStart(2, '0');
  return `${date.getFullYear()}-${pad(date.getMonth() + 1)}-${pad(date.getDate())}`;
}

// A field naming the day being shown, which turns the calendar to whatever day is typed or picked
// into it. Turning over a day or a week at a time is the toolbar's; this is for a day far from the
// one the calendar opened on, which would otherwise be a long way of turning over.
//
// The field keeps what is being typed into it apart from the day showing. A browser reports a date
// field changed at every keystroke, and partway through typing a year it reports the year typed so
// far -- 0002, 0020, 0202 -- so turning to each of those would carry the calendar back two thousand
// years and write that year back into the field under the reader's fingers. It turns only to a day
// whose year has all four of its digits, and follows the day showing whenever that is turned to some
// other way.
export function GoToDay({date, onChange}) {
  const shown = dayText(date);
  const [draft, setDraft] = useState(shown);
  useEffect(() => setDraft(shown), [shown]);
  return (
    <TextField
      type="date"
      size="small"
      label="Go to day"
      value={draft}
      onChange={event => {
        const typed = event.target.value;
        setDraft(typed);
        if (/^[1-9]\d{3}-\d{2}-\d{2}$/.test(typed) && typed !== shown) {
          onChange(asLocalDay(typed));
        }
      }}
      InputLabelProps={{shrink: true}}
      inputProps={{'data-testid': 'go-to-day'}}
      sx={{mb: 1}}
    />
  );
}

// How the blocks of a day are laid out where an entry is drawn over the time it takes up. The time a
// block spans is written beside what it holds rather than above it, and the hours are drawn tall
// enough that a block of half an hour -- the length a block is given by default -- holds a line.
// Where the days are drawn a week at a time the time is left off, as there is no room for it beside
// the writing and the grid already says it.
export function timeGridStyles(weekly) {
  return {
    '& .rbc-timeslot-group': {
      minHeight: '4em'
    },
    '& .rbc-time-view .rbc-event': {
      flexFlow: 'row nowrap',
      alignItems: 'flex-start',
      gap: 6,
      padding: '1px 4px'
    },
    // Where a block can be dragged, its label and writing are inside the wrapper that holds its
    // resize handles rather than in the block itself, so that wrapper is laid out the same way. The
    // handles are placed absolutely at its edges, so they are not part of the row.
    '& .rbc-time-view .rbc-addons-dnd-resizable': {
      display: 'flex',
      flexFlow: 'row nowrap',
      alignItems: 'flex-start',
      gap: 6
    },
    '& .rbc-time-view .rbc-event-label': {
      flex: 'none',
      paddingTop: 3,
      fontSize: '0.75em',
      display: weekly ? 'none' : undefined
    },
    '& .rbc-event-content': {
      minWidth: 0,
      overflow: 'hidden'
    }
  };
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
          <span className={classes.entryValue}>{valueOf(item, false)}</span>
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
