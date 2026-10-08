import React, {useMemo} from 'react';

import makeStyles from '@mui/styles/makeStyles';
import Checkbox from '@mui/material/Checkbox';
import Chip from '@mui/material/Chip';

import {dateText, someValue} from './time';

// The status a done box wears, which is the one the checkbox reads. Which character means which
// status is the vault's to say, so what is read is the status and never the character in the box.
const DONE = "/Data/Note/StatusType/Done";

// How a priority is shown, where it is shown at all. A task nobody gave a priority to has none to
// show, so /Data/Note/Priority/None is absent from this and shows nothing.
const PRIORITIES = {
  "/Data/Note/Priority/Highest": "highest",
  "/Data/Note/Priority/High": "high",
  "/Data/Note/Priority/Medium": "medium",
  "/Data/Note/Priority/Low": "low",
  "/Data/Note/Priority/Lowest": "lowest"
};

const useStyles = makeStyles({
  task: {
    display: 'flex',
    alignItems: 'center',
    flexWrap: 'wrap',
    gap: 4
  },
  description: {
    flexGrow: 1
  },
  done: {
    color: 'gray',
    textDecoration: 'line-through'
  },
  // One line in a small space: the writing is cut short where it does not fit, rather than wrapped
  // below the edge of the block holding it.
  dense: {
    flexWrap: 'nowrap',
    minWidth: 0,
    // Absolute, as a dense Task's is, so that the two readings of a checkbox are one size wherever
    // they are drawn rather than each relative to whatever holds it.
    fontSize: '0.85rem'
  },
  denseBox: {
    padding: '0 4px 0 0'
  },
  denseDescription: {
    minWidth: 0,
    overflow: 'hidden',
    textOverflow: 'ellipsis',
    whiteSpace: 'nowrap'
  }
});

// A /Data/Maybe of a string as the string it holds, or null where it holds none. A task's fields are
// written this way because a task has a recurrence or has not got one, and an empty string would be
// a third thing that is neither.
function some(value) {
  const held = someValue(value);
  return held ? held.contents : null;
}

// A /Data/Maybe of a /Data/Time/Date as the day it holds, written as a note writes it.
function someDate(value) {
  return dateText(someValue(value));
}

// One task of a note in a vault running the Obsidian Tasks plugin, as the checkbox it was written as
// and the fields the plugin wrote beside it.
//
// Some of those fields are fields of the value and the rest arrive together as JSON in `rest`, for
// the reason /Data/Note/Task/Obsidian's own comment gives: a data type wide enough to name them all
// cannot be typechecked today. Both are read here and shown the same way, so which of them is which
// is not something a reader of the page can see.
//
// Shown and not edited. Reading a line apart is what the importer now does; writing one back from
// the parts it was read into is not, so there is nothing here to write with — and a value given no
// update is simply a value that cannot be edited. The box is disabled for that reason rather than
// because a task cannot be checked.
function ObsidianTask(props) {
  const description = props["/description"].contents;
  const done = props["/statusType"].name === DONE;
  const priority = PRIORITIES[props["/priority"].name];
  const restArg = props["/rest"];
  const rest = useMemo(() => JSON.parse(restArg.contents), [restArg]);
  const classes = useStyles();

  // Each field is shown only where the task carries one, so a plain checkbox is shown as a plain
  // checkbox rather than as a row of empty labels.
  const fields = [
    ["due", someDate(props["/due"])],
    ["scheduled", someDate(props["/scheduled"])],
    ["repeats", some(props["/recurrence"])],
    ["starts", someDate(props["/start"])],
    // The day the Day Planner plans the task for, which a daily note gives every task in it. Its
    // time of day is not shown apart, as the line already says it.
    ["planned", someDate(props["/plannedDay"])],
    ["created", rest.created],
    // The date a task was finished or dropped, which is one field because a task is one or the
    // other and never both. Which of the two it is, is the status.
    [done ? "done" : "cancelled", rest.statusDate],
    ["on completion", rest.onCompletion],
    ["id", rest.id],
    ["waits on", (rest.dependsOn || []).join(', ')],
    ["priority", priority]
  ];

  const shown = fields.filter(([, value]) => value);

  // Dense, the task is its box and what it says on one line, and the fields beside it are left to
  // the tooltip: a block of a calendar's day is too short to hold them, and the time and day they
  // would most often say are what the calendar is already drawing.
  if (props.dense) {
    const tooltip = [description, rest.statusName, ...shown.map(([label, value]) => `${label}: ${value}`)].filter(Boolean).join('\n');
    return (
      <span className={`${classes.task} ${classes.dense}`} title={tooltip}>
        <Checkbox
          checked={done}
          disabled
          size="small"
          className={classes.denseBox}
          inputProps={{'aria-label': description}}
        />
        <span className={`${classes.description} ${classes.denseDescription} ${done ? classes.done : ''}`}>{description}</span>
      </span>
    );
  }

  return (
    <span className={classes.task}>
      <Checkbox
        checked={done}
        disabled
        size="small"
        inputProps={{'aria-label': description}}
      />
      <span className={`${classes.description} ${done ? classes.done : ''}`}>{description}</span>
      {/* The status is shown by the name the vault gave it, which is the only thing that says what
          a box holding something other than a space or an x means. */}
      {rest.statusName ? <Chip label={rest.statusName} size="small" variant="outlined" /> : null}
      {shown.map(([label, value]) => (
        <Chip key={label} label={`${label}: ${value}`} size="small" variant="outlined" />
      ))}
      {(rest.tags || []).map(tag => (
        <Chip key={tag} label={tag} size="small" variant="outlined" />
      ))}
    </span>
  );
}

export default ObsidianTask;
