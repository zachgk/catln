import React, {useState, useEffect} from 'react';

import makeStyles from '@mui/styles/makeStyles';
import Checkbox from '@mui/material/Checkbox';
import InputBase from '@mui/material/InputBase';

// The Catln values a task's state is one of. A task carries the value itself rather than a display
// standing in for it, so checking the box means writing the declaration back with the other one.
const TRUE = "/Data/Primitive/True";
const FALSE = "/Data/Primitive/False";

const useStyles = makeStyles({
  task: {
    display: 'flex',
    alignItems: 'center'
  },
  contents: {
    flexGrow: 1
  },
  done: {
    color: 'gray',
    textDecoration: 'line-through'
  },
  // One line in a small space: the box loses its padding and the writing is smaller, so the two fit
  // in a block of a calendar's day as short as its default one.
  denseBox: {
    padding: '0 4px 0 0'
  },
  denseContents: {
    fontSize: '0.85rem'
  }
});

// The Catln source a task with these contents and this state is declared by. It is what update
// replaces the declaration with, so it has to name the type absolutely: it is parsed on its own
// rather than in the note it came from.
function taskSource(contents, done) {
  return `/Data/Note/Task/Simple(contents= ${JSON.stringify(contents)}, state= ${done ? TRUE : FALSE})`;
}

// One task of a note, as the checkbox it was written as. Editing it rewrites the declaration it was
// compiled from, which the page writes back into the note the task was read from.
//
// The box saves as soon as it is checked, since a click is the whole of that edit. The contents are
// saved when they are finished with rather than as they are typed, so a task is written back once
// per edit rather than once per keystroke. Without an update neither can be edited — a task shown
// where nothing can be written is still a task to read.
function Task(props) {
  const {update, dense} = props;
  const contents = props["/contents"].contents;
  const done = props["/state"].name === TRUE;
  const classes = useStyles();

  // What the box says while it is being edited. The saved contents are the ones the compiler
  // answered with, so they replace the draft whenever the page is read again.
  const [draft, setDraft] = useState(contents);
  useEffect(() => setDraft(contents), [contents]);

  // Both edits check for update rather than leaving it to the controls being disabled. Disabling a
  // control is how a task says it cannot be edited, not how it enforces it.
  const toggle = () => {
    if (update) {
      update(taskSource(contents, !done));
    }
  };

  const commit = () => {
    if (update && draft !== contents) {
      update(taskSource(draft, done));
    }
  };

  return (
    <span className={classes.task}>
      <Checkbox
        checked={done}
        disabled={!update}
        size="small"
        className={dense ? classes.denseBox : undefined}
        inputProps={{'aria-label': contents}}
        onChange={toggle}
      />
      <InputBase
        className={`${classes.contents} ${done ? classes.done : ''} ${dense ? classes.denseContents : ''}`}
        value={draft}
        readOnly={!update}
        fullWidth
        inputProps={{'aria-label': 'task contents'}}
        onChange={event => setDraft(event.target.value)}
        onBlur={commit}
        onKeyDown={event => {
          if (event.key === 'Enter') {
            event.preventDefault();
            commit();
          }
        }}
      />
    </span>
  );
}

export default Task;
export {taskSource};
