import React from 'react';

import makeStyles from '@mui/styles/makeStyles';

const useStyles = makeStyles({
  event: {
    display: 'flex',
    alignItems: 'center',
    gap: 6
  },
  // The bullet the line was written with, which is what tells an event from a task at a glance: a
  // task has a box, and an event has none because there is nothing to check off.
  bullet: {
    flex: 'none',
    color: 'gray'
  },
  // One line in a small space, as a dense task is: cut short where it does not fit, at the size a
  // dense task is written at so the two read as one size beside each other.
  dense: {
    minWidth: 0,
    fontSize: '0.85rem'
  },
  denseContents: {
    minWidth: 0,
    overflow: 'hidden',
    textOverflow: 'ellipsis',
    whiteSpace: 'nowrap'
  }
});

// One line of a note the Obsidian Day Planner reads a block of the day off without it being a task:
// a bullet such as "12:00 - 13:00 Lunch". It is shown as the line it was written as, since the time
// it writes is the time a planner draws it over and the rest is what happens then.
function NoteEvent(props) {
  const contents = props["/contents"].contents;
  const classes = useStyles();
  return (
    <span className={`${classes.event} ${props.dense ? classes.dense : ''}`} title={props.dense ? contents : undefined}>
      <span className={classes.bullet}>•</span>
      <span className={props.dense ? classes.denseContents : undefined}>{contents}</span>
    </span>
  );
}

export default NoteEvent;
