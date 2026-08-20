import React, {useMemo} from 'react';

import makeStyles from '@mui/styles/makeStyles';

import TaskRows from './TaskRows';

const useStyles = makeStyles({
  error: {
    color: 'red'
  }
});

// What the query a note wrote found: the tasks of its vault which are what it asked for, in the
// order it asked for. A row is the task's own value and is written back to the note it came from,
// which TaskRows decides for this and for the list of every task alike.
//
// A query nobody could read is shown where the reader wrote it rather than swallowed. It is an
// ordinary answer and not a failure of the page: the reader typed the query, so the line that was
// not understood belongs next to it, and the note around it is shown as it was written.
//
// The query itself is not edited from here. It is markdown of the note, so it is changed by editing
// the note; replacing the annotation with anything that is not the one the query was read into
// would leave the note unwriteable.
function TaskQuery(props) {
  const resultArg = props["/result"];
  const result = useMemo(() => JSON.parse(resultArg.contents), [resultArg]);
  const classes = useStyles();

  if (result.qrError) {
    return <div className={classes.error} data-testid="task-query-error">{result.qrError}</div>;
  }

  // A query which found nothing has an answer to show, which is that nothing it asked for is there.
  if (result.qrTasks.length === 0) {
    return <div>No tasks match this query.</div>;
  }

  return <TaskRows tasks={result.qrTasks} />;
}

export default TaskQuery;
