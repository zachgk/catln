import React, {useState, useContext} from 'react';

import makeStyles from '@mui/styles/makeStyles';
import {Link} from 'react-router-dom';

import {ConfigContext, ReloadContext, writeExpr} from '../Common';
import {Value} from '../Value';

const useStyles = makeStyles({
  task: {
    display: 'flex',
    alignItems: 'center'
  },
  value: {
    flexGrow: 1
  },
  page: {
    marginLeft: '1em',
    fontSize: '0.85em'
  },
  error: {
    color: 'red'
  }
});

// A list of tasks gathered from notes, each row the task's own value beside the note it was written
// in. What a task looks like is whatever shows a task; this decides only how a row of a list is laid
// out and where a change to one is written.
//
// A row is written back to the note it was read from, which is not the page displaying the list. So
// it does not use whatever update the display was given -- that one replaces the annotation the list
// came from -- and names instead the page and expression the task carries. Once the file is written
// the display is asked to read itself again, so what is on screen is what the notes now say rather
// than what was asked for.
//
// One place decides this for both the list of every task there is and the list a note's query found,
// so a row of either is laid out and edited the same way.
function TaskRows({tasks}) {
  const reload = useContext(ReloadContext);
  const config = useContext(ConfigContext);
  const canEdit = config.isLoaded && !config.data?.readonly;
  const [error, setError] = useState(null);
  const classes = useStyles();

  const save = async (task, src) => {
    setError(null);
    const res = await writeExpr(task.ptPage, task.ptExpr, src);
    if (res.ok) {
      reload();
    } else {
      setError(res.message);
    }
  };

  return (
    <div>
      {error && <div className={classes.error} data-testid="task-list-error">save failed: {error}</div>}
      {tasks.map(task => (
        <div key={task.ptExpr} className={classes.task}>
          <span className={classes.value}>
            <Value data={task.ptTask} update={canEdit ? (src => save(task, src)) : undefined} />
          </span>
          <Link className={classes.page} to={`/docs/${encodeURIComponent(task.ptPage)}`}>{noteName(task.ptPage)}</Link>
        </div>
      ))}
    </div>
  );
}

// The name a note is known by, which is the file it is written in without the path to it. The whole
// path is what the link goes to; a list of tasks gathered from a few notes is read by which note
// each came from rather than by where those notes sit.
function noteName(page) {
  return page.split('/').pop();
}

export default TaskRows;
