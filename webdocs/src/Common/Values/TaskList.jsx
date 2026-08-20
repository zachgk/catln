import React, {useMemo} from 'react';

import TaskRows from './TaskRows';

// The tasks of every note the annotation covers, as one list. This decides which tasks there are and
// what order they are in; what a row looks like and where a change to one is written is TaskRows.
function TaskList(props) {
  const tasksArg = props["/tasks"];
  const tasks = useMemo(() => JSON.parse(tasksArg.contents), [tasksArg]);

  // A doc server whose notes hold no checkboxes has an empty list to show, which is an answer rather
  // than a blank page: nothing on screen reads as a list that failed to arrive.
  if (tasks.length === 0) {
    return <div>No tasks.</div>;
  }

  return <TaskRows tasks={tasks} />;
}

export default TaskList;
