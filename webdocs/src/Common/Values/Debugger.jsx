import React, {useMemo} from 'react';

import {SimpleTreeView} from '@mui/x-tree-view/SimpleTreeView';
import ExpandMoreIcon from '@mui/icons-material/ExpandMore';
import ChevronRightIcon from '@mui/icons-material/ChevronRight';
import {TreeItem} from '@mui/x-tree-view/TreeItem';

import {ObjArr} from '../Semantics';
import {Value} from '../Value';

function Debugger(props) {
  const fun = props["/fun"].contents;
  const run = props["/run"];

  // The run rides on the compiled value, so nothing is fetched. It was made under a mock IO handle,
  // which is why the output below is here to show rather than on the doc server's own stdout.
  const {drExit, drOutput, drResult} = useMemo(() => JSON.parse(run.contents), [run]);

  // An empty tree means the run never entered an arrow, which a program that failed before its first
  // call can do. Say so rather than rendering an undefined node.
  const [root] = drResult.erTreebug;

  return (
    <div>
      <h2>{fun}</h2>
      {root ? (
        <SimpleTreeView slots={{collapseIcon: ExpandMoreIcon, expandIcon: ChevronRightIcon}}>
          <TreebugNode data={root} />
        </SimpleTreeView>
      ) : <p>Evaluated no arrows.</p>}
      <h3>Output</h3>
      {drOutput ? <pre>{drOutput}</pre> : <p>Printed nothing.</p>}
      <p>Exited with {drExit}.</p>
    </div>
  );
}

function TreebugNode(props) {
  let [eitherOa, input, val, children, id] = props.data;

  let oa;
  if ("Left" in eitherOa) {
    oa = eitherOa["Left"];
  } else {
    oa = eitherOa["Right"];
  }

  let label = <span><ObjArr oa={oa} /> |::| <Value data={input} /> =&gt; <Value data={val} /></span>;
  return (
    <TreeItem itemId={id} label={label}>
      {children.map(child => <TreebugNode data={child} key={child[4]} />)}
    </TreeItem>
  );
}

export default Debugger;
