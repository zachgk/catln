import React from 'react';

import TreeView from '@material-ui/lab/TreeView';
import ExpandMoreIcon from '@material-ui/icons/ExpandMore';
import ChevronRightIcon from '@material-ui/icons/ChevronRight';
import TreeItem from '@material-ui/lab/TreeItem';

import {useApi, Loading} from '../Common';
import {ObjArr} from '../Semantics';
import {Value} from '../Value';

function Debug(props) {
  const { prgmName, fun } = props;
  let apiResult = useApi(`/api/treebug?prgmName=${prgmName}&function=${fun}`);

  return (
    <Loading status={apiResult}>
    <Main data={apiResult.data} />
    </Loading>
  );
}

function Main(props) {
  let data = props.data.erTreebug[0];
  return (
      <TreeView
        defaultCollapseIcon={<ExpandMoreIcon />}
        defaultExpandIcon={<ChevronRightIcon />}
      >
        <TreebugNode data={data} />
      </TreeView>
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
    <TreeItem nodeId={id} label={label}>
      {children.map(child => <TreebugNode data={child} key={child[4]} />)}
    </TreeItem>
  );
}

export default Debug;
