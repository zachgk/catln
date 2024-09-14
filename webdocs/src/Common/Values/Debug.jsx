import React from 'react';

import {SimpleTreeView} from '@mui/x-tree-view/SimpleTreeView';
import ExpandMoreIcon from '@mui/icons-material/ExpandMore';
import ChevronRightIcon from '@mui/icons-material/ChevronRight';
import {TreeItem} from '@mui/x-tree-view/TreeItem';

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
      <SimpleTreeView slots={{collapseIcon: ExpandMoreIcon, expandIcon: ChevronRightIcon}}>
        <TreebugNode data={data} />
      </SimpleTreeView>
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

export default Debug;
