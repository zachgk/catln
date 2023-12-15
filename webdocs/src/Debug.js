import React from 'react';

import TreeView from '@material-ui/lab/TreeView';
import ExpandMoreIcon from '@material-ui/icons/ExpandMore';
import ChevronRightIcon from '@material-ui/icons/ChevronRight';
import TreeItem from '@material-ui/lab/TreeItem';
import {useParams} from 'react-router-dom';

import {Value} from './Common/Value';
import {useApi, Loading} from './Common/Common';

function Debug(props) {
  const { prgmName, fun } = useParams();
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
  let [obj, , val, children, id] = props.data;

  // let label = <span><Obj obj={obj} Meta={ObjMeta}/> => <Value data={val} /></span>;
  console.log(props);
  let label = "Label";
  return (
    <TreeItem nodeId={id} label={label}>
      {children.map(child => <TreebugNode data={child} key={child[4]} />)}
    </TreeItem>
  );
}

function ObjMeta(props) {
  return "";
}

export default Debug;
