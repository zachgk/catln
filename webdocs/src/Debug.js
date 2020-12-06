import React from 'react';

import TreeView from '@material-ui/lab/TreeView';
import ExpandMoreIcon from '@material-ui/icons/ExpandMore';
import ChevronRightIcon from '@material-ui/icons/ChevronRight';
import TreeItem from '@material-ui/lab/TreeItem';

import {useApi, tagJoin, Loading, Obj} from './Common';

function Debug(props) {
  let apiResult = useApi("/treebug");

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

  let label = <span><Obj obj={obj} Meta={ObjMeta}/> => <Val val={val} /></span>;
  return (
    <TreeItem nodeId={id} label={label}>
      {children.map(child => <TreebugNode data={child} key={child[4]} />)}
    </TreeItem>
  );
}

function ObjMeta(props) {
  return "";
}

function Val(props) {
  let {val} = props;
  switch(val.tag) {
  case "IntVal":
  case "FloatVal":
  case "StrVal":
    return <span>{val.contents}</span>;
  case "TupleVal":

    let showArgs = "";
    if(Object.keys(val.args).length > 0) {
      showArgs = (
        <span>
        (
          {tagJoin(Object.keys(val.args).map(arg => <>{arg} = <Val key={arg} val={val.args[arg]}/></>), ", ")}
        )
        </span>
      );
    }

    return <span>{val.name}{showArgs}</span>;
  case "IOVal":
    return <span>IOVal</span>;
  case "NoVal":
    return <span>NoVal</span>;
  default:
    console.error("Unknown val", val);
    return <span>Unknown Val</span>;
  }
}

export default Debug;
