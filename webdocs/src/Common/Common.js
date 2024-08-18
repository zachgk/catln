import React, {useState, useEffect} from 'react';

import TreeView from '@material-ui/lab/TreeView';
import ExpandMoreIcon from '@material-ui/icons/ExpandMore';
import ChevronRightIcon from '@material-ui/icons/ChevronRight';
import TreeItem from '@material-ui/lab/TreeItem';
import { makeStyles } from '@material-ui/core/styles';
import {useLocation, Link} from 'react-router-dom';


function tagJoin(lst, joinWith) {
  return lst.reduce((acc, x) => acc == null ? [x] : <>{acc}{joinWith}{x}</>, null);
}

const useStyles = makeStyles( theme => ({
  notesError: {
    whiteSpace: 'pre-wrap',
    background: theme.palette.error.main,
  },
  notesWarning: {
    whiteSpace: 'pre-wrap',
    background: theme.palette.warning.main
  },
  partialNameTp: {
    color: 'green'
  },
  partialNameClass: {
    color: 'purple'
  },
  partialNameRelative: {
    color: 'black'
  },
  keyword: {
    fontWeight: 'bold'
  },
  typevar: {
    fontWeight: 'bold'
  }
}));

const TocContext = React.createContext({});

function useQuery() {
  return new URLSearchParams(useLocation().search);
}

function useApi(path) {
  const [result, setResult] = useState({
    error: null,
    isLoaded: false,
    data: null,
    notes: []
  });
  useEffect(() => {
    fetch(path)
      .then(res => {
        if(res.status !== 200) {
          throw new Error(res.statusText);
        } else {
          return res;
        }
      })
      .then(res => res.json())
      .then(
        (res) => {
          switch(res.tag) {
          case "Success":
            setResult({
              isLoaded: true,
              data: res.contents[0],
              notes: res.contents[1]
            });
            break;
          case "ResFail":
            setResult({
              isLoaded: true,
              notes: res.contents
            });
            break;
          default:
            setResult({
              isLoaded: true,
              notes: [],
              error: "Unknown result type"
            });
          }
        },
        // Note: it's important to handle errors here
        // instead of a catch() block so that we don't swallow
        // exceptions from actual bugs in components.
        (error) => {
          setResult({
            isLoaded: true,
            error
          });
        }
      );
  }, [path]);
  return result;
}

function Loading(props) {
  const {error, isLoaded, data, notes} = props.status;

  if (error) {
    return <div>Error: {error.message}</div>;
  } else if (!isLoaded) {
    return <div>Loading...</div>;
  } else if (!data){
    return <div><Notes notes={notes} /></div>;
  } else {
    return props.children;
  }
}

function Notes(props) {
  let {notes, noPosOnly} = props;

  if(noPosOnly) {
    notes = notes.filter(note => !note.pos);
  }

  return (
    <div>
      {notes.map((note, noteIndex) => <Note key={noteIndex} note={note} />)}
    </div>
  );
}

function Note(props) {
  let note = props.note;
  const classes = useStyles();

  let noteClass;
  switch(note.tp) {
  case "CNoteError":
    noteClass = classes.notesError;
    break;
  case "CNoteWarning":
    noteClass = classes.notesWarning;
    break;
  default:
    console.error("Unknown note type", note);
  }

  return (
    <pre className={noteClass}>{note.msg}</pre>
  );
}

function PartialType(props) {
  const {ptName, ptVars, ptArgs, ptPreds, ptArgMode} = props.data;

  let showVars = "";
  if(Object.keys(ptVars).length > 0) {
    showVars = (
      <span>
        [
        {tagJoin(ptVars.map((v, index) => <span key={index}><PartialKey data={v[0]}/><TypeWithPrefix prefix=": " data={v[1]}/></span>), ", ")}
        ]
      </span>
    );
  }

  let showArgs = "";
  if(Object.keys(ptArgs).length > 0) {
    showArgs = (
        <span>
        (
          {tagJoin(ptArgs.map((v, index) => <span key={index}><PartialKey data={v[0]}/><TypeWithPrefix prefix=": " data={v[1]}/></span>), ", ")}
        )
      </span>
    );
  }

  let showArgMode;
  if (ptArgMode.tag === "PtArgAny") {
    showArgMode = "..";
  }

  let showPreds;
  if (ptPreds.length > 0) {
    console.error("Show type preds", ptPreds);
  }

  return (
    <span><PTypeName name={ptName}/>{showVars}{showArgs}{showArgMode}{showPreds}</span>
  );
}

function PartialKey(props) {
  const {pkName, pkArgs, pkVars} = props.data;
  if (pkArgs.length > 0 || pkVars.length > 0) {
    return "PartialKeyWith";
  }
  return <PTypeName name={pkName} />;
}

function TypePredicate(props) {
  const {data} = props;
  switch(data.tag) {
  case "PredClass":
    return <span>Class[{<PartialType data={data.contents}/>}]</span>;
  case "PredRel":
    return <span>Rel[{<PartialType data={data.contents}/>}]</span>;
  case "PredExpr":
    return <PartialType data={data.contents}/>;
  default:
    console.error("Unknown TypePredicate", data);
    return "???";
  }
}

function TypePredicates(props) {
  const {data} = props;
  switch(data.tag) {
  case "PredsOne":
    return <TypePredicate data={data.contents}/>;
  case "PredsAnd":
    return tagJoin(data.contents.map((ps, index) => <TypePredicates key={index} data={ps}/>), " && ");
  case "PredsNot":
    return <span>!<TypePredicate data={data.contents}/></span>;
  default:
    console.error("Unknown TypePredicates", data);
    return "???";
  }
}

function isTopType(t) {
  if (t.tag !== "TopType") return false;

  const [topNeg, topPreds] = t.contents;
  return Object.keys(topNeg).length === 0 && topPreds.tag === "PredsAnd" && topPreds.contents.length === 0;
}

function TypeWithPrefix(props) {
  const {prefix, data} = props;
  if (isTopType(data)) {
    return "";
  }
  return <span>{prefix}<Type data={data}/></span>;
}

function Type(props) {
  let t = props.data;
  switch(t.tag) {
  case "TopType":
    const [topNeg, topPreds] = t.contents;
    if (isTopType(t)) {
      return "";
    } else {
      let showTopNeg;
      if (Object.keys(topNeg).length !== 0) {
        let n = {tag: "UnionType", contents: topNeg};
        showTopNeg = <span> - <Type data={n}/></span>;
      }

      let showTopPreds;
      if (topPreds.tag !== "PredsAnd" || topPreds.contents.length !== 0) {
        showTopPreds = <span> | <TypePredicates data={topPreds}/></span>;
      }
      return <span>Any{showTopNeg}{showTopPreds}</span>;
    }
  case "TypeVar":
    return <TypeVar><PartialKey data={t.contents[0].contents} /></TypeVar>;
  case "UnionType":
    var partials = [];
    Object.entries(t.contents).forEach(partialOptions => {
      let [partialName, options] = partialOptions;
      options.forEach((partialData, partialIndex) => {
        let [partialVars, partialArgs, preds, argMode] = partialData;
        let ptData = {ptName: partialName, ptVars: partialVars, ptArgs: partialArgs, ptPreds: preds, ptArgMode: argMode};
        partials.push(<PartialType key={[partialName, partialIndex]} data={ptData}/>);
      });
    });
    return tagJoin(partials, " || ");
  default:
    console.error("Unknown type", t);
    return "";
  }
}

function KeyWord(props) {
  const classes = useStyles();
  return <span className={classes.keyword}>{props.children}</span>;
}

function TypeVar(props) {
  const classes = useStyles();
  return <span className={classes.typevar}>{props.children}</span>;
}

function PTypeName(props) {
  return <PartialName name={{tag: "PTypeName", contents: props.name}}/>;
}

function PClassName(props) {
  return <PartialName name={{tag: "PClassName", contents: props.name}}/>;
}

function PartialName(props) {
  const {name} = props;
  const classes = useStyles();

  let cls, link;
  switch(name.tag) {
  case "PRelativeName":
    cls = classes.partialNameRelative;
    link = `/relative/${name.contents}`;
    break;
  case "PTypeName":
    cls = classes.partialNameTp;
    link = `/type/${name.contents}`;
    break;
  case "PClassName":
    cls = classes.partialNameClass;
    link = `/class/${name.contents}`;
    break;
  default:
    console.error("Unknown partial name", name);
  }
  return <Link to={link} className={cls}>{name.contents}</Link>;
}

function ReachesTree(props) {
  const {tree} = props;
  return (
      <TreeView
        defaultCollapseIcon={<ExpandMoreIcon />}
        defaultExpandIcon={<ChevronRightIcon />}
      >
        <ReachesTreeNode tree={tree} id={"0"} />
      </TreeView>
  );
}

function ReachesTreeNode(props) {
  const {tree, id} = props;
  switch(tree.tag) {
  case "ReachesPartialTree":
    return tree.contents.map((node, nodeIndex) => {
      let lab = <PartialType data={node[0]}/>;
      let showSubNode;
      if (node[1].tag !== "ReachesLeaf") {
        showSubNode = <ReachesTreeNode id={id + nodeIndex + "-"} tree={node[1]}/>;
      }
      return (
        <TreeItem nodeId={id + nodeIndex} label={lab}>
          {showSubNode}
        </TreeItem>
      );
    });
  case "ReachesTypeTree":
    return tree.contents.map((node, nodeIndex) => {
      let lab = <span><Type data={node[0]}/> by {node[1][0]}</span>;
      let subNodeDat = node[1][1];
      if (node[0].tag === "UnionType" && subNodeDat.tag === "ReachesPartialTree" && subNodeDat.contents.length === 1) {
        subNodeDat = subNodeDat.contents[0][1];
      }
      let showSubNode;
      if (subNodeDat.tag !== "ReachesLeaf") {
        showSubNode = <ReachesTreeNode id={id + nodeIndex + "-"} tree={subNodeDat}/>;
      }
      return (
        <TreeItem nodeId={id + nodeIndex} label={lab}>
          {showSubNode}
        </TreeItem>
      );
    });
  case "ReachesLeaf":
    return (
      <TreeItem nodeId={id} label={"ReachesLeaf"}>
      </TreeItem>
    );
  default:
    console.error("Unknown ReachesTreeNode", tree);
    return (
      <TreeItem nodeId={id} label={"TREE"}>
      </TreeItem>
    );
  }
}

export {
  TocContext,
  tagJoin,
  useQuery,
  useApi,
  Loading,
  Notes,
  PartialKey,
  PartialType,
  isTopType,
  Type,
  KeyWord,
  TypeVar,
  PTypeName,
  PClassName,
  PartialName,
  ReachesTree
};
