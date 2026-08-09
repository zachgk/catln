import React, {useState, useEffect} from 'react';

import { styled } from '@mui/material/styles';
import {SimpleTreeView} from '@mui/x-tree-view/SimpleTreeView';
import ExpandMoreIcon from '@mui/icons-material/ExpandMore';
import ChevronRightIcon from '@mui/icons-material/ChevronRight';
import {TreeItem} from '@mui/x-tree-view/TreeItem';
import makeStyles from '@mui/styles/makeStyles';
import {useLocation, Link} from 'react-router-dom';


function tagJoin(lst, joinWith) {
  return lst.reduce((acc, x) => acc == null ? [x] : <>{acc}{joinWith}{x}</>, null);
}

const ErrorPre = styled('pre')(({ theme }) => ({
  whiteSpace: 'pre-wrap',
  background: theme.palette.error.main,
}));

const WarningPre = styled('pre')(({ theme }) => ({
  whiteSpace: 'pre-wrap',
  background: theme.palette.warning.main,
}));

const useStyles = makeStyles({
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
});

const TocContext = React.createContext({});
const ConfigContext = React.createContext({});

// The page an annotation is being evaluated with respect to. A Value is rendered from deep inside
// RawExpr, which threads no page name, so a value that wants to ask the compiler a follow-up
// question about the same page reads it from here. Empty means the evaluation was not scoped to a
// page, which is what an annotation about the whole program wants.
const PrgmNameContext = React.createContext("");

// How the sidebar's page tree gets back out of the value that carries it. The tree is the compiled
// #sidebar annotation, so it arrives deep inside a RawExpr like any other value, but it is also the
// app's list of pages: the previous/next buttons and relative documentation links read it. The
// layout puts its setter here and the sidebar value publishes the tree it rendered.
const SidebarContext = React.createContext(() => {});

function useQuery() {
  return new URLSearchParams(useLocation().search);
}

// reloadKey is optional. Changing it refetches the same path, which is how a save
// picks up the rebuilt page.
function useApi(path, reloadKey) {
  const [result, setResult] = useState({
    error: null,
    isLoaded: false,
    data: null,
    notes: []
  });
  useEffect(() => {
    // A falsy path means the caller already has its data (such as a value that carries it) and
    // has nothing to fetch.
    if(!path) {
      return;
    }
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
  }, [path, reloadKey]);
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

  switch(note.tp) {
  case "CNoteError":
    return <ErrorPre>{note.msg}</ErrorPre>;
  case "CNoteWarning":
    return <WarningPre>{note.msg}</WarningPre>;
  default:
    console.error("Unknown note type", note);
  }
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

  // showPtArgMode: (lb, ub) is (T,T) exactly the listed args, (T,F) extras allowed,
  // (F,T) any subset, (F,F) unconstrained.
  let showArgMode = "";
  if (!ptArgMode.ptArgLb) {
    showArgMode = ptArgMode.ptArgUb ? "??" : "..??";
  } else if (!ptArgMode.ptArgUb) {
    showArgMode = "..";
  }

  let showPreds;
  if (isPredsNone(ptPreds)) {
    showPreds = "";
  } else if (ptPreds.tag === "PredsOne") {
    showPreds = <span> | <TypePredicate data={ptPreds.contents}/></span>;
  } else {
    showPreds = <span> | (<TypePredicates data={ptPreds}/>)</span>;
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

// PredsNone is the empty conjunction.
function isPredsNone(preds) {
  return preds.tag === "PredsAnd" && preds.contents.length === 0;
}

// tryPredsToList flattens a conjunction into its predicates, or gives null for any negation.
function tryPredsToList(preds) {
  switch(preds.tag) {
  case "PredsOne":
    return [preds.contents];
  case "PredsAnd": {
    let out = [];
    for(const sub of preds.contents) {
      const subList = tryPredsToList(sub);
      if(subList === null) return null;
      out = out.concat(subList);
    }
    return out;
  }
  default:
    return null;
  }
}

// The partials of a PartialLeafs map: a name maps to each option's [vars, args, preds, argMode].
function partialLeafs(leafs) {
  let partials = [];
  Object.entries(leafs).forEach(([partialName, options]) => {
    options.forEach((partialData, partialIndex) => {
      let [partialVars, partialArgs, preds, argMode] = partialData;
      let ptData = {ptName: partialName, ptVars: partialVars, ptArgs: partialArgs, ptPreds: preds, ptArgMode: argMode};
      partials.push(<PartialType key={`${partialName}/${partialIndex}`} data={ptData}/>);
    });
  });
  return partials;
}

// showJoinParts: no parts is the empty set, one shows bare, several are parenthesised.
function joinParts(parts) {
  if(parts.length === 0) return "∅";
  if(parts.length === 1) return parts[0];
  return <span>({tagJoin(parts, " + ")})</span>;
}

// Mirrors showTypeD's TypeVar cases. The aux is TVVar or TVArg over a PartialKey, or TVApplied
// wrapping another aux with per-key overrides; the location is TVInt (internal) or TVExt.
function TypeVarType(props) {
  const [aux, loc] = props.data;
  switch(aux.tag) {
  case "TVVar":
    return <TypeVar>{loc === "TVExt" ? "$_" : "$"}<PartialKey data={aux.contents}/></TypeVar>;
  case "TVArg":
    return <TypeVar>{loc === "TVExt" ? "TVArg(ext) " : "TVArg "}<PartialKey data={aux.contents}/></TypeVar>;
  case "TVApplied": {
    const [base, overrides] = aux.contents;
    const showOverrides = tagJoin(overrides.map((ovr, index) =>
      <span key={index}><PartialKey data={ovr[0]}/>=<Type data={ovr[1]}/></span>), ", ");
    return <span><TypeVarType data={[base, loc]}/>[{showOverrides}]</span>;
  }
  default:
    console.error("Unknown TypeVarAux", aux);
    return "???";
  }
}

function Constant(props) {
  const {data} = props;
  switch(data.tag) {
  case "CStr":
    return <span>{JSON.stringify(data.contents)}</span>;
  case "CChar":
    return <span>{`'${data.contents}'`}</span>;
  case "CInt":
  case "CFloat":
    return <span>{String(data.contents)}</span>;
  default:
    console.error("Unknown Constant", data);
    return "???";
  }
}

// PTopType: the unconstrained top, with no predicates, negative partials, partials or constants.
function isTopType(t) {
  if (t.tag !== "UnionType") return false;

  const [top, leafs, consts] = t.contents;
  if (top === null) return false;

  const [preds, negPartials] = top;
  return isPredsNone(preds)
    && Object.keys(negPartials).length === 0
    && Object.keys(leafs).length === 0
    && consts.length === 0;
}

function TypeWithPrefix(props) {
  const {prefix, data} = props;
  if (isTopType(data)) {
    return "";
  }
  return <span>{prefix}<Type data={data}/></span>;
}

// Mirrors showTypeD in src/Semantics/Types.hs. A Type is
// UnionType (Maybe (TypePredicates, PartialLeafs)) PartialLeafs [Constant]: an optional top part
// (Top narrowed by predicates, less its negative partials), the positive partials, and the
// constants. There is no separate TopType constructor.
function Type(props) {
  let t = props.data;
  switch(t.tag) {
  case "TypeVar":
    return <TypeVarType data={t.contents}/>;
  case "UnionType": {
    const [top, leafs, consts] = t.contents;

    if (top === null && consts.length === 0) {
      return joinParts(partialLeafs(leafs));
    }

    if (top !== null && Object.keys(leafs).length === 0 && consts.length === 0) {
      const [preds, negPartials] = top;

      let showTop;
      const predList = tryPredsToList(preds);
      if (predList !== null && predList.length === 1 && predList[0].tag === "PredClass") {
        showTop = <span>∀<PartialType data={predList[0].contents}/></span>;
      } else if (predList !== null && predList.length === 1 && predList[0].tag === "PredRel") {
        showTop = <span>~<PartialType data={predList[0].contents}/></span>;
      } else if (predList !== null && predList.length === 0) {
        // A bare top says nothing about the value, so the webdocs leave it out entirely.
        if (Object.keys(negPartials).length === 0) {
          return "";
        }
        showTop = "TopType";
      } else {
        showTop = <span>(TopType | <TypePredicates data={preds}/>)</span>;
      }

      let showNeg;
      if (Object.keys(negPartials).length !== 0) {
        showNeg = <span> - {joinParts(partialLeafs(negPartials))}</span>;
      }
      return <span>{showTop}{showNeg}</span>;
    }

    // The mixed forms have no shorthand; showTypeD spells the three components out.
    const showTopPart = top === null
      ? "Nothing"
      : <span>Just(<TypePredicates data={top[0]}/>, {joinParts(partialLeafs(top[1]))})</span>;
    const showConsts = tagJoin(consts.map((c, index) => <Constant key={index} data={c}/>), ", ");
    return (
      <span>UnionType({showTopPart}, {joinParts(partialLeafs(leafs))}, [{showConsts}])</span>
    );
  }
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

// Links to a page rendered by evaluating an annotation, rather than to a route that fetches for
// itself. Naming a page scopes the evaluation to it, which is how an annotation that reports on
// "the program it is in" is pointed at a particular one.
function annotLink(annot, prgmName) {
  const scope = prgmName ? `&prgmName=${encodeURIComponent(prgmName)}` : "";
  return `/annot?annot=${encodeURIComponent(annot)}${scope}`;
}

// The call that evaluates an annotation, which is what an annotLink route resolves to. It scopes
// the same way: naming a page compiles the annotation as though it were written there, and no name
// compiles it against every loaded file.
function annotApi(annot, prgmName) {
  const scope = prgmName ? `&prgmName=${encodeURIComponent(prgmName)}` : "";
  return `/api/annot?annot=${encodeURIComponent(annot)}${scope}`;
}

// A type or class page is a #type annotation, so linking to one means asking the compiler to
// evaluate that annotation rather than routing to a page that fetches for itself.
function typeAnnotLink(name) {
  return annotLink(`#type(name= "${name}")`);
}

function PartialName(props) {
  const {name} = props;
  const classes = useStyles();

  let cls, link;
  switch(name.tag) {
  case "PRelativeName":
    cls = classes.partialNameRelative;
    link = `/relative/${encodeURIComponent(name.contents)}`;
    break;
  case "PTypeName":
    cls = classes.partialNameTp;
    link = typeAnnotLink(name.contents);
    break;
  case "PClassName":
    cls = classes.partialNameClass;
    link = typeAnnotLink(name.contents);
    break;
  default:
    console.error("Unknown partial name", name);
  }
  return <Link to={link} className={cls}>{name.contents}</Link>;
}

function ReachesTree(props) {
  const {tree} = props;
  return (
      <SimpleTreeView slots={{collapseIcon: ExpandMoreIcon, expandIcon: ChevronRightIcon}}>
        <ReachesTreeNode tree={tree} id={"0"} />
      </SimpleTreeView>
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
        <TreeItem itemId={id + nodeIndex} label={lab}>
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
        <TreeItem itemId={id + nodeIndex} label={lab}>
          {showSubNode}
        </TreeItem>
      );
    });
  case "ReachesLeaf":
    return (
      <TreeItem itemId={id} label={"ReachesLeaf"}>
      </TreeItem>
    );
  default:
    console.error("Unknown ReachesTreeNode", tree);
    return (
      <TreeItem itemId={id} label={"TREE"}>
      </TreeItem>
    );
  }
}

export {
  TocContext,
  ConfigContext,
  PrgmNameContext,
  SidebarContext,
  annotLink,
  annotApi,
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
