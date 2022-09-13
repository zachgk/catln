import React, {useState, useEffect} from 'react';

import { makeStyles } from '@material-ui/core/styles';
import {Link} from 'react-router-dom';

import SyntaxHighlighter from 'react-syntax-highlighter';

import {Comment} from './DocsPage';

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

function posKey(pos) {
  const f = p => `${p.name}-${p.line}-${p.col}`;
  return `${f(pos[0])}_${f(pos[1])}_${pos[2]}`;
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

function Guard(props) {
  const {guard, Expr, showExprMetas, Meta} = props;
  switch(guard.tag) {
  case "IfGuard":
    return <span> <KeyWord>if</KeyWord> <Expr expr={guard.contents} Meta={Meta} showMetas={showExprMetas}/></span>;
  case "ElseGuard":
    return <span> <KeyWord>else</KeyWord></span>;
  case "NoGuard":
    return "";
  default:
    console.error("Unknown guard: ", guard);
    return "";
  }
}

function Type(props) {
  let t = props.data;
  switch(t.tag) {
  case "TopType":
    return "";
  case "TypeVar":
    return <TypeVar>{t.contents.contents}</TypeVar>;
  case "UnionType":
    var partials = [];
    t.contents.forEach(partialOptions => {
      let [partialName, options] = partialOptions;
      options.forEach((partialData, partialIndex) => {
        let [partialVars, , partialArgs] = partialData;

        let showVars = "";
        if(Object.keys(partialVars).length > 0) {
          showVars = (
            <span>
              &lt;
            {tagJoin(Object.keys(partialVars).map(v => <span key={v}><Type data={partialVars[v]}/> {v}</span>), ", ")}
              &gt;
            </span>
          );
        }

        let showArgs = "";
        if(Object.keys(partialArgs).length > 0) {
          showArgs = (
              <span>
              (
                {tagJoin(Object.keys(partialArgs).map(arg => <span key={arg}><Type data={partialArgs[arg]}/> {arg}</span>), ", ")}
              )
            </span>
          );
        }

        partials.push(<span key={[partialName, partialIndex]}><PartialName name={partialName}/>{showVars}{showArgs}</span>);
      });
    });
    return tagJoin(partials, " | ");
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


function Obj(props) {
  const {obj, details, Meta} = props;
  let {deprecatedObjM: objM, objBasis, deprecatedObjPath: objPath, deprecatedObjVars: objVars, deprecatedObjArgs: objArgs, objDoc} = obj;
  let objName = objPath;

  let showContext;
  if(objName === "/Catln/Context") {
    const ctxArgs = Object.assign({}, objArgs);
    delete ctxArgs.value;
    showContext = (
      <span>
        &#123;
        {tagJoin(Object.keys(ctxArgs).map(arg => <span key={arg}><Meta data={ctxArgs[arg][0]}/> {arg}</span>), ", ")}
        &#125;
      </span>);
    let valueObj = objArgs.value[1];
    objM = valueObj.objM;
    objBasis = valueObj.objBasisi;
    objName = valueObj.objPath;
    objVars = valueObj.objVars;
    objArgs = valueObj.objArgs;
    objDoc = valueObj.objDoc;
  }

  let showVars;
  if(Object.keys(objVars).length > 0) {
    showVars = (
      <span>
        &lt;
        {tagJoin(Object.keys(objVars).map(v => <span key={v}><Meta data={objVars[v]}/> <TypeVar>{v}</TypeVar></span>), ", ")}
        &gt;
      </span>);
  }

  let showCaller;
  let argsCall = Object.assign({}, objArgs);
  if('this' in objArgs) {
    showCaller = <span><Meta data={objArgs['this'][0]}/>.</span>;
    delete argsCall['this'];
  }

  let showArgs;
  if(Object.keys(argsCall).length > 0) {
    showArgs = (
      <span>
      (
        {tagJoin(Object.keys(argsCall).map(arg => <span key={arg}><Meta data={argsCall[arg][0]}/> {arg}</span>), ", ")}
      )
      </span>);
  }

  let showObjDetails;
  let showDoc = null;
  if(details) {
    if(objDoc) {
      showDoc = (<div><Comment comment={objDoc} obj={obj} /></div>);
    }
    showObjDetails = (<span className={details}>{objBasis} - <Meta data={objM}/></span>);
  }

  return (<span>
            {showDoc}
            {showObjDetails}
            <span>{showCaller}<PTypeName name={objName}/>{showContext}{showVars}{showArgs}</span>
          </span>
         );
}

function Val(props) {
  let val = props.data;
  switch(val.tag) {
  case "IntVal":
  case "FloatVal":
  case "StrVal":
    return <span>{val.contents}</span>;
  case "TupleVal":
    switch(val.name) {
    case "CatlnResult":
      return <CatlnResult data={val}/>;
    default:
      let showArgs = "";
      if(Object.keys(val.args).length > 0) {
        showArgs = (
          <span>
            (
            {tagJoin(Object.keys(val.args).map(arg => <span key={arg}>{arg} = <Val key={arg} data={val.args[arg]}/></span>), ", ")}
            )
          </span>
        );
      }

      return <span>{val.name}{showArgs}</span>;
    }
  case "IOVal":
    return <span>IOVal</span>;
  case "NoVal":
    return <span>NoVal</span>;
  default:
    console.error("Unknown val tag", val);
    return <span>Unknown Val tag</span>;
  }
}

function CatlnResult(props) {
  let data = props.data;
  let fileName = data.args.name.contents;
  let fileContents = data.args.contents.contents;
  if(fileName.endsWith(".ll")) {
    return (
      <SyntaxHighlighter language="llvm">
        {fileContents}
      </SyntaxHighlighter>
    );
  } else if(fileName.endsWith(".html")) {
    return <iframe srcDoc={fileContents} title={fileName} />;
  } else {
    return (
      <pre>{fileContents}</pre>
    );
  }
}

export {
  TocContext,
  tagJoin,
  posKey,
  useApi,
  Loading,
  Notes,
  Guard,
  Type,
  KeyWord,
  TypeVar,
  PTypeName,
  PClassName,
  PartialName,
  Obj,
  Val
};
