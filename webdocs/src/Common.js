import React, {useState, useEffect} from 'react';

import SyntaxHighlighter from 'react-syntax-highlighter';

function tagJoin(lst, joinWith) {
  return lst.reduce((acc, x) => acc == null ? [x] : <>{acc}{joinWith}{x}</>, null);
}

const useStyles = {
  notes: {
    all: {
      whiteSpace: 'pre-wrap'
    },
    error: {
      background: 'lightCoral'
    },
    warning: {
      background: 'moccasin'
    }
  }
};

function useApi(path) {
  const [result, setResult] = useState({
    error: null,
    isLoaded: false,
    data: null,
    notes: []
  });
  useEffect(() => {
    fetch(path)
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
  let notes = props.notes;
  return (
    <div>
      {notes.map((note, noteIndex) => <Note key={noteIndex} note={note} />)}
    </div>
  );
}

function Note(props) {
  let note = props.note;
  var styles = useStyles.notes.all;
  switch(note.tp) {
  case "CNoteError":
    styles = {...styles, ...useStyles.notes.error};
    break;
  case "CNoteWarning":
    styles = {...styles, ...useStyles.notes.warning};
    break;
  default:
    console.error("Unknown note type", note);
  }

  return (
    <pre style={styles}>{note.msg}</pre>
  );
}

function Guard(props) {
  const {guard, Expr, showExprMetas, Meta} = props;
  switch(guard.tag) {
  case "IfGuard":
    return <span>if <Expr expr={guard.contents} Meta={Meta} showMetas={showExprMetas}/></span>;
  case "ElseGuard":
    return "else";
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
    return "TopType";
  case "TypeVar":
    return t.contents.contents;
  case "SumType":
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

        partials.push(<span key={[partialName, partialIndex]}>{partialName.contents}{showVars}{showArgs}</span>);
      });
    });
    return tagJoin(partials, " | ");
  default:
    console.error("Unknown type", t);
    return "";
  }
}

function Obj(props) {
  const {obj, details, Meta} = props;
  const [objM, objBasis, name, vars, args] = obj;

  let showVars;
  if(Object.keys(vars).length > 0) {
    showVars = (
      <span>
        &lt;
        {tagJoin(Object.keys(vars).map(v => <span key={v}><Meta data={vars[v]}/> {v}</span>), ", ")}
        &gt;
      </span>);
  }

  let showArgs;
  if(Object.keys(args).length > 0) {
    showArgs = (
      <span>
      (
        {tagJoin(Object.keys(args).map(arg => <span key={arg}><Meta data={args[arg][0]}/> {arg}</span>), ", ")}
      )
      </span>);
  }

  let showObjDetails;
  if(details) {
    showObjDetails = (<span style={details}>{objBasis} - <Meta data={objM}/></span>);
  }

  return (<span>
            {showObjDetails}
            <span> {name}{showVars}{showArgs}</span>
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
  tagJoin,
  useApi,
  Loading,
  Notes,
  Guard,
  Type,
  Obj,
  Val
};
