import React, {useState, useEffect} from 'react';

function tagJoin(lst, joinWith) {
  return lst.reduce((acc, x) => acc == null ? [x] : <>{acc}{joinWith}{x}</>, null);
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
      .then(res => res.json())
      .then(
        (res) => {
          if(res.length === 2) {
            setResult({
              isLoaded: true,
              data: res[0],
              notes: res[1]
            });
          } else {
            setResult({
              isLoaded: true,
              notes: res[0]
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
  const {error, isLoaded} = props.status;

  if (error) {
    return <div>Error: {error.message}</div>;
  } else if (!isLoaded) {
    return <div>Loading...</div>;
  } else {
    return props.children;
  }
}

function renderGuard(guard, exprRenderer) {
  switch(guard.tag) {
  case "IfGuard":
    return `if (${exprRenderer(guard.contents[0])})`;
  case "ElseGuard":
    return "else";
  case "NoGuard":
    return "";
  default:
    console.error("Unknown guard: ", guard);
    return "";
  }
}

function renderType(t) {
  switch(t.tag) {
  case "TopType":
    return "TopType";
  case "TypeVar":
    return t.contents.contents;
  case "SumType":
    let partials = t.contents.map(partialOptions => {
      let [partialName, options] = partialOptions;
      return options.map(partialData => {
        let [partialVars, , partialArgs] = partialData;

        let showVars = "";
        if(Object.keys(partialVars).length > 0) {
          showVars = (
            <span>
              &lt;
              {tagJoin(Object.keys(partialVars).map(v => <>{renderType(partialVars[v])} {v}</>), ", ")}
              &gt;
            </span>
          );
        }

        let showArgs = "";
        if(Object.keys(partialArgs).length > 0) {
          showArgs = (
              <span>
              (
                {tagJoin(Object.keys(partialArgs).map(arg => <>{renderType(partialArgs[arg])} {arg}</>), ", ")}
              )
            </span>
          );
        }

        return (<span>{partialName.contents}{showVars}{showArgs}</span>);
      });
    }).flat();
    return tagJoin(partials, " | ");
  default:
    console.error("Unknown type", t);
    return "";
  }
}

function renderObj(obj, objDetails, renderMeta) {
  const [objM, objBasis, name, vars, args] = obj;

  let showVars;
  if(Object.keys(vars).length > 0) {
    showVars = (
      <span>
        &lt;
        {tagJoin(Object.keys(vars).map(v => <>{renderMeta(vars[v])} {v}</>), ", ")}
        &gt;
      </span>);
  }

  let showArgs;
  if(Object.keys(args).length > 0) {
    showArgs = (
      <span>
      (
        {tagJoin(Object.keys(args).map(arg => <>{renderMeta(args[arg][0])} {arg}</>), ", ")}
      )
      </span>);
  }

  let showObjDetails;
  if(objDetails) {
    showObjDetails = (<span style={objDetails}>{objBasis} - {renderMeta(objM)}</span>);
  }

  return (<span>
            {showObjDetails}
            <span> {name}{showVars}{showArgs}</span>
          </span>
         );
}

export {
  tagJoin,
  useApi,
  Loading,
  renderGuard,
  renderType,
  renderObj
};
