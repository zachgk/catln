import React from 'react';

function tagJoin(lst, joinWith) {
  return lst.reduce((acc, x) => acc == null ? [x] : <>{acc}{joinWith}{x}</>, null);
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
    console.error("Unknown renderMeta");
    return "";
  }
}

function renderObj(obj, objDetails) {
  const [objM, objBasis, name, vars, args] = obj;

  let showVars;
  if(Object.keys(vars).length > 0) {
    showVars = (
      <span>
        &lt;
        {tagJoin(Object.keys(vars).map(v => <>{renderType(vars[v])} {v}</>), ", ")}
        &gt;
      </span>);
  }

  let showArgs;
  if(Object.keys(args).length > 0) {
    showArgs = (
      <span>
      (
        {tagJoin(Object.keys(args).map(arg => <>{renderType(args[arg][0])} {arg}</>), ", ")}
      )
      </span>);
  }

  let showObjDetails;
  if(objDetails) {
    showObjDetails = (<span style={objDetails}>{objBasis} - {renderType(objM)}</span>);
  }

  return (<span>
            {showObjDetails}
            <span> {name}{showVars}{showArgs}</span>
          </span>
         );
}

export {
  tagJoin,
  renderGuard,
  renderType,
  renderObj
};
