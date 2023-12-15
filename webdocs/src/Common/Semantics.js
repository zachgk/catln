import React  from 'react';

import {PartialKey, PTypeName, Type} from './Common';

function Expr(props) {
  let {expr, Meta, showMetas} = props;
  switch(expr.tag) {
  case "CExpr":
    return "" + expr.contents[1].contents;
  case "Value":
    return <PTypeName name={expr.contents[1]} />;
  case "HoleExpr":
    return "_";
  case "TupleApply":
    const [m, [baseM ,base], arg] = expr.contents;

    let showBaseM;
    if(showMetas) {
      showBaseM = <i><Meta data={baseM}/></i>;
    }

    let showM;
    if(showMetas) {
      showM = <i>[<Meta data={m}/>]</i>;
    }

    let showBase = <Expr expr={base} Meta={Meta} showMetas={showMetas}/>;

    return <span>{showBase}({showBaseM} <ObjArr oa={arg} />){showM}</span>;
  case "VarApply":
    const [vm, vbase, varName, varVal] = expr.contents;

    let showvM;
    if(showMetas) {
      showM = <i>[<Meta data={vm}/>]</i>;
    }

    let showvBase = <Expr expr={vbase} Meta={Meta} showMetas={showMetas}/>;

    return <span>{showvBase}[<PartialKey data={varName} />: <Type data={varVal[0]} />]{showvM}</span>;
  default:
    console.error("Unknown renderExpr", expr);
    return "";
  }
}

function GuardExpr(props) {
  const {Meta, showExprMetas, expr} = props;
  const {rgeExpr, rgeGuard} = expr;

  let showGuard;
  if (rgeGuard) {
    showGuard = <span> | <Expr expr={rgeGuard} Meta={Meta} showMetas={showExprMetas}/></span>;
  }

  return <span><Expr expr={rgeExpr} Meta={Meta} showMetas={showExprMetas} />{showGuard}</span>;
}

function ObjArr(props) {
  const {Meta, showExprMetas, oa} = props;
  const {oaObj, oaArr} = oa;

  let showObj;
  if (oaObj) {
    showObj = <GuardExpr expr={oaObj} Meta={Meta} showMetas={showExprMetas} />;
  }

  let showArr;
  if (oaArr) {
    const [oaArrExpr, oaArrM] = oaArr;
    let showArrExpr;
    if (oaArrExpr) {
      showArrExpr = <span> = <GuardExpr expr={oaArrExpr} Meta={Meta} showMetas={showExprMetas}/></span>;
    }

    let showArrM;
    if (oaArrM[0].tag != "TopType" || oaArrM[0].contents != []) {
      showArrM = <span> -&gt; <Type data={oaArrM[0]} /></span>;
    }
    showArr = <span>{showArrM}{showArrExpr}</span>;
  }

  return <span>{showObj}{showArr}</span>;
}

export {
  Expr,
  GuardExpr,
  ObjArr,
}
