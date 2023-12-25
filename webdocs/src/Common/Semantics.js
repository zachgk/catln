import React  from 'react';

import {PartialKey, PTypeName, Type} from './Common';

function Expr(props) {
  let {expr, Meta, showMetas} = props;
  switch(expr.tag) {
  case "CExpr":
    return "" + expr.contents[1].contents;
  case "Value":
    let showValM;
    if (showMetas) {
      showValM = <Meta data={expr.contents[0]}/>;
    }
    return <span><PTypeName name={expr.contents[1]} />{showValM}</span>;
  case "HoleExpr":
    let showHoleM;
    if (showMetas) {
      showHoleM = <Meta data={expr.contents[0]}/>;
    }
    return <span>_{showHoleM}</span>;
  case "AliasExpr":
    return <span><Expr expr={expr.contents[0]} Meta={Meta} showMetas={showMetas}/>@<Expr expr={expr.contents[1]} Meta={Meta} showMetas={showMetas} /></span>;
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

    return <span>{showBase}{showBaseM}(<ObjArr oa={arg} Meta={Meta} showExprMetas={showMetas} />){showM}</span>;
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
  const {Meta, showMetas, expr} = props;
  const {rgeExpr, rgeGuard} = expr;

  let showGuard;
  if (rgeGuard) {
    showGuard = <span> | <Expr expr={rgeGuard} Meta={Meta} showMetas={showMetas}/></span>;
  }

  return <span><Expr expr={rgeExpr} Meta={Meta} showMetas={showMetas} />{showGuard}</span>;
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

    let showArrType;
    if (oaArrM[0].tag !== "TopType" || oaArrM[0].contents !== []) {
      showArrType = <span> -&gt; <Type data={oaArrM[0]} /></span>;
    }
    let showArrM;
    if (showExprMetas) {
      showArrM = <Meta data={oaArrM}/>;
    }
    showArr = <span>{showArrType}{showArrM}{showArrExpr}</span>;
  }

  return <span>{showObj}{showArr}</span>;
}

export {
  Expr,
  GuardExpr,
  ObjArr,
}
