import React  from 'react';

import {tryValue} from './Value';
import {KeyWord, PartialKey, PTypeName, isTopType, Type, tagJoin} from './Common';

function RawObjArr(props) {
  const {roaObj, roaArr, roaDef} = props.roa;

  let showObj;
  if (roaObj) {
    showObj = <RawExpr expr={roaObj} />;
  }

  let showArr;
  if (roaArr) {
    const [roaArrExpr, roaArrM] = roaArr;
    let showArrExpr;
    if (roaArrExpr) {
      showArrExpr = <RawExpr expr={roaArrExpr}/>;
    }

    let showArrM;
    if (!isTopType(roaArrM[0])) {
      showArrM = <span> -&gt; <Type data={roaArrM[0]} /></span>;
    }
    showArr = <span>{showArrM}{showArrExpr}</span>;
  }

  let showEq;
  if (showObj && roaArr && roaArr[0]) {
    showEq = " = ";
  }

  let showDef;
  if (roaDef) {
    showDef = <span> ? <RawExpr expr={roaDef}/></span>;
  }

  return <span>{showObj}{showEq}{showArr}{showDef}</span>;
}

function RawExpr(props) {
  let {expr} = props;

  switch(expr.tag) {
  case "RawCExpr":
    return "" + expr.contents[1].contents;
  case "RawValue":
    const [, val] = expr.contents;
    if (val === "") {
      return "()";
    }
    return <PTypeName name={val}/>;
  case "RawHoleExpr":
    return "_";
  case "RawMacroValue":
    return (<span>$ &#123;{expr.contents[1]}&#125;</span>);
  case "RawTheExpr":
    return <span><KeyWord>:</KeyWord><RawExpr expr={expr.contents}/></span>;
  case "RawSpread":
    return <span><RawExpr expr={expr.contents}/><KeyWord>..</KeyWord></span>;
  case "RawAliasExpr":
    return <span><RawExpr expr={expr.contents[0]}/>@<RawExpr expr={expr.contents[1]}/></span>;
  case "RawTupleApply":
    const [tupleM, [,base], args] = expr.contents;

    let fromVal = tryValue(tupleM[2][1]);
    if (fromVal) {
      return fromVal;
    }

    if(base.tag === "RawValue" && base.contents[1].startsWith("/operator")) { // Operator
      const op = base.contents[1].substring("/operator".length);

      if(args.length === 1) {
        return (<span>{op}<RawExpr expr={args[0].roaArr[0]}/></span>);
      } else {
        return (<span><RawExpr expr={args[0].roaArr[0]} /> {op} <RawExpr expr={args[1].roaArr[0]}/></span>);
      }

    }
    let showArgs = tagJoin(args.map((arg, argIndex) => {
      return <RawObjArr key={argIndex} roa={arg}/>;
    }), ", ");

    if (base.tag === "RawValue" && base.contents[1] === "") { // Anonymous tuple
      return (<span>({showArgs})</span>);
    }

    return (<span><RawExpr expr={base}/>({showArgs})</span>);
  case "RawVarsApply":
    const [vtupleM, vbase, vars] = expr.contents;

    let vfromVal = tryValue(vtupleM[2][1]);
    if (vfromVal) {
      return vfromVal;
    }
    let showVars = tagJoin(vars.map((vr, argIndex) => {
      if (isEmptyMeta(vr[1])) {
        return <RawExpr key={argIndex} expr={vr[0]}/>;
      } else {
        return <span key={argIndex}><RawExpr expr={vr[0]}/>: <RawMeta data={vr[1]} /></span>;
      }
    }), ", ");

    return (<span><RawExpr expr={vbase}/>[{showVars}]</span>);
  case "RawContextApply":
    const [,[,cxbase], cxargs] = expr.contents;
    const showCxargs = tagJoin(cxargs.map((arg, argIndex) => (<span key={argIndex}><PartialKey data={arg[0]}/>: <RawMeta data={arg[1]}/></span>)), ", ");
    return (<span><RawExpr expr={cxbase}/>&#123;{showCxargs}&#125;</span>);
  case "RawWhere":
    let [whBase, whCond] = expr.contents;
    return <span><RawExpr expr={whBase}/> | <RawExpr expr={whCond}/></span>;
  case "RawParen":
    return <span>(<RawExpr expr={expr.contents}/>)</span>;
  case "RawMethod":
    let [mBase, method] = expr.contents;
    return (<span><RawExpr expr={mBase}/>.<RawExpr expr={method} /></span>);
  case "RawList":
    let showList = tagJoin(expr.contents[1].map((arg, argIndex) => (<RawExpr key={argIndex} expr={arg} />)), ", ");
    return (<span>[{showList}]</span>);
  case "RawTypeProp":
    return (<span><RawExpr expr={expr.contents[1]}/><TypeProperty prop={expr.contents[2]}/></span>);
  default:
    console.error("Unknown renderRawExpr", expr);
    return "";
  }
}

function TypeProperty(props) {
  const {tag, contents} = props.prop;
  switch(tag) {
  case "TypePropProj":
    return <span>_{contents[0]}(<RawExpr expr={contents[1]}/>)</span>;
  case "TypePropRel":
    return <span>__{contents[0]}(<RawExpr expr={contents[1]}/>)</span>;
  default:
    console.error("Unknown TypeProperty", props);
    return "Prop";
  }
}

function isEmptyMeta(m) {
  let [tp,,] = m;
  return isTopType(tp);
}

function RawMeta(props) {
  let [tp,,] = props.data;
  return <Type data={tp} />;
}

export {
  RawExpr,
  RawObjArr,
}
