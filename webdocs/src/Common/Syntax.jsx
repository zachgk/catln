import React  from 'react';

import {tryValue} from './Value';
import {KeyWord, PTypeName, isTopType, Type, tagJoin} from './Common';

// The program as it was written, read by the names the compiler gives its parts.
//
// Every raw constructor names its fields (see the note above `TypeProperty` in
// `src/Syntax/Ct/Prgm.hs`: a Catln argument is named, so a type the compiler hands a Catln program
// has to say what its arguments are called), and those names are what aeson serializes. So a raw
// value arrives as `{tag, rvMeta, rvName}` rather than as `{tag, contents: [meta, name]}`, and
// nothing here reads `contents`.

function RawObjArr(props) {
  const {roaObj, roaArr, roaDef} = props.roa;
  const {updateExpr} = props;

  // Only the object is given updateExpr. It is the declared thing, and so the part of a declaration
  // that has a value to show and to edit; the arrow and the definition are how it is worked out.
  let showObj;
  if (roaObj) {
    showObj = <RawExpr expr={roaObj} updateExpr={updateExpr} />;
  }

  let showArr;
  if (roaArr) {
    // A Haskell pair, which is an array however its neighbours are serialized.
    const [roaArrExpr,roaArrMExpr] = roaArr;
    let showArrExpr;
    if (roaArrExpr) {
      showArrExpr = <RawExpr expr={roaArrExpr}/>;
    }

    let showArrM;
    if (roaArrMExpr) {
      showArrM = <span> -&gt; <RawExpr expr={roaArrMExpr} /></span>;
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

// The meta of a raw expression, whichever kind it is.
//
// The three that wrap another expression without carrying a meta of their own hand back the meta of
// what they wrap, which is the expression a value shown here would belong to.
function rawExprMeta(expr) {
  switch(expr.tag) {
  case "RawCExpr":        return expr.rceMeta;
  case "RawValue":        return expr.rvMeta;
  case "RawHoleExpr":     return expr.rheMeta;
  case "RawMacroValue":   return expr.rmvMeta;
  case "RawApplyExpr":    return expr.raeMeta;
  case "RawFmtStrExpr":   return expr.rfsMeta;
  case "RawTupleApply":   return expr.rtaMeta;
  case "RawVarsApply":    return expr.rvaMeta;
  case "RawContextApply": return expr.rcaMeta;
  case "RawWhere":        return expr.rwMeta;
  case "RawMethod":       return expr.rmMeta;
  case "RawList":         return expr.rlMeta;
  case "RawTypeProp":     return expr.rtpMeta;
  case "RawTheExpr":      return rawExprMeta(expr.rteExpr);
  case "RawAliasExpr":    return rawExprMeta(expr.raliasBase);
  case "RawParen":        return rawExprMeta(expr.rpnExpr);
  default:
    console.error("Unknown rawExprMeta", expr);
    return null;
  }
}

// What a constant holds, named per kind for the reason the expressions are.
function constantValue(c) {
  switch(c.tag) {
  case "CInt":   return c.ciValue;
  case "CFloat": return c.cfValue;
  case "CStr":   return c.csValue;
  case "CChar":  return c.chValue;
  default:
    console.error("Unknown Constant", c);
    return "";
  }
}

function RawExpr(props) {
  let {expr, updateExpr} = props;

  // updateExpr replaces whichever expression is named, so this is where it becomes the one-argument
  // update a value is given: this expression's id is what a value rendered here would be replacing.
  // It is not passed on to the nested RawExprs below, which are not rendered as values of their own.
  let m = rawExprMeta(expr);
  let update = updateExpr && (src => updateExpr(m.getMetaId, src));
  let fromVal = tryValue(m.getMetaDat[2], update);
  if (fromVal) {
    return fromVal;
  }

  switch(expr.tag) {
  case "RawCExpr":
    return "" + constantValue(expr.rceConst);
  case "RawValue":
    if (expr.rvName === "") {
      return "()";
    }
    return <PTypeName name={expr.rvName}/>;
  case "RawHoleExpr":
    return "_";
  case "RawMacroValue":
    return (<span>$ &#123;{expr.rmvName}&#125;</span>);
  case "RawFmtStrExpr":
    return (<span><PTypeName name={expr.rfsName}/>{JSON.stringify(expr.rfsStr)}</span>);
  case "RawApplyExpr": {
    // The terms an apply is written over, each marked by how far down it reaches.
    const showTerms = tagJoin(expr.raeApply.raTerms.map((term, i) => (
      <span key={i}>
        {term.tag === "RATermDeep" ? ".." : ""}
        <RawExpr expr={term.tag === "RATermDeep" ? term.ratdExpr : term.ratcExpr}/>
      </span>
    )), ", ");
    return (<span><KeyWord>apply</KeyWord>({showTerms})</span>);
  }
  case "RawTheExpr":
    return <span><KeyWord>:</KeyWord><RawExpr expr={expr.rteExpr}/></span>;
  case "RawAliasExpr":
    return <span><RawExpr expr={expr.raliasBase}/>@<RawExpr expr={expr.raliasAlias}/></span>;
  case "RawTupleApply": {
    const [, base] = expr.rtaBase;
    const args = expr.rtaArgs;

    if(base.tag === "RawValue" && base.rvName.startsWith("/operator")) { // Operator
      const op = base.rvName.substring("/operator".length);

      if(args.length === 1) {
        return (<span>{op}<RawExpr expr={args[0][1].roaArr[0]}/></span>);
      } else {
        return (<span><RawExpr expr={args[0][1].roaArr[0]} /> {op} <RawExpr expr={args[1][1].roaArr[0]}/></span>);
      }

    }
    const showArgs = tagJoin(args.map((arg, argIndex) => {
      if (arg[0]) {// isSpreadArg
        return <span key={argIndex}>..<RawObjArr roa={arg[1]}/></span>;
      } else {// is normal arg
        return <RawObjArr key={argIndex} roa={arg[1]}/>;
      }
    }), ", ");

    if (base.tag === "RawValue" && base.rvName === "") { // Anonymous tuple
      return (<span>({showArgs})</span>);
    }

    return (<span><RawExpr expr={base}/>({showArgs})</span>);
  }
  case "RawVarsApply": {
    const showVars = tagJoin(expr.rvaVars.map((vr, argIndex) => <RawObjArr key={argIndex} roa={vr} />), ", ");
    return (<span><RawExpr expr={expr.rvaBase}/>[{showVars}]</span>);
  }
  case "RawContextApply": {
    const [, cxbase] = expr.rcaBase;
    const showCxargs = tagJoin(expr.rcaArgs.map((arg, argIndex) => <RawObjArr key={argIndex} roa={arg}/>), ", ");
    return (<span><RawExpr expr={cxbase}/>&#123;{showCxargs}&#125;</span>);
  }
  case "RawWhere":
    return <span><RawExpr expr={expr.rwBase}/> | <RawExpr expr={expr.rwCond}/></span>;
  case "RawParen":
    return <span>(<RawExpr expr={expr.rpnExpr}/>)</span>;
  case "RawMethod":
    return (<span><RawExpr expr={expr.rmBase}/>.<RawExpr expr={expr.rmValue} /></span>);
  case "RawList": {
    const showList = tagJoin(expr.rlValues.map((arg, argIndex) => (<RawExpr key={argIndex} expr={arg} />)), ", ");
    return (<span>[{showList}]</span>);
  }
  case "RawTypeProp":
    return (<span><RawExpr expr={expr.rtpBase}/><TypeProperty prop={expr.rtpProp}/></span>);
  default:
    console.error("Unknown renderRawExpr", expr);
    return "";
  }
}

function TypeProperty(props) {
  const prop = props.prop;
  switch(prop.tag) {
  case "TypePropProj":
    return <span>_{prop.tppName}(<RawExpr expr={prop.tppExpr}/>)</span>;
  case "TypePropRel":
    return <span>__{prop.tprName}(<RawExpr expr={prop.tprExpr}/>)</span>;
  default:
    console.error("Unknown TypeProperty", props);
    return "Prop";
  }
}

// eslint-disable-next-line
function isEmptyMeta(m) {
  let {getMetaType} = m;
  return isTopType(getMetaType);
}

// eslint-disable-next-line
function RawMeta(props) {
  let {getMetaType} = props.data;
  return <Type data={getMetaType} />;
}

export {
  rawExprMeta,
  RawExpr,
  RawObjArr,
}
