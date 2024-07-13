import React  from 'react';

import {PartialKey, PartialType, PTypeName, isTopType, Type, KeyWord} from './Common';

function Expr(props) {
  let {expr, Meta, showMetas} = props;
  switch(expr.tag) {
  case "CExpr":
  case "TCExpr":
    return "" + expr.contents[1].contents;
  case "Value":
  case "TValue":
    let showValM;
    if (showMetas) {
      showValM = <Meta data={expr.contents[0]}/>;
    }
    if (expr.contents[1] === "") {
      return <span>(){showValM}</span>;
    }
    return <span><PTypeName name={expr.contents[1]} />{showValM}</span>;
  case "HoleExpr":
  case "THoleExpr":
    let showHoleM;
    if (showMetas) {
      showHoleM = <Meta data={expr.contents[0]}/>;
    }
    return <span>_{showHoleM}</span>;
  case "AliasExpr":
  case "TAliasExpr":
    return <span><Expr expr={expr.contents[0]} Meta={Meta} showMetas={showMetas}/>@<Expr expr={expr.contents[1]} Meta={Meta} showMetas={showMetas} /></span>;
  case "EWhere":
  case "TWhere":
    let [whBase, whCond] = expr.contents;
    return <span><Expr expr={whBase} Meta={Meta} showMetas={showMetas}/> | <Expr expr={whCond} Meta={Meta} showMetas={showMetas}/></span>;
  case "TupleApply":
  case "TTupleApply":
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
    let showArg = <span>(<ObjArr oa={arg} Meta={Meta} showExprMetas={showMetas} />)</span>;

    if ((base.tag === "Value" || base.tag === "TValue") && base.contents[1] === "") { // Anonymous tuple
      return <span>{showArg}{showM}</span>;
    }

    return <span>{showBase}{showBaseM}{showArg}{showM}</span>;
  case "VarApply":
  case "TVarApply":
    const [vm, vbase, varName, varVal] = expr.contents;

    let showvM;
    if(showMetas) {
      showM = <i>[<Meta data={vm}/>]</i>;
    }

    let showvBase = <Expr expr={vbase} Meta={Meta} showMetas={showMetas}/>;

    if (isTopType(varVal[0])) {
      return <span>{showvBase}[<PartialKey data={varName} />]{showvM}</span>;
    } else {
      return <span>{showvBase}[<PartialKey data={varName} />: <Type data={varVal[0]} />]{showvM}</span>;
    }
  case "TCalls":
    const [,callE,callTree] = expr.contents;
    return <span><Expr expr={callE} Meta={Meta} showMetas={showMetas}/>↦<TCallTree tree={callTree} /></span>;
  default:
    console.error("Unknown renderExpr", expr);
    return "";
  }
}

function ObjArr(props) {
  const {Meta, showExprMetas, multiLine, oa} = props;
  const {oaObj, oaArr} = oa;

  let showObj;
  if (oaObj) {
    showObj = <Expr expr={oaObj} Meta={Meta} showMetas={showExprMetas} />;
  }

  let showArr;
  if (oaArr) {
    const [oaArrExpr, oaArrM] = oaArr;
    let showArrExpr;
    if (oaArrExpr) {
      let showMultiLine = multiLine ? <span><br />&nbsp;&nbsp;&nbsp;&nbsp;</span> : "";
      showArrExpr = <span> ={showMultiLine} <Expr expr={oaArrExpr} Meta={Meta} showMetas={showExprMetas}/></span>;
    }

    let showArrType;
    if (!isTopType(oaArrM[0])) {
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

function TCallTree(props) {
  const {tree} = props;

  switch(tree.tag) {
  case "TCTId":
    return "TCTId";
  case "TCMatch":
    return (
      <table>
        <tbody>
          { tree.contents.map((mt, index) =>
            <tr key={index}>
              <td><PartialType data={mt[0]}/></td>
              <td><TCallTree tree={mt[1]}/></td>
            </tr>
          )}
        </tbody>
      </table>
    );
  case "TCSeq":
    return <span><TCallTree tree={tree.contents[0]}/>↦<TCallTree tree={tree.contents[1]}/></span>;
  case "TCCond":
    return (
      <table>
        <tbody>
          { tree.contents[1].map((mt, index) =>
            <tr key={index}>
              <td><KeyWord>if</KeyWord> <Expr expr={mt[0][0]}/></td>
              <td><TCallTree tree={mt[1]}/></td>
            </tr>
          )}
          <tr>
            <td><KeyWord>else</KeyWord></td>
            <td><TCallTree tree={tree.contents[2]}/></td>
          </tr>
        </tbody>
      </table>
    );
  case "TCArg":
    return "TCArg";
  case "TCObjArr":
    return <span>TCObjArr <Type data={tree.contents.oaArr[1][0]}/></span>;
  case "TCPrim":
    return <span>TCPrim <Type data={tree.contents[0]}/></span>;
  case "TCMacro":
    return <span>TCMacro <Type data={tree.contents[0]}/></span>;
  default:
    console.error("Unknown TCallTree", tree);
    return "";
  }
}

export {
  Expr,
  ObjArr,
}
