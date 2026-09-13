import React  from 'react';

import {PartialKey, PartialType, PTypeName, isTopType, Type, KeyWord} from './Common';

// An expression's parts, in the positional order this file is written against.
//
// Two kinds arrive here and they are serialized differently. The desugared `Expr` names its fields
// (`veName`, `taBase`, ...), because a type the compiler hands a Catln program has to say what its
// arguments are called; the built `TExpr` is an ordinary positional constructor and arrives as
// `contents`. A debugger node holds one of each -- the arrow as the program wrote it, or the one
// built for the call -- so both are read rather than either being assumed.
function exprContents(expr) {
  switch (expr.tag) {
  case "CExpr":       return [expr.ceMeta, expr.ceConst];
  case "Value":       return [expr.veMeta, expr.veName];
  case "HoleExpr":    return [expr.heMeta, expr.heHole];
  case "AliasExpr":   return [expr.aeBase, expr.aeAlias];
  case "EWhere":      return [expr.ewMeta, expr.ewBase, expr.ewCond];
  case "TupleApply":  return [expr.taMeta, expr.taBase, expr.taApp];
  default:            return expr.contents;
  }
}

// The same for what is applied to an expression.
function appContents(app) {
  switch (app.tag) {
  case "EAppArg":    return app.eaaArg;
  case "EAppVar":    return [app.eavName, app.eavMeta];
  case "EAppSpread": return app.easExpr;
  default:           return app.contents;
  }
}

// What a constant holds, named per kind for the reason above.
function constantValue(c) {
  switch (c.tag) {
  case "CInt":   return c.ciValue;
  case "CFloat": return c.cfValue;
  case "CStr":   return c.csValue;
  case "CChar":  return c.chValue;
  default:       return c.contents;
  }
}

function Expr(props) {
  let {expr, Meta, showMetas} = props;
  const contents = exprContents(expr);
  if (contents === undefined) {
    console.error("Unknown renderExpr", expr);
    return "";
  }
  switch(expr.tag) {
  case "CExpr":
  case "TCExpr":
    return "" + constantValue(contents[1]);
  case "Value":
  case "TValue":
    let showValM;
    if (showMetas) {
      showValM = <Meta data={contents[0]}/>;
    }
    if (contents[1] === "") {
      return <span>(){showValM}</span>;
    }
    return <span><PTypeName name={contents[1]} />{showValM}</span>;
  case "HoleExpr":
  case "THoleExpr":
    let showHoleM;
    if (showMetas) {
      showHoleM = <Meta data={contents[0]}/>;
    }
    return <span>_{showHoleM}</span>;
  case "AliasExpr":
  case "TAliasExpr":
    return <span><Expr expr={contents[0]} Meta={Meta} showMetas={showMetas}/>@<Expr expr={contents[1]} Meta={Meta} showMetas={showMetas} /></span>;
  case "EWhere":
  case "TWhere":
    let [,whBase, whCond] = contents;
    return <span><Expr expr={whBase} Meta={Meta} showMetas={showMetas}/> | <Expr expr={whCond} Meta={Meta} showMetas={showMetas}/></span>;
  case "TupleApply":
  case "TTupleApply":
    const [m, [baseM ,base], arg] = contents;

    let showBaseM;
    if(showMetas) {
      showBaseM = <i><Meta data={baseM}/></i>;
    }

    let showM;
    if(showMetas) {
      showM = <i><Meta data={m}/></i>;
    }

    let showBase = <Expr expr={base} Meta={Meta} showMetas={showMetas}/>;
    let showArg;
    switch(arg.tag) {
    case "EAppArg":
      showArg = <span>(<ObjArr oa={appContents(arg)} Meta={Meta} showExprMetas={showMetas} />)</span>;
      break;
    case "EAppVar":
      if (isTopType(appContents(arg)[1].getMetaType)) {
        showArg = <span>[<PartialKey data={appContents(arg)[0]} />]</span>;
      } else {
        showArg = <span>[<PartialKey data={appContents(arg)[0]} />: <Type data={appContents(arg)[1].getMetaType} />]</span>;
      }
      break;
    case "EAppSpread":
      showArg = <span>(..<Expr expr={appContents(arg)} Meta={Meta} showMetas={showMetas}/>)</span>;
      break;
    default:
      console.error("Unknown tupleApply arg", arg);
    }

    if ((base.tag === "Value" || base.tag === "TValue") && exprContents(base)[1] === "") { // Anonymous tuple
      return <span>{showArg}{showM}</span>;
    }

    return <span>{showBase}{showBaseM}{showArg}{showM}</span>;
  case "TCalls":
    const [,callE,callTree] = contents;
    return <span><Expr expr={callE} Meta={Meta} showMetas={showMetas}/>↦<TCallTree tree={callTree} /></span>;
  default:
    console.error("Unknown renderExpr", expr);
    return "";
  }
}

function ObjArr(props) {
  const {Meta, showExprMetas, multiLine, oa} = props;
  const {oaObj, oaArr, oaAnnots} = oa;

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
    if (!isTopType(oaArrM.getMetaType)) {
      showArrType = <span> -&gt; <Type data={oaArrM.getMetaType} /></span>;
    }
    let showArrM;
    if (showExprMetas) {
      showArrM = <Meta data={oaArrM}/>;
    }
    showArr = <span>{showArrType}{showArrM}{showArrExpr}</span>;
  }

  let showAnnots;
  if (multiLine) {
    showAnnots = oaAnnots.map((annot, index) => <div key={index}>&nbsp;&nbsp;&nbsp;&nbsp;<Expr expr={annot} Meta={Meta} showMetas={showExprMetas}/></div>);
  }

  return <span>{showObj}{showArr}{showAnnots}</span>;
}

function TCallTree(props) {
  const {tree} = props;

  switch(tree.tag) {
  case "TCTId":
    return "TCTId";
  case "TCMatch":
    // TCMatch carries its destination Type first, then the per-partial trees.
    return (
      <table>
        <tbody>
          { tree.contents[1].map((mt, index) =>
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
          { tree.contents[1].map((mt, index) => {
            if (mt[0]) {
              return (
                <tr key={index}>
                  <td><KeyWord>if</KeyWord> <Expr expr={mt[0][1].oaObj}/></td>
                  <td><KeyWord>then</KeyWord> <TCallTree tree={mt[1]}/></td>
                </tr>
              );
            } else {
              return (
                <tr key={index}>
                  <td></td>
                  <td><TCallTree tree={mt[1]}/></td>
                </tr>
              );
            }
          }
          )}
        </tbody>
      </table>
    );
  case "TCArg":
    return "TCArg";
  case "TCObjArr":
    return <span>TCObjArr <Type data={tree.contents.oaArr[1].getMetaType}/></span>;
  case "TCPrim":
    return <span>TCPrim <Type data={tree.contents[0].oaArr[1].getMetaType}/></span>;
  case "TCMacro":
    return <span>TCMacro <Type data={tree.contents[0].oaArr[1].getMetaType}/></span>;
  default:
    console.error("Unknown TCallTree", tree);
    return "";
  }
}

export {
  exprContents,
  Expr,
  ObjArr,
}
