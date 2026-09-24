import React, {createContext, useContext} from 'react';

import Box from '@mui/material/Box';
import Tooltip from '@mui/material/Tooltip';

import {PartialKey, PartialType, PTypeName, isTopType, Type, KeyWord} from './Common';

// Whether the types an expression carries are written into it or kept for a hover.
//
// Written in is the default because a program listing is read for its declarations, and a type is
// half of what a declaration says. A debugger reads the other way round: what it is for is which call
// was made on which value, and every argument of every call spelling its type out -- the declared
// one, often a union several lines long -- buried the call it belonged to. There the type is one
// hover away, on the name it is the type of.
const TypesOnHoverContext = createContext(false);

// A name whose type is shown when it is hovered, rather than beside it.
//
// The hover sits on the name alone and never on a whole expression, so that no two of them are ever
// nested: an argument inside a call is its own name with its own type, and hovering it has to show
// that type rather than it and the call's together.
function TypeTip(props) {
  const {type, children} = props;
  return (
    <Tooltip
      title={<span className="catln-type-tip"><Type data={type} /></span>}
      placement="bottom-start"
      slotProps={{tooltip: {sx: tipSx}}}
    >
      <Box component="span" className="catln-typed" sx={typedSx}>{children}</Box>
    </Tooltip>
  );
}

// A type in the tip is read as it would be in a listing -- it holds links to the types it names, which
// a tip is kept open to follow -- so it takes the page's colours and type rather than a tooltip's.
const tipSx = theme => ({
  backgroundColor: theme.palette.background.paper,
  color: theme.palette.text.primary,
  border: `1px solid ${theme.palette.divider}`,
  boxShadow: theme.shadows[2],
  fontSize: theme.typography.body2.fontSize,
  maxWidth: '40em',
});

// Something to find the tips by without hovering everything: a name with one is underlined dotted.
const typedSx = {
  textDecoration: 'underline dotted',
  textUnderlineOffset: '0.2em',
  cursor: 'help',
  '&:hover': {backgroundColor: 'action.hover'},
};

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

// headTip is the type of whatever this expression is the object of, when that type is to be hovered
// rather than written: it is put on the name at the head of the expression, down through whatever is
// applied to it or written around it, since that name is what the reader points at to ask.
function Expr(props) {
  let {expr, Meta, showMetas, headTip} = props;
  const hoverTypes = useContext(TypesOnHoverContext);
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
      return <span>{tipped(headTip, "()")}{showValM}</span>;
    }
    return <span>{tipped(headTip, <PTypeName name={contents[1]} />)}{showValM}</span>;
  case "HoleExpr":
  case "THoleExpr":
    let showHoleM;
    if (showMetas) {
      showHoleM = <Meta data={contents[0]}/>;
    }
    return <span>_{showHoleM}</span>;
  case "AliasExpr":
  case "TAliasExpr":
    return <span><Expr expr={contents[0]} Meta={Meta} showMetas={showMetas} headTip={headTip}/>@<Expr expr={contents[1]} Meta={Meta} showMetas={showMetas} /></span>;
  case "EWhere":
  case "TWhere":
    let [,whBase, whCond] = contents;
    return <span><Expr expr={whBase} Meta={Meta} showMetas={showMetas} headTip={headTip}/> | <Expr expr={whCond} Meta={Meta} showMetas={showMetas}/></span>;
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

    // An anonymous tuple has no name at its head to hover, so its type is on the paren that opens it.
    const anonymous = (base.tag === "Value" || base.tag === "TValue") && exprContents(base)[1] === "";

    let showBase = <Expr expr={base} Meta={Meta} showMetas={showMetas} headTip={headTip}/>;
    let showArg;
    switch(arg.tag) {
    case "EAppArg":
      showArg = <span>{anonymous ? tipped(headTip, "(") : "("}<ObjArr oa={appContents(arg)} Meta={Meta} showExprMetas={showMetas} />)</span>;
      break;
    case "EAppVar":
      const varType = appContents(arg)[1].getMetaType;
      if (isTopType(varType) || hoverTypes) {
        showArg = <span>[{tipped(isTopType(varType) ? null : varType, <PartialKey data={appContents(arg)[0]} />)}]</span>;
      } else {
        showArg = <span>[<PartialKey data={appContents(arg)[0]} />: <Type data={varType} />]</span>;
      }
      break;
    case "EAppSpread":
      showArg = <span>{anonymous ? tipped(headTip, "(") : "("}..<Expr expr={appContents(arg)} Meta={Meta} showMetas={showMetas}/>)</span>;
      break;
    default:
      console.error("Unknown tupleApply arg", arg);
    }

    if (anonymous) {
      return <span>{showArg}{showM}</span>;
    }

    return <span>{showBase}{showBaseM}{showArg}{showM}</span>;
  case "TCalls":
    const [,callE,callTree] = contents;
    return <span><Expr expr={callE} Meta={Meta} showMetas={showMetas} headTip={headTip}/>↦<TCallTree tree={callTree} /></span>;
  default:
    console.error("Unknown renderExpr", expr);
    return "";
  }
}

function ObjArr(props) {
  const {Meta, showExprMetas, multiLine, oa} = props;
  const {oaObj, oaArr, oaAnnots} = oa;
  const hoverTypes = useContext(TypesOnHoverContext);

  // The type this ObjArr declares, when there is one worth saying and it is hovered rather than
  // written. It goes on the object's head, or on the head of the arrow's expression when there is no
  // object to put it on.
  const arrType = oaArr && !isTopType(oaArr[1].getMetaType) ? oaArr[1].getMetaType : null;
  const tip = hoverTypes ? arrType : null;

  let showObj;
  if (oaObj) {
    showObj = <Expr expr={oaObj} Meta={Meta} showMetas={showExprMetas} headTip={tip} />;
  }

  let showArr;
  if (oaArr) {
    const [oaArrExpr, oaArrM] = oaArr;
    let showArrExpr;
    if (oaArrExpr) {
      let showMultiLine = multiLine ? <span><br />&nbsp;&nbsp;&nbsp;&nbsp;</span> : "";
      showArrExpr = <span> ={showMultiLine} <Expr expr={oaArrExpr} Meta={Meta} showMetas={showExprMetas} headTip={oaObj ? null : tip}/></span>;
    }

    let showArrType;
    if (arrType && !hoverTypes) {
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

// The name wrapped in a hover of its type, or left as it is when there is none to show.
function tipped(type, children) {
  return type ? <TypeTip type={type}>{children}</TypeTip> : children;
}

export {
  exprContents,
  Expr,
  ObjArr,
  TypesOnHoverContext,
}
