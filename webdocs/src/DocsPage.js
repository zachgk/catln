import React, {useState, useContext} from 'react';

import Menu from '@material-ui/core/Menu';
import MenuItem from '@material-ui/core/MenuItem';
import PlayArrowIcon from '@material-ui/icons/PlayArrow';
import ReactMarkdown from 'react-markdown';
import {
  useParams,
  useHistory
} from 'react-router-dom';

import {useApi, posKey, Loading, KeyWord, TypeVar, PTypeName, PClassName, Obj, Guard, Type, Val, tagJoin} from './Common';

const useStyles = {
  indented: {
    marginLeft: '4em'
  },
  playIcon: {
    color: 'green'
  },
  noPlay: {
    marginLeft: '24px'
  }
};

const ResMaps = React.createContext({});

function DocsPage(props) {
  const {prgmName} = props;

  let apiResult = useApi(`/page?prgmName=${prgmName}`);

  return (
    <Loading status={apiResult}>
    <Main data={apiResult.data} />
    </Loading>
  );
}

function Main(props) {
  let [page, typed, annots] = props.data;

  let metaMap = {};
  let objMap = {};
  typed[0][0].forEach(tObjArrs => {
    const [obj, arrs] = tObjArrs;
    const objM = obj[0];
    const [metaType, metaPos] = objM;
    metaMap[posKey(metaPos)] = metaType;
    objMap[posKey(metaPos)] = obj;

    arrs.forEach(tArr => {
      const [, arrAnnots, , expr] = tArr;
      addExprToMetaMap(metaMap, expr);
      arrAnnots.forEach(arrAnnot => {
        addExprToMetaMap(metaMap, arrAnnot);
      });
    });
  });

  var annotsMap = {};
  annots.forEach(annot => {
    let pos = annot[0].contents[0][1];
    let val = annot[1];
    annotsMap[posKey(pos)] = val;
  });

  const statements = page[0][1];
  const resMaps = {metaMap, objMap, annotsMap};

  return (
    <ResMaps.Provider value={resMaps}>
      <Statements statements={statements} />
    </ResMaps.Provider>
  );
}

function Statements(props) {
  return props.statements.map((statement, index) => <Statement key={index} statement={statement} />);
}

function Statement(props) {
  let {statement} = props;
  let {annotsMap} = useContext(ResMaps);

  switch(statement.tag) {
  case "RawDeclStatement":
    return (
      <div>
        <PlayButton />
      <Decl contents={statement.contents} />
      </div>
    );
  case "MultiTypeDefStatement":
    let [className, classVars, classDatas] = statement.contents;

    let showClassVars;
    if(Object.keys(classVars).length > 0) {
      showClassVars = (
        <span>
          &lt;
          {tagJoin(Object.keys(classVars).map(v => <span key={v}><Type data={classVars[v]}/> v</span>), ", ")}
          &gt;
        </span>
      );
    }

    let showClassDatas = tagJoin(classDatas.map((d, dIndex) => <span key={dIndex}><Type data={d[0]}/></span>), " | ");

    return (<h3 style={useStyles.noPlay}><KeyWord>class</KeyWord> <PClassName name={className}/>{showClassVars} = {showClassDatas}</h3>);
  case "TypeDefStatement":
    return (<h3 style={useStyles.noPlay}><KeyWord>data</KeyWord> <Type data={statement.contents[0]}/></h3>);
  case "RawClassDefStatement":
    let [[instanceType, instanceVars], instanceClass] = statement.contents;

    let showInstanceVars;
    if(Object.keys(instanceVars).length > 0) {
      showInstanceVars = (
        <span>
          &lt;
          {tagJoin(Object.keys(instanceVars).map(v => <span key={v}><Type data={instanceVars[v]}/> <TypeVar>{v}</TypeVar></span>), ", ")}
          &gt;
        </span>
      );
    }

    return (<h3 style={useStyles.noPlay}><KeyWord>every</KeyWord> <PTypeName name={instanceType}/>{showInstanceVars} <KeyWord>isa</KeyWord> <PClassName name={instanceClass}/></h3>);
  case "RawClassDeclStatement":
    let [classDeclName, classDeclVars] = statement.contents;

    let showClassDeclVars;
    if(Object.keys(classDeclVars).length > 0) {
      showClassDeclVars = (
        <span>
          &lt;
        {tagJoin(Object.keys(classDeclVars).map(v => <span key={v}><Type data={classDeclVars[v]}/> <TypeVar>{v}</TypeVar></span>), ", ")}
          &gt;
        </span>
      );
    }


    return (<h3 style={useStyles.noPlay}><KeyWord>class</KeyWord> <PClassName name={classDeclName}/>{showClassDeclVars}</h3>);
  case "RawGlobalAnnot":
    let pos = statement.contents.contents[0][1];
    let val = annotsMap[posKey(pos)];

    let showAnnotExpr = <Expr style={useStyles.noPlay} expr={statement.contents} />;
    if(val.name === "#print") {
      return (
        <div style={useStyles.noPlay}>
          {showAnnotExpr}
          <br />
          <Val data={val.args.p} />
        </div>
      );
    } else {
      return showAnnotExpr;
    }
  case "RawComment":
    return (<div style={useStyles.noPlay}><ReactMarkdown children={statement.contents} /></div>);
  default:
    console.error("Unknown renderStatement", statement);
    return "";
  }
}

function Decl(props) {
  const {objMap} = useContext(ResMaps);

  const {contents} = props;
  const [lhs, subStatements, maybeExpr] = contents;
  const [, [obj, guard]] = lhs;
  const objM = obj[0];

  let showObj = obj;
  let typedObj = objMap[posKey(objM[1])];
  if(typedObj) {
    showObj = typedObj;
  }

  let showExpr;
  if(maybeExpr) {
    showExpr = (<span> = <Expr expr={maybeExpr}/></span>);
  }

  return (
    <span>
      <Obj obj={showObj} Meta={RawMeta}/><Guard guard={guard} Expr={Expr}/>{showExpr}
      <div style={useStyles.indented}>
      {subStatements.map((subStatement, index) => <DeclSubStatement key={index} subStatement={subStatement} />)}
      </div>
    </span>
  );
}

function DeclSubStatement(props) {
  const {subStatement} = props;

  switch(subStatement.tag) {
  case "RawDeclSubStatementDecl":
    return <div><Decl contents={subStatement.contents} /></div>;
  case "RawDeclSubStatementAnnot":
    return <div><Expr expr={subStatement.contents} /></div>;
  default:
    console.error("Unknown DeclSubStatement", subStatement);
    return "";
  }
}

function Expr(props) {
  let {metaMap} = useContext(ResMaps);
  let {expr} = props;
  switch(expr.tag) {
  case "RawCExpr":
    return "" + expr.contents[1].contents;
  case "RawValue":
    const [[, pos], val] = expr.contents;
    if(pos) {
      const tp = metaMap[posKey(pos)];
      if(tp && tp.tag === "TypeVar" && tp.contents.tag === "TVArg") {
        return <span>{val}</span>;
      }
    }
    return <PTypeName name={val}/>;
  case "RawTupleApply":
    const [, [,base], args] = expr.contents;

    let showArgs = tagJoin(args.map((arg, argIndex) => {
      switch(arg.tag) {
      case "RawTupleArgNamed":
        return (<span key={argIndex}>{arg.contents[0]} = <Expr expr={arg.contents[1]}/></span>);
      case "RawTupleArgInfer":
        return <Expr key={argIndex} expr={arg.contents}/>;
      default:
        console.error("Unknown renderExpr tupleApply arg type");
        return <span key={argIndex}></span>;
      }
    }), ", ");

    return (<span><Expr expr={base}/>({showArgs})</span>);
  case "RawParen":
    return <span>(<Expr expr={expr.contents}/>)</span>;
  case "RawMethods":
    let [mBase, methods] = expr.contents;
    let showMethods = methods.map((method, methodIndex) => <span key={methodIndex}>.<Expr expr={method}/></span>);
    return (<span><Expr expr={mBase}/>{showMethods}</span>);
  case "RawIfThenElse":
    let [, ifExpr, thenExpr, elseExpr] = expr.contents;
    return (<span><KeyWord>if</KeyWord> <Expr expr={ifExpr}/> <KeyWord>then</KeyWord> <Expr expr={thenExpr}/> <KeyWord>else</KeyWord> <Expr expr={elseExpr}/></span>);
  case "RawMatch":
    let [, matchExpr, matchPatterns] = expr.contents;
    let showMPatterns = tagJoin(matchPatterns.map((pattern, patternIndex) => {
      let [obj, guard] = pattern;
      return (<span key={patternIndex}><Obj obj={obj} Meta={RawMeta}/><Guard guard={guard} Expr={Expr}/></span>);
    }), "");
    return (<span><KeyWord>match</KeyWord> <Expr expr={matchExpr}/> <KeyWord>of</KeyWord> <div style={useStyles.indented}>{showMPatterns}</div></span>);
  case "RawCase":
    let [, caseExpr, casePatterns] = expr.contents;
    let showCPatterns = tagJoin(casePatterns.map((pattern, patternIndex) => {
      let [obj, guard] = pattern;
      return (<span key={patternIndex}><Obj obj={obj} Meta={RawMeta}/><Guard guard={guard} Expr={Expr}/></span>);
    }), "");
    return (<span><KeyWord>case</KeyWord> <Expr expr={caseExpr}/> <KeyWord>of</KeyWord> <div style={useStyles.indented}>{showCPatterns}</div></span>);
  default:
    console.error("Unknown renderExpr", expr);
    return "";
  }
}

function RawMeta(props) {
  let [tp, ] = props.data;
  return <Type data={tp} />;
}

function PlayButton() {
  let history = useHistory();
  const { curPage } = useParams();
  const [open, setOpen] = useState(false);
  const [anchorEl, setAnchorEl] = useState(null);

  let handleClick = (event) => {
    setOpen(!open);
    setAnchorEl(event.currentTarget);
  };

  let handleClose = (link) => () => {
    setOpen(false);
    history.push({pathname: link});
  };

  let button = <PlayArrowIcon fontSize='small' style={useStyles.playIcon} aria-controls="fade-menu" aria-haspopup="true" onClick={handleClick} />;

  return (
    <span>
      {button}
      <Menu
        anchorEl={anchorEl}
        keepMounted
        open={open}
        onClose={handleClose}
      >
        <MenuItem onClick={handleClose(`/build/${curPage}`)}>Build</MenuItem>
        <MenuItem onClick={handleClose(`/debug/${curPage}`)}>Debug</MenuItem>
        <MenuItem onClick={handleClose(`/constrain/${curPage}`)}>Constrain</MenuItem>
      </Menu>
    </span>
  );
}

function addExprToMetaMap(base, expr) {
  if(!expr) {
    return base;
  }

  switch(expr.tag) {
  case "CExpr":
  case "Value":
  case "Arg":
    return addToMetaMap(base, expr.contents[0]);
  case "TupleApply":
    const [m, [baseM ,baseExpr], , subExpr] = expr.contents;

    addToMetaMap(base, m);
    addToMetaMap(base, baseM);
    addExprToMetaMap(base, baseExpr);
    addExprToMetaMap(base, subExpr);

    return base;
  default:
    console.error("Unknown expr", expr);
    return {};
  }
}

function addToMetaMap(base, m) {
  const [tp, pos] = m;
  if(pos) {
    base[posKey(pos)] = tp;
  }
  return base;
}


export default DocsPage;