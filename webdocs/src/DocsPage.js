import React, {useState, useContext} from 'react';

import { makeStyles } from '@material-ui/core/styles';
import Menu from '@material-ui/core/Menu';
import MenuItem from '@material-ui/core/MenuItem';
import PlayArrowIcon from '@material-ui/icons/PlayArrow';
import ReactMarkdown, {uriTransformer} from 'react-markdown';
import {useHistory} from 'react-router-dom';

import {useApi, posKey, Loading, KeyWord, TypeVar, PTypeName, PClassName, Obj, Guard, Type, Val, tagJoin} from './Common';

const useStyles = makeStyles({
  indented: {
    marginLeft: '4em'
  },
  playIcon: {
    color: 'green',
    cursor: 'pointer'
  },
  noPlay: {
    marginLeft: '24px'
  }
});

const ResMaps = React.createContext({});

function DocsPage(props) {
  const {prgmName} = props;

  let apiResult = useApi(`/api/page?prgmName=${prgmName}`);

  return (
    <Loading status={apiResult}>
      <Main data={apiResult.data} prgmName={prgmName}/>
    </Loading>
  );
}

function Main(props) {
  const {prgmName} = props;
  let [page, typed, annots] = props.data;

  const classToType = typed[0][1][1];

  let metaMap = {};
  let objMap = {};
  let objNames = {};
  typed[0][0].forEach(tObjArrs => {
    const [obj, arrs] = tObjArrs;
    const {objM, objName} = obj;
    const [metaType, metaPos] = objM;
    metaMap[posKey(metaPos)] = metaType;
    objMap[posKey(metaPos)] = obj;

    // Need to check for functions because "toString" is already defined
    if(!(objName in objNames) || typeof(objNames[objName]) === "function") {
      objNames[objName] = [];
    }
    objNames[objName].push(obj);

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
  const resMaps = {metaMap, objMap, objNames, classToType, annotsMap, prgmName};

  return (
    <ResMaps.Provider value={resMaps}>
      {page[0][0].map((imp, ind) => <Import key={ind} name={imp}/>)}
      <br/>
      <Statements statements={statements} />
    </ResMaps.Provider>
  );
}

function Import(props) {
  const {name} = props;
  const classes = useStyles();
  return (
    <div className={classes.noPlay}><KeyWord>import</KeyWord> {name}</div>
  );
}

function Statements(props) {
  return props.statements.map((statement, index) => <Statement key={index} statement={statement} />);
}

function Statement(props) {
  let {statement} = props;
  let {annotsMap} = useContext(ResMaps);
  const classes = useStyles();

  switch(statement.tag) {
  case "RawDeclStatement":
    const objName = statement.contents[0][1][0].objName;
    if(objName === "main" || objName === "mainx") {
      return (
        <div>
          <PlayButton />
          <Decl contents={statement.contents} />
        </div>
      );
    } else {
      return (
        <div className={classes.noPlay}>
          <Decl contents={statement.contents} />
        </div>
      );
    }
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

    return (<h3 className={classes.noPlay} id={`defClass${className}`}><KeyWord>class</KeyWord> <PClassName name={className}/>{showClassVars} = {showClassDatas}</h3>);
  case "TypeDefStatement":
    return (<h3 className={classes.noPlay}><KeyWord>data</KeyWord> <Type data={statement.contents[0]}/></h3>);
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

    return (<h3 className={classes.noPlay}><KeyWord>every</KeyWord> <PTypeName name={instanceType}/>{showInstanceVars} <KeyWord>isa</KeyWord> <PClassName name={instanceClass}/></h3>);
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


    return (<h3 className={classes.noPlay} id={`defClass${classDeclName}`}><KeyWord>class</KeyWord> <PClassName name={classDeclName}/>{showClassDeclVars}</h3>);
  case "RawGlobalAnnot":
    const [annot, annotSubStatements] = statement.contents;
    let pos = annot.contents[0][1];
    let val = annotsMap[posKey(pos)];

    let showAnnotExpr = <Expr expr={annot} />;
    let showAnnot;
    if(val && val.name === "#print") {
      showAnnot = (
        <div>
          {showAnnotExpr}
          <br />
          <Val data={val.args.p} />
        </div>
      );
    } else {
      showAnnot = showAnnotExpr;
    }
    return (
      <div className={classes.noPlay}>
        {showAnnot}
        <div><Statements statements={annotSubStatements}/></div>
      </div>
    );
  case "RawModule":
    return (
      <div className={classes.noPlay}>
        <KeyWord>module</KeyWord> {statement.contents[0]}
        <div><Statements statements={statement.contents[1]}/></div>
      </div>
    );
  case "RawComment":
    return (<div className={classes.noPlay}><Comment comment={statement.contents} /></div>);
  default:
    console.error("Unknown renderStatement", statement);
    return "";
  }
}

function Decl(props) {
  const {objMap} = useContext(ResMaps);
  const classes = useStyles();

  const {contents} = props;
  const [lhs, subStatements, maybeExpr] = contents;
  const [, [obj, guard]] = lhs;

  let showObj = obj;
  let typedObj = objMap[posKey(obj.objM[1])];
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
      <div className={classes.indented}>
        {subStatements.map((subStatement, index) => <DeclSubStatement key={index} obj={showObj} subStatement={subStatement} />)}
      </div>
    </span>
  );
}

function DeclSubStatement(props) {
  const {subStatement, obj} = props;

  switch(subStatement.tag) {
  case "RawDeclSubStatementDecl":
    return <div><Decl contents={subStatement.contents} /></div>;
  case "RawDeclSubStatementAnnot":
    return <div><Expr expr={subStatement.contents} /></div>;
  case "RawDeclSubStatementComment":
    return <div><Comment comment={subStatement.contents} obj={obj} /></div>;
  default:
    console.error("Unknown DeclSubStatement", subStatement);
    return "";
  }
}

function Expr(props) {
  let {metaMap} = useContext(ResMaps);
  const classes = useStyles();
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

    if(base.tag === "RawValue" && base.contents[1].startsWith("operator")) {
      const op = base.contents[1].substring("operator".length);

      if(args.length === 1) {
        return (<span>{op}<Expr expr={args[0].contents[1]}/></span>);
      } else {
        return (<span><Expr expr={args[0].contents[1]} /> {op} <Expr expr={args[1].contents[1]}/></span>);
      }

    } else {
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
    }

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
    return (<span><KeyWord>match</KeyWord> <Expr expr={matchExpr}/> <KeyWord>of</KeyWord> <div className={classes.indented}>{showMPatterns}</div></span>);
  case "RawCase":
    let [, caseExpr, casePatterns] = expr.contents;
    let showCPatterns = tagJoin(casePatterns.map((pattern, patternIndex) => {
      let [obj, guard] = pattern;
      return (<span key={patternIndex}><Obj obj={obj} Meta={RawMeta}/><Guard guard={guard} Expr={Expr}/></span>);
    }), "");
    return (<span><KeyWord>case</KeyWord> <Expr expr={caseExpr}/> <KeyWord>of</KeyWord> <div className={classes.indented}>{showCPatterns}</div></span>);
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
  let {prgmName} = useContext(ResMaps);
  const [open, setOpen] = useState(false);
  const [anchorEl, setAnchorEl] = useState(null);
  const classes = useStyles();

  let handleClick = (event) => {
    setOpen(!open);
    setAnchorEl(event.currentTarget);
  };

  let handleClose = () => {
    setOpen(false);
  };

  let linkClose = (link) => () => {
    setOpen(false);
    history.push({pathname: link});
  };

  let button = <PlayArrowIcon fontSize='small' className={classes.playIcon} aria-controls="fade-menu" aria-haspopup="true" onClick={handleClick} />;

  return (
    <span>
      {button}
      <Menu
        anchorEl={anchorEl}
        keepMounted
        open={open}
        onClose={handleClose}
      >
        <MenuItem onClick={linkClose(`/build/${prgmName}`)}>Build</MenuItem>
        <MenuItem onClick={linkClose(`/debug/${prgmName}`)}>Debug</MenuItem>
      </Menu>
    </span>
  );
}

export function Comment(props) {
  const {comment, obj} = props;
  let {objNames, classToType} = useContext(ResMaps);

  // Replace usages of [TypeName] and [ClassName] with catn:// link reference
  const regex = /\[(\S+)\][^[(]/g;
  const comment2 = comment.replaceAll(regex, (m, name) => {
    if(name in classToType) {
      return `[${name}](catln://class/${name})${m.slice(-1)}`;
    } else if(name in objNames) {
      return `[${name}](catln://type/${name})${m.slice(-1)}`;
    } else if(obj && name in obj.objArgs){
      return `[${name}](catln://arg/${name})${m.slice(-1)}`;
    } else if(obj && name in obj.objVars){
      return `[${name}](catln://typevar/${name})${m.slice(-1)}`;
    } else {
      return m;
    }
  });

  const components = {
    a: ({href, children}) => {
      if(href.startsWith("catln://class/")) {
        const className = href.substring("catln://class/".length);
        return <PClassName name={className}/>;
      } else if(href.startsWith("catln://type/")) {
        const typeName = href.substring("catln://type/".length);
        return <PTypeName name={typeName}/>;
      } else if(href.startsWith("catln://arg/")) {
        const argName = href.substring("catln://arg/".length);
        return <KeyWord>{argName}</KeyWord>;
      } else if(href.startsWith("catln://typevar/")) {
          const typeVarName = href.substring("catln://typevar/".length);
          return <KeyWord>{typeVarName}</KeyWord>;
      } else {
        return <a href={href}>{children}</a>;
      }
    }
  };

  function transformLinkUri(href, children, title) {
    if(href.startsWith("catln://")) {
      return href;
    } else {
      return uriTransformer(href, children, title);
    }
  }

  return <ReactMarkdown children={comment2} transformLinkUri={transformLinkUri} components={components}/>;
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
