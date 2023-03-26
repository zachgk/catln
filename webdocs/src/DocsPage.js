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
  let [page, annots] = props.data;

  const classToType = typed[0][1][1];

  let metaMap = {};
  let objMap = {};
  let objNames = {};
  typed[0].forEach(tObjArrs => {
    const [obj, annots, arr] = tObjArrs;
    const {deprecatedObjM, deprecatedObjPath} = obj;
    const [metaType, metaPos] = deprecatedObjM;

    if (metaPos) {
      metaMap[posKey(metaPos)] = metaType;
      objMap[posKey(metaPos)] = obj;
    }

    // Need to check for functions because "toString" is already defined
    if(!(deprecatedObjPath in objNames) || typeof(objNames[deprecatedObjPath]) === "function") {
      objNames[deprecatedObjPath] = [];
    }
    objNames[deprecatedObjPath].push(obj);

    if (arr) {
      const [, , arrExpr] = arr;
      addExprToMetaMap(metaMap, arrExpr);
      annots.forEach(annot => {
        addExprToMetaMap(metaMap, annot);
      });
    }
  });

  var annotsMap = {};
  annots.forEach(annot => {
    let pos = annot[0].contents[0][1];
    let val = annot[1];
    annotsMap[posKey(pos)] = val;
  });

  const statements = page[1];
  const resMaps = {metaMap, objMap, objNames, classToType, annotsMap, prgmName};

  return (
    <ResMaps.Provider value={resMaps}>
      {page[0].map((imp, ind) => <Import key={ind} name={imp}/>)}
      <br/>
      <Statements statements={statements} root={true} />
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
  return props.statements.map((statement, index) => <StatementTree key={index} statementTree={statement} root={props.root} />);
}

function StatementTree(props) {
  const {statementTree} = props;
  const [statement, subStatements] = statementTree;
  const classes = useStyles();

  return (
    <div>
      <Statement statement={statement}/>
      <div className={classes.indented}>
        {subStatements.map((subStatement, index) => <StatementTree key={index} statementTree={subStatement} />)}
      </div>
    </div>
  );
}

function Statement(props) {
  const {statement} = props;
  const classes = useStyles();

  switch(statement.tag) {
  case "RawDeclStatement":
    const obj = statement.contents[0][1][0];
    let objName = obj.deprecatedObjPath;
    const noArgObj = Object.keys(obj.deprecatedObjArgs).length === 0;
    let contextObj = false;
    if(objName === "/Catln/Context" && Object.keys(obj.deprecatedObjArgs['value'][1].objArgs).length === 0) {
      contextObj = true;
      objName = obj.deprecatedObjArgs['value'][1].objName;
    }
    if(noArgObj || contextObj) {
      return (
        <div>
          <PlayButton fun={objName}/>
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
    let [multiTypeDecl] = statement.contents;
    let [className, classVars, classDatas] = multiTypeDecl;

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

    return <h3 className={classes.noPlay} id={`defClass${className}`}><KeyWord>class</KeyWord> <PClassName name={className}/>{showClassVars} = {showClassDatas}</h3>;
  case "TypeDefStatement":
    let typeDef = statement.contents;
    return <h3 className={classes.noPlay}><KeyWord>data</KeyWord> <Type data={typeDef[0]}/></h3>;
  case "RawClassDefStatement":
    let classDef = statement.contents;
    let [[instanceType, instanceVars], instanceClass] = classDef;

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

    return <h3 className={classes.noPlay}><KeyWord>every</KeyWord> <PTypeName name={instanceType}/>{showInstanceVars} <KeyWord>isa</KeyWord> <PClassName name={instanceClass}/></h3>;
  case "RawClassDeclStatement":
    let classDecl = statement.contents;
    let [classDeclName, classDeclVars] = classDecl;

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


    return <h3 className={classes.noPlay} id={`defClass${classDeclName}`}> <KeyWord>class</KeyWord> <PClassName name={classDeclName}/>{showClassDeclVars} </h3>;
  case "RawAnnot":
    const annot = statement.contents;

    return (
      <div className={classes.noPlay}>
        <Annot annot={annot} />
      </div>
    );
  case "RawModule":
    return (
      <div className={classes.noPlay}>
        <KeyWord>module</KeyWord> {statement.contents[0]}
      </div>
    );
  default:
    console.error("Unknown renderStatement", statement);
    return "";
  }
}

function Decl(props) {
  const {objMap} = useContext(ResMaps);

  const {contents} = props;
  const [lhs, maybeExpr] = contents;
  const [, [obj, guard]] = lhs;

  let showObj = obj;
  let typedObj = objMap[posKey(obj.deprecatedObjM[1])];
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
    </span>
  );
}

function Annot(props) {
  const {annot, obj} = props;
  let {annotsMap} = useContext(ResMaps);

  let pos = annot.contents[0][1];
  let val = annotsMap[posKey(pos)];

  let showAnnotExpr = <Expr expr={annot} obj={obj} />;
  if(val && val.name === "#print") {
    return (
      <div>
        {showAnnotExpr}
        <br />
        <Val data={val.args.p} />
      </div>
    );
  }

  if(val && val.name === "/Catln/#md") {
    return <Comment comment={val.args.text.contents} />;
  }

  if (annot.tag === "RawTupleApply") {
    if (annot.contents[1][1].tag === "RawValue") {
      if (annot.contents[1][1].contents[1] === "/Catln/#md") {
        return <Comment comment={annot.contents[2][0].contents[2].contents[1].contents} />;
      }
    }
  }

  return showAnnotExpr;
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
        case "TupleArgIO":
          return (<span key={argIndex}>{arg.contents[1]} = <Expr expr={arg.contents[2]}/></span>);
        case "TupleArgO":
          return <Expr key={argIndex} expr={arg.contents[1]}/>;
        case "TupleArgI":
          return <span>{arg.contents[1]}</span>;
        default:
          console.error("Unknown renderExpr tupleApply arg type");
          return <span key={argIndex}></span>;
        }
      }), ", ");

      return (<span><Expr expr={base}/>({showArgs})</span>);
    }

  case "RawParen":
    return <span>(<Expr expr={expr.contents}/>)</span>;
  case "RawMethod":
    let [mBase, method] = expr.contents;
    return (<span><Expr expr={mBase}/>.<Expr expr={method} /></span>);
  case "RawIfThenElse":
    let [, ifExpr, thenExpr, elseExpr] = expr.contents;
    return (<span><KeyWord>if</KeyWord> <Expr expr={ifExpr}/> <KeyWord>then</KeyWord> <Expr expr={thenExpr}/> <KeyWord>else</KeyWord> <Expr expr={elseExpr}/></span>);
  case "RawMatch":
    let [, matchExpr, matchPatterns] = expr.contents;
    let showMPatterns = tagJoin(matchPatterns.map((pattern, patternIndex) => {
      let [[obj, guard], matchExpr] = pattern;
      return (<div key={patternIndex}><Obj obj={obj} Meta={RawMeta}/><Guard guard={guard} Expr={Expr}/><KeyWord> =&gt;</KeyWord> <Expr expr={matchExpr} /></div>);
    }), "");
    return (<span><KeyWord>match</KeyWord> <Expr expr={matchExpr}/> <KeyWord>of</KeyWord> <div className={classes.indented}>{showMPatterns}</div></span>);
  case "RawCase":
    let [, caseExpr, casePatterns] = expr.contents;
    let showCPatterns = tagJoin(casePatterns.map((pattern, patternIndex) => {
      let [[obj, guard], caseExpr] = pattern;
      return (<div key={patternIndex}><Obj obj={obj} Meta={RawMeta}/><Guard guard={guard} Expr={Expr}/><KeyWord> =&gt;</KeyWord> <Expr expr={caseExpr}/></div>);
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

function PlayButton(props) {
  const {fun} = props;
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
        <MenuItem onClick={linkClose(`/build/${prgmName}/${fun}`)}>Build</MenuItem>
        <MenuItem onClick={linkClose(`/debug/${prgmName}/${fun}`)}>Debug</MenuItem>
      </Menu>
    </span>
  );
}

export function Comment(props) {
  const {comment, obj} = props;
  let {objNames, classToType} = useContext(ResMaps);
  objNames = objNames || {};
  classToType = classToType || {};

  // Replace usages of [TypeName] and [ClassName] with catn:// link reference
  const regex = /\[(\S+)\][^[(]/g;
  const comment2 = comment.replaceAll(regex, (m, name) => {
    if(classToType && name in classToType) {
      return `[${name}](catln://class/${name})${m.slice(-1)}`;
    } else if(objNames && name in objNames) {
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
