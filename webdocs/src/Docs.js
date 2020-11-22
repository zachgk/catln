import React from 'react';

import Grid from '@material-ui/core/Grid';
import {
  Switch,
  Route,
  Redirect,
  Link,
  useParams,
  useRouteMatch
} from 'react-router-dom';

import {useApi, Loading, Obj, Guard, Type, tagJoin} from './Common';

const useStyles = {
  indented: {
    marginLeft: '10em'
  }
};

function Docs() {
  let apiResult = useApi("/raw");

  return (
    <Loading status={apiResult}>
      <Main data={apiResult.data} />
    </Loading>
  );
}

function Main(props) {
  let pages = props.data;
  let { path } = useRouteMatch();
  return (
    <Switch>
      <Route exact path={path}>
        <Redirect to={`${path}/0`} />
      </Route>
      <Route path={`${path}/:curPage`}>
        <Grid container spacing={2} justify="center">
          <Grid item xs={2}>
            {pages.map((page, index) => <div><Link key={page[0]} to={`${path}/${index}`}>{page[0]}</Link></div>)}
          </Grid>
          <Grid item xs={8}>
            <MainStatements pages={pages} />
          </Grid>
        </Grid>
      </Route>
    </Switch>
  );
}

function MainStatements(props) {
  let { curPage } = useParams();
  let statements = props.pages[parseInt(curPage)][1][1];
  return <Statements statements={statements}/>;
}

function Statements(props) {
  return props.statements.map(statement => <Statement statement={statement}/>);
}

function Statement(props) {
  let {statement} = props;
  switch(statement.tag) {
  case "RawDeclStatement":
    let [lhs, subStatements, maybeExpr] = statement.contents;
    let [, [obj, guard]] = lhs;

    let showExpr;
    if(maybeExpr) {
      showExpr = (<span> = <Expr expr={maybeExpr}/></span>);
    }

    return (
      <div>
        <Obj obj={obj} Meta={Type}/><Guard guard={guard} Expr={Expr}/>{showExpr}
        <div style={useStyles.indented}><Statements statements={subStatements}/></div>
      </div>
    );
  case "MultiTypeDefStatement":
    let [className, classVars, classDatas] = statement.contents;

    let showClassVars;
    if(Object.keys(classVars).length > 0) {
      showClassVars = (
        <span>
          &lt;
          {tagJoin(Object.keys(classVars).map(v => <><Type data={classVars[v]}/> v</>), ", ")}
          &gt;
        </span>
      );
    }

    let showClassDatas = tagJoin(classDatas.map(d => <span><Type data={d}/></span>), " | ");

    return (<div>class {className}{showClassVars} = {showClassDatas}</div>);
  case "TypeDefStatement":
    return (<div>data <Type data={statement.contents}/></div>);
  case "RawClassDefStatement":
    let [instanceType, instanceClass] = statement.contents;
    return (<div>instance {instanceType} of {instanceClass}</div>);
  default:
    console.error("Unknown renderStatement");
    return "";
  }
}

function Expr(props) {
  let {expr} = props;
  switch(expr.tag) {
  case "RawCExpr":
    return "" + expr.contents[1].contents;
  case "RawValue":
    return "" + expr.contents[1];
  case "RawTupleApply":
    const [, [,base], args] = expr.contents;

    let showArgs = tagJoin(args.map(arg => {
      switch(arg.tag) {
      case "RawTupleArgNamed":
        return (<span>{arg.contents[0]} = <Expr expr={arg.contents[1]}/></span>);
      case "RawTupleArgInfer":
        return <Expr expr={arg.contents}/>;
      default:
        console.error("Unknown renderExpr tupleApply arg type");
        return "";
      }
    }), ", ");

    return (<span><Expr expr={base}/>({showArgs})</span>);
  case "RawMethods":
    let [mBase, methods] = expr.contents;
    let showMethods = methods.map(method => <span>.<Expr expr={method}/></span>);
    return (<span><Expr expr={mBase}/>{showMethods}</span>);
  case "RawIfThenElse":
    let [, ifExpr, thenExpr, elseExpr] = expr.contents;
    return (<span>if <Expr expr={ifExpr}/> then <Expr expr={thenExpr}/> else <Expr expr={elseExpr}/></span>);
  case "RawMatch":
    let [, matchExpr, matchPatterns] = expr.contents;
    let showMPatterns = tagJoin(matchPatterns.map(pattern => {
      let [obj, guard] = pattern;
      return (<span><Obj obj={obj} Meta={Type}/><Guard guard={guard} Expr={Expr}/></span>);
    }), "");
    return (<span>match <Expr expr={matchExpr}/> of <div style={useStyles.indented}>{showMPatterns}</div></span>);
  case "RawCase":
    let [, caseExpr, casePatterns] = expr.contents;
    let showCPatterns = tagJoin(casePatterns.map(pattern => {
      let [obj, guard] = pattern;
      return (<span><Obj obj={obj} Meta={Type}/><Guard guard={guard} Expr={Expr}/></span>);
    }), "");
    return (<span>case <Expr expr={caseExpr}/> of <div style={useStyles.indented}>{showCPatterns}</div></span>);
  default:
    console.error("Unknown renderExpr", expr);
    return "";
  }
}

export default Docs;
