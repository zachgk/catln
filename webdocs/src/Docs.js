import React from 'react';

import Grid from '@material-ui/core/Grid';

import {renderObj, renderGuard, renderType, tagJoin} from './Common';

const useStyles = {
  indented: {
    marginLeft: '10em'
  }
};

class Docs extends React.Component {
  constructor(props) {
    super(props);
    this.state = {
      error: null,
      isLoaded: false,
      pages: "",
      curPage: 0,
      notes: []
    };
    this.showPage.bind(this);
  }

  runFetch() {
    fetch("/raw")
      .then(res => res.json())
      .then(
        (result) => {
          if(result.length === 2) {
            this.setState({
              isLoaded: true,
              pages: result[0],
              curPage: 0,
              notes: result[1]
            });
          } else {
            this.setState({
              isLoaded: true,
              notes: result[0]
            });
          }
        },
        // Note: it's important to handle errors here
        // instead of a catch() block so that we don't swallow
        // exceptions from actual bugs in components.
        (error) => {
          this.setState({
            isLoaded: true,
            error
          });
        }
      );
  }

  componentDidMount() {
    this.runFetch();
  }

  showPage(pageIndex) {
    return () => {
      this.setState({curPage: pageIndex});
    };
  }

  render() {
    const { error, isLoaded, pages, curPage } = this.state;
    if (error) {
      return <div>Error: {error.message}</div>;
    } else if (!isLoaded) {
      return <div>Loading...</div>;
    } else {
      return (
        <Grid container spacing={2} justify="center">
          <Grid item xs={2}>
            {pages.map((page, index) => <div key={page[0]} onClick={this.showPage(index)}>{page[0]}</div>)}
          </Grid>
          <Grid item xs={8}>
            {renderStatements(pages[curPage][1][1])}
          </Grid>
        </Grid>
      );
    }
  }
}

function renderStatements(statements) {
  return statements.map(renderStatement);
}

function renderStatement(statement) {
  switch(statement.tag) {
  case "RawDeclStatement":
    let [lhs, subStatements, maybeExpr] = statement.contents;
    let [, [obj, guard]] = lhs;

    let showExpr;
    if(maybeExpr) {
      showExpr = (<span> = {renderExpr(maybeExpr)}</span>);
    }

    return (
      <div>
        {renderObj(obj)}{renderGuard(guard)}{showExpr}
        <div style={useStyles.indented}>{renderStatements(subStatements)}</div>
      </div>
    );
  case "MultiTypeDefStatement":
    let [className, classVars, classDatas] = statement.contents;

    let showClassVars;
    if(Object.keys(classVars).length > 0) {
      showClassVars = (
        <span>
          &lt;
          {tagJoin(Object.keys(classVars).map(v => <>{renderType(classVars[v])} v</>), ", ")}
          &gt;
        </span>
      );
    }

    let showClassDatas = tagJoin(classDatas.map(d => <span>{renderType(d)}</span>), " | ");

    return (<div>class {className}{showClassVars} = {showClassDatas}</div>);
  case "TypeDefStatement":
    return (<div>data {renderType(statement.contents)}</div>);
  case "RawClassDefStatement":
    let [instanceType, instanceClass] = statement.contents;
    return (<div>instance {instanceType} of {instanceClass}</div>);
  default:
    console.error("Unknown renderStatement");
    return "";
  }
}

function renderExpr(expr) {
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
        return (<span>{arg.contents[0]} = {renderExpr(arg.contents[1])}</span>);
      case "RawTupleArgInfer":
        return renderExpr(arg.contents);
      default:
        console.error("Unknown renderExpr tupleApply arg type");
        return "";
      }
    }), ", ");

    return (<span>{renderExpr(base)}({showArgs})</span>);
  case "RawMethods":
    let [mBase, methods] = expr.contents;
    let showMethods = tagJoin(methods.map(method => <span>.{renderExpr(method)}</span>), "");
    return (<span>{renderExpr(mBase)}{showMethods}</span>);
  case "RawIfThenElse":
    let [, ifExpr, thenExpr, elseExpr] = expr.contents;
    return (<span>if {renderExpr(ifExpr)} then {renderExpr(thenExpr)} else {renderExpr(elseExpr)}</span>);
  case "RawMatch":
    let [, matchExpr, matchPatterns] = expr.contents;
    let showMPatterns = tagJoin(matchPatterns.map(pattern => {
      let [obj, guard] = pattern;
      return (<span>{renderObj(obj)}{renderGuard(guard, renderExpr)}</span>);
    }), "");
    return (<span>match {renderExpr(matchExpr)} of <div style={useStyles.indented}>{showMPatterns}</div></span>);
  case "RawCase":
    let [, caseExpr, casePatterns] = expr.contents;
    let showCPatterns = tagJoin(casePatterns.map(pattern => {
      let [obj, guard] = pattern;
      return (<span>{renderObj(obj)}{renderGuard(guard, renderExpr)}</span>);
    }), "");
    return (<span>case {renderExpr(caseExpr)} of <div style={useStyles.indented}>{showCPatterns}</div></span>);
  default:
    console.error("Unknown renderExpr", expr);
    return "";
  }
}

export default Docs;
