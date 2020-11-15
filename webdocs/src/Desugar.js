import React from 'react';

import List from '@material-ui/core/List';
import ListItem from '@material-ui/core/ListItem';
import ListItemText from '@material-ui/core/ListItemText';
import Card from '@material-ui/core/Card';
import CardContent from '@material-ui/core/CardContent';

const useStyles = {
  objDetails: {
    float: 'right',
    color: 'gray'
  },
  arrow: {
    background: '#f6'
  },
  arrowDeclaration: {
    padding: 0,
    fontWeight: 'bold'
  },
  arrowExpression: {
    padding: 0
  }
};

class Desugar extends React.Component {
  constructor(props) {
    super(props);
    this.state = {
      error: null,
      isLoaded: false,
      objMap: [],
      classMap: [],
      notes: []
    };
  }

  componentDidMount() {
    fetch("/desugar")
      .then(res => res.json())
      .then(
        (result) => {
          if(result.length === 2) {
            this.setState({
              isLoaded: true,
              objMap: result[0][0],
              classMap: result[0][1],
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

  render() {
    const { error, isLoaded, objMap } = this.state;
    if (error) {
      return <div>Error: {error.message}</div>;
    } else if (!isLoaded) {
      return <div>Loading...</div>;
    } else {
      return (
          <div>
            <ObjMap objMap={objMap}/>
          </div>
      );
    }
  }
}

class ObjMap extends React.Component {
  render() {
    return (
      <List component="nav">
        {this.props.objMap
         .sort((obj1, obj2) => obj1[0][2] < obj2[0][2])
         .map(obj =>
            <ObjArrows objas={obj} />
        )}
      </List>
    );
  }
}

class ObjArrows extends React.Component {
  render() {
    const [obj, arrows] = this.props.objas;
    const [objM, objBasis, name, vars, args] = obj;

    let showVars;
    if(Object.keys(vars).length > 0) {
      showVars = (
          <span>
          &lt;
          {Object.keys(vars).map(v => <>{renderType(vars[v])} {v}</>)
           .reduce((acc, x) => acc == null ? [x] : <>{acc}, {x}</>, null)}
          &gt;
        </span>);
    }

    let showArgs;
    if(Object.keys(args).length > 0) {
      showArgs = (
        <span>
          (
          {Object.keys(args).map(arg => <>{renderType(args[arg][0])} {arg}</>)
           .reduce((acc, x) => acc == null ? [x] : <>{acc}, {x}</>, null)}
          )
        </span>);
    }

    let showArrows;
    if(Object.keys(arrows).length > 0) {
      showArrows = (
        <div>
          {arrows.map(arrow => <Arrow arrow={arrow} />)}
        </div>
      );
    }

    let primary = (<div>
                   <span> {name}{showVars}{showArgs}</span>
                   <span style={useStyles.objDetails}>{objBasis} - {renderType(objM)}</span>
                   </div>);

    return (
        <ListItem divider>
          <ListItemText primary={primary} secondary={showArrows} />
        </ListItem>
    );
  }
}

class Arrow extends React.Component {
  render() {
    const [arrM, , guard, maybeExpr] = this.props.arrow;

    let showExpr;
    if(maybeExpr) {
      showExpr = ` = ${renderExpr(maybeExpr)}`;
    }

    let header = (<span>{renderGuard(guard)} -> {renderType(arrM)}</span>);

    return (
      <Card style={useStyles.arrow}>
        <CardContent style={useStyles.arrowDeclaration}>{header}</CardContent>
        <CardContent style={useStyles.arrowExpression}>{showExpr}</CardContent>
      </Card>
    );
  }
}

function renderType(t) {
  switch(t.tag) {
  case "TopType":
    return "TopType";
  case "TypeVar":
    return t.contents.contents;
  case "SumType":
    let partials = t.contents.map(partialOptions => {
      let [partialName, options] = partialOptions;
      return options.map(partialData => {
        let [partialVars, , partialArgs] = partialData;

        let showVars = "";
        if(Object.keys(partialVars).length > 0) {
          showVars = (
            <span>
              &lt;
            {Object.keys(partialVars).map(v => <>{renderType(partialVars[v])} {v}</>)
             .reduce((acc, x) => acc == null ? [x] : <>{acc}, {x}</>, null)}
              &gt;
            </span>
          );
        }

        let showArgs = "";
        if(Object.keys(partialArgs).length > 0) {
          showArgs = (
              <span>
              (
                {Object.keys(partialArgs).map(arg => <>{renderType(partialArgs[arg])} {arg}</>)
                 .reduce((acc, x) => acc == null ? [x] : <>{acc}, {x}</>, null)}
              )
            </span>
          );
        }

        return (<span>{partialName.contents}{showVars}{showArgs}</span>);
      });
    }).flat();
    return partials.reduce((acc, x) => acc == null ? [x] : <>{acc} | {x}</>, null);
  default:
    console.error("Unknown renderMeta");
    return "";
  }
}

function renderGuard(guard) {
  switch(guard.tag) {
  case "IfGuard":
    return `if (${renderExpr(guard.contents[0])})`;
  case "ElseGuard":
    return "else";
  case "NoGuard":
    return "";
  default:
    console.error("Unkwnon guard: ", guard);
    return "";
  }
}

function renderExpr(expr) {
  switch(expr.tag) {
  case "ICExpr":
    return "" + expr.contents[1].contents;
  case "IValue":
    return "" + expr.contents[1];
  case "IArg":
    return "" + expr.contents[1];
  case "ITupleApply":
    const [, [,base], arg, subExpr] = expr.contents;

    let showArg;
    if(arg) {
      showArg = `${arg} = `;
    }

    return `${renderExpr(base)}(${showArg}${renderExpr(subExpr)})`;
  default:
    console.error("Unknown renderExpr", expr);
  }
}

export default Desugar;
