import React from 'react';

import List from '@material-ui/core/List';
import ListItem from '@material-ui/core/ListItem';
import ListItemText from '@material-ui/core/ListItemText';
import Card from '@material-ui/core/Card';
import CardContent from '@material-ui/core/CardContent';

import {renderGuard, renderType, renderObj} from './Common';

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

class ListProgram extends React.Component {
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

  runFetch() {
    fetch(this.props.dataPath)
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

  componentDidMount() {
    this.runFetch();
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

    let showArrows;
    if(Object.keys(arrows).length > 0) {
      showArrows = (
        <div>
          {arrows.map(arrow => <Arrow arrow={arrow} />)}
        </div>
      );
    }

    let primary = renderObj(obj, useStyles.objDetails);

    return (
        <ListItem divider>
          <ListItemText disableTypography primary={primary} secondary={showArrows} />
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

    let header = (<span>{renderGuard(guard, renderExpr)} -&gt; {renderType(arrM)}</span>);

    return (
      <Card style={useStyles.arrow}>
        <CardContent style={useStyles.arrowDeclaration}>{header}</CardContent>
        <CardContent style={useStyles.arrowExpression}>{showExpr}</CardContent>
      </Card>
    );
  }
}

function renderExpr(expr) {
  switch(expr.tag) {
  case "ICExpr":
  case "CExpr":
    return "" + expr.contents[1].contents;
  case "IValue":
  case "Value":
    return "" + expr.contents[1];
  case "IArg":
  case "Arg":
    return "" + expr.contents[1];
  case "ITupleApply":
  case "TupleApply":
    const [, [,base], arg, subExpr] = expr.contents;

    let showArg;
    if(arg) {
      showArg = `${arg} = `;
    }

    return `${renderExpr(base)}(${showArg}${renderExpr(subExpr)})`;
  default:
    console.error("Unknown renderExpr", expr);
    return "";
  }
}

export default ListProgram;
