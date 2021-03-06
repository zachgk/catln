import React from 'react';

import List from '@material-ui/core/List';
import ListItem from '@material-ui/core/ListItem';
import ListItemText from '@material-ui/core/ListItemText';
import Card from '@material-ui/core/Card';
import CardContent from '@material-ui/core/CardContent';
import FormGroup from '@material-ui/core/FormGroup';
import FormControlLabel from '@material-ui/core/FormControlLabel';
import Switch from '@material-ui/core/Switch';
import {
  useHistory,
  useLocation,
  useRouteMatch
} from 'react-router-dom';

import {useApi, Loading, Guard, Type, Obj} from './Common';

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

function ListProgram(props) {
  let query = useQuery();
  let history = useHistory();

  let noTypecheck = (query.get("noTypecheck") || "false") === "true";
  let dataPath;
  if(noTypecheck) {
    dataPath = "/desugar";
  } else {
    dataPath = "/typecheck";
  }
  let apiResult = useApi(dataPath);
  let { path } = useRouteMatch();

  let switchTypecheck = (event) => {
    query.set("noTypecheck", !noTypecheck);
    history.push({
      pathname: path,
      search: `?${query.toString()}`
    });
  };

  return (
    <div>
      <FormGroup row>
        <FormControlLabel
          control={
            <Switch
              checked={noTypecheck}
              onChange={switchTypecheck}
              color="primary"
            />
          }
          label="No Typecheck"
        />
      </FormGroup>
      <Loading status={apiResult}>
        <Main data={apiResult.data} />
      </Loading>
    </div>
  );
}

function useQuery() {
  return new URLSearchParams(useLocation().search);
}

function Main(props) {
  let [objMap, ] = props.data;

  return (
    <div>
      <ObjMap objMap={objMap} Meta={Meta}/>
    </div>
  );
}

function ObjMap(props) {
  return (
    <List component="nav">
      {props.objMap
        .sort((obj1, obj2) => obj1[0][2] < obj2[0][2])
        .map((obj, objIndex) =>
          <ObjArrows key={objIndex} objas={obj} Meta={props.Meta} showExprMetas={props.showExprMetas}/>
      )}
    </List>
  );
}

function ObjArrows(props) {
  const [obj, arrows] = props.objas;

  let showArrows;
  if(Object.keys(arrows).length > 0) {
    showArrows = (
      <div>
        {arrows.map((arrow, arrowIndex) => <Arrow key={arrowIndex} arrow={arrow} Meta={props.Meta} showExprMetas={props.showExprMetas}/>)}
      </div>
    );
  }

  let primary = <Obj obj={obj} details={useStyles.objDetails} Meta={props.Meta}/>;

  return (
      <ListItem divider>
        <ListItemText disableTypography primary={primary} secondary={showArrows} />
      </ListItem>
  );
}

function Arrow(props) {
  const {Meta} = props;
  const [arrM, , guard, maybeExpr] = props.arrow;

  let showExpr;
  if(maybeExpr) {
    showExpr = <span> = <Expr expr={maybeExpr} Meta={Meta} showMetas={props.showExprMetas}/></span>;
  }

  let header = (<span><Guard guard={guard} Expr={Expr} Meta={Meta} showExprMetas={props.showExprMetas}/> -&gt; <Meta data={arrM} /></span>);

  return (
    <Card style={useStyles.arrow}>
      <CardContent style={useStyles.arrowDeclaration}>{header}</CardContent>
      <CardContent style={useStyles.arrowExpression}>{showExpr}</CardContent>
    </Card>
  );
}

function Expr(props) {
  let {expr, Meta, showMetas} = props;
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
    const [m, [baseM ,base], arg, subExpr] = expr.contents;

    let showArg;
    if(arg) {
      showArg = `${arg} = `;
    }

    let showBaseM;
    if(showMetas) {
      showBaseM = <i><Meta data={baseM}/></i>;
    }

    let showM;
    if(showMetas) {
      showM = <i>[<Meta data={m}/>]</i>;
    }

    let showBase = <Expr expr={base} Meta={Meta} showMetas={showMetas}/>;
    let showSubExpr = <Expr expr={subExpr} Meta={Meta} showMetas={showMetas}/>;

    return <span>{showBase}({showArg} {showBaseM} {showSubExpr}){showM}</span>;
  default:
    console.error("Unknown renderExpr", expr);
    return "";
  }
}

function Meta(props) {
  let [tp, ] = props.data;
  return <Type data={tp} />;
}

export default ListProgram;
export {
  ObjMap
};
