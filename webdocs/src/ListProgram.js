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

import {useApi, tagJoin, Loading, Guard, PTypeName, PClassName, Type, Obj} from './Common';

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
    dataPath = "/api/desugar";
  } else {
    dataPath = "/api/typecheck";
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
        <ShowList data={apiResult.data} />
      </Loading>
    </div>
  );
}

function useQuery() {
  return new URLSearchParams(useLocation().search);
}

function ShowList(props) {
  let [objMap, classMap] = props.data;

  return (
    <div>
      <ObjMap objMap={objMap} Meta={Meta}/>
      <br /> <br /> <br />
      <ClassMap classMap={classMap} />
    </div>
  );
}

function ObjMap(props) {
  return (
    <List component="nav">
      {props.objMap
        .sort((obj1, obj2) => obj1[0].objName < obj2[0].objName)
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

function ClassMap(props) {
  const {classMap} = props;
  const [typeToClass, classToType] = classMap;
  return (
    <div>
      <h2>Types</h2>
      {tagJoin(Object.keys(typeToClass).map(typeName => <TypeToClassEntry key={typeName} typeName={typeName} classes={typeToClass[typeName]} />), "")}
      <br/>
      <h2>Classes</h2>
      {tagJoin(Object.keys(classToType).map(className => <ClassToTypeEntry key={className} className={className} val={classToType[className]} />), "")}
    </div>
  );
}

function TypeToClassEntry(props) {
  const {typeName, classes} = props;

  let showTypeName = <PTypeName name={typeName} />;
  let showClasses = tagJoin(classes.map(c => <PClassName key={c} name={c} />), ", ");

  return <div>{showTypeName}: {showClasses}</div>;
}

function ClassToTypeEntry(props) {
  const {className, val: [, vars, types]} = props;

  let showVars = "";
  if(Object.keys(vars).length > 0) {
    showVars = (
      <span>
        &lt;
        {tagJoin(Object.keys(vars).map(v => <span key={v}><Type data={vars[v]}/> {v}</span>), ", ")}
        &gt;
      </span>
    );
  }

  let showClassName = <PClassName name={className} />;
  let showTypes = tagJoin(types.map((t, i) => <span key={i}><Type data={t}/></span>), ", ");

  return <div>{showClassName}{showVars} = {showTypes}</div>;
}

export default ListProgram;
export {
  ObjMap,
  ShowList
};
