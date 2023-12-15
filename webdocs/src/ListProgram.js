import React from 'react';

import { makeStyles } from '@material-ui/core/styles';
import List from '@material-ui/core/List';
import ListItem from '@material-ui/core/ListItem';
import ListItemText from '@material-ui/core/ListItemText';
import FormGroup from '@material-ui/core/FormGroup';
import FormControlLabel from '@material-ui/core/FormControlLabel';
import Switch from '@material-ui/core/Switch';
import {
  useHistory,
  useLocation,
  useRouteMatch
} from 'react-router-dom';

import {useApi, tagJoin, Loading, PTypeName, PClassName, PartialType, Type} from './Common/Common';
import {ObjArr} from './Common/Semantics';

const useStyles = makeStyles({
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
});

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
        .map((oa, objIndex) =>
          <RootObjArr key={objIndex} oa={oa} Meta={props.Meta} showExprMetas={props.showExprMetas}/>
      )}
    </List>
  );
}

function RootObjArr(props) {
  const classes = useStyles();

  let primary = <ObjArr oa={props.oa} details={classes.objDetails} Meta={props.Meta}/>;

  return (
      <ListItem divider>
        <ListItemText disableTypography primary={primary} />
      </ListItem>
  );
}

function Meta(props) {
  let [tp, ] = props.data;
  return <Type data={tp} />;
}

export function ClassComments(props) {
  const [,classMap] = props.data;
  const [, classToType] = classMap;
  const { name } = props;
  let showComments = "";
  const classType = classToType[name] || [];
  // if (classType[3] && classType[3].length > 0) {
  //   showComments = <Comment comment={classType[3]} obj={undefined} />;
  // }
  return showComments;
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
  const [, cls, types,] = props.val;

  let showTypes = tagJoin(types.map((t, i) => <span key={i}><Type data={t}/></span>), ", ");

  return (
    <div>
      <PartialType data={cls} /> = {showTypes}
    </div>
  );
}

export default ListProgram;
export {
  ObjMap,
  ShowList
};
