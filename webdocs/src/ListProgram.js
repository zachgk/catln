import React from 'react';

import { makeStyles } from '@material-ui/core/styles';
import List from '@material-ui/core/List';
import ListItem from '@material-ui/core/ListItem';
import ListItemText from '@material-ui/core/ListItemText';
import FormGroup from '@material-ui/core/FormGroup';
import FormControlLabel from '@material-ui/core/FormControlLabel';
import Radio from '@material-ui/core/Radio';
import RadioGroup from '@material-ui/core/RadioGroup';import {
  useHistory,
  useRouteMatch
} from 'react-router-dom';

import {useQuery, useApi, tagJoin, Loading, PTypeName, PClassName, PartialType, Type} from './Common/Common';
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

  let mode = query.get("mode") || "";
  let dataPath;
  switch(mode) {
  case "noTypecheck":
    dataPath = "/api/desugar";
    break;
  case "treeBuild":
    dataPath = "/api/treebuild";
    break;
  default:
    dataPath = "/api/typecheck";
    break;
  }
  let apiResult = useApi(dataPath);
  let { path } = useRouteMatch();

  let switchTypecheck = (event) => {
    if (event.target.value) {
      query.set("mode", event.target.value);
    } else {
      query.delete("mode");
    }

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
            <RadioGroup aria-label="mode" defaultValue="" value={mode} onChange={switchTypecheck}>
                    <FormControlLabel value="noTypecheck" control={<Radio />} label="No Typecheck" />
                    <FormControlLabel value="" control={<Radio />} label="List" />
                    <FormControlLabel value="treeBuild" control={<Radio />} label="TreeBuild" />
            </RadioGroup>
          }
        />
      </FormGroup>
      <Loading status={apiResult}>
        <ShowList data={apiResult.data} />
      </Loading>
    </div>
  );
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

  let primary = <ObjArr oa={props.oa} details={classes.objDetails} multiLine={true} Meta={props.Meta} showExprMetas={props.showExprMetas}/>;

  return (
      <ListItem divider>
        <ListItemText disableTypography primary={primary} />
      </ListItem>
  );
}

function Meta(props) {
  let {getMetaType} = props.data;
  return <Type data={getMetaType} />;
}

export function ClassComments(props) {
  return "ClassComments";
  // const [,classMap] = props.data;
  // const [, classToType] = classMap;
  // const { name } = props;
  // let showComments = "";
  // const classType = classToType[name] || [];
  // if (classType[3] && classType[3].length > 0) {
  //   showComments = <Comment comment={classType[3]} obj={undefined} />;
  // }
  // return showComments;
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
