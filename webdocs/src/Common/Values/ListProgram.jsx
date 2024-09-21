import React, {useState} from 'react';

import makeStyles from '@mui/styles/makeStyles';
import List from '@mui/material/List';
import ListItem from '@mui/material/ListItem';
import ListItemText from '@mui/material/ListItemText';
import FormGroup from '@mui/material/FormGroup';
import FormControlLabel from '@mui/material/FormControlLabel';
import Radio from '@mui/material/Radio';
import RadioGroup from '@mui/material/RadioGroup';

import {useApi, tagJoin, Loading, PTypeName, PClassName, PartialType, Type} from '../Common';
import {ObjArr} from '../Semantics';

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

function ListProgram() {
  const [mode, setMode] = useState("");

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

  let switchTypecheck = (event) => {
    setMode(event.target.value);
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
  let {prgmObjMap, prgmCG} = props.data;

  return (
    <div>
      <ObjMap objMap={prgmObjMap} Meta={Meta}/>
      <br /> <br /> <br />
      <ClassMap classMap={prgmCG} />
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

export function ClassComments() {
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
