import React, {useMemo} from 'react';

import makeStyles from '@mui/styles/makeStyles';
import List from '@mui/material/List';
import ListItem from '@mui/material/ListItem';
import ListItemText from '@mui/material/ListItemText';
import FormGroup from '@mui/material/FormGroup';
import FormControlLabel from '@mui/material/FormControlLabel';
import Radio from '@mui/material/Radio';
import RadioGroup from '@mui/material/RadioGroup';

import {tagJoin, PTypeName, PClassName, PartialType, Type} from '../Common';
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

// The members of the Catln ListProgramMode enum, which is what a #listProgram annotation names.
// The compiler decides what each one lists; this only names the mode and labels it.
const MODES = [
  {name: "/Catln/Doc/Desugar", label: "No Typecheck"},
  {name: "/Catln/Doc/Typecheck", label: "List"},
  {name: "/Catln/Doc/TreeBuild", label: "TreeBuild"}
];

function ListProgram(props) {
  const {update} = props;

  // Both fields are required arguments of Show/ListProgram, so a compiled value always carries the
  // mode it was listed for and the program listed at that mode. Nothing is fetched: switching mode
  // means asking the compiler for a different annotation, not for a different route.
  const mode = props["/mode"].name;
  const prgm = props["/prgm"];
  const listed = useMemo(() => JSON.parse(prgm.contents), [prgm]);

  // Without an update there is nothing to rewrite, so the radios are disabled below and this never
  // runs. They still show which mode the source named, which is also the right rendering for a
  // source that cannot be edited at all, such as one served read-only.
  let switchMode = (event) => update(`#listProgram(mode= ${event.target.value})`);

  return (
    <div>
      <FormGroup row>
        <FormControlLabel
          control={
            <RadioGroup aria-label="mode" value={mode} onChange={switchMode}>
              {MODES.map(m => <FormControlLabel key={m.name} value={m.name} control={<Radio />} label={m.label} disabled={!update} />)}
            </RadioGroup>
          }
        />
      </FormGroup>
      <ShowList data={listed} />
    </div>
  );
}

// A program as the compiler writes one, by the names of its fields in src/Semantics/Prgm.hs: its
// object map, its class graph (`prgmCg`) and its annotations, which a listing does not show.
function ShowList(props) {
  let {prgmObjMap, prgmCg} = props.data;

  return (
    <div>
      <ObjMap objMap={prgmObjMap} Meta={Meta}/>
      <br /> <br /> <br />
      <ClassMap classMap={prgmCg} />
    </div>
  );
}

// An ObjectMap, which holds its items under `omItems`: keyed by type name, each an ObjectMapItem
// with the declarations, runtime declarations, refinement declarations and definitions for that name.
function ObjMap(props) {
  const {objMap} = props;
  const items = objMap.omItems;
  return (
    <List component="nav">
      {Object.keys(items).sort().flatMap(name => {
        const {omiDecls, omiRuntimeDecls, omiRefineDecls, omiDefs} = items[name];
        return [...omiDecls, ...omiRuntimeDecls, ...omiRefineDecls, ...omiDefs]
          .map((oa, objIndex) =>
            <RootObjArr key={`${name}/${objIndex}`} oa={oa} Meta={props.Meta} showExprMetas={props.showExprMetas}/>
        );
      })}
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
