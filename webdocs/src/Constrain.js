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

import {useApi, posKey, Loading, Notes, Type} from './Common';
import {ObjMap} from './ListProgram';

const useStyles = {
  objMap: {
    maxHeight: '100vh',
    overflow: 'scroll'
  }
};

function Constrain() {
  let apiResult = useApi("/constrain");

  return (
    <Loading status={apiResult}>
      <Main data={apiResult.data} notes={apiResult.notes} />
    </Loading>
  );
}

function Main(props) {
  let {notes, data: [, prgm, trace]} = props;
  let { path } = useRouteMatch();

  var notesMap = {};
  notes.forEach(note => {
    if(note.pos) {
      let k = posKey(note.pos);
      if(k in notesMap) {
        notesMap[k].push(note);
      } else {
        notesMap[k] = [note];
      }
    }
  });
  let Meta = VarMeta(notesMap);

  return (
    <div>
      <Notes notes={notes} noPosOnly />
      <Grid container spacing={2} justify="center">
        <Grid item xs >
          <div style={useStyles.objMap}>
            <ObjMap objMap={prgm[0]} Meta={Meta} showExprMetas />
          </div>
        </Grid>
        <Grid item xs>
          <Switch>
            <Route exact path={path}>
              <Redirect to={`${path}/0`} />
            </Route>
            <Route path={`${path}/:curMeta`}>
              <TraceEpochs trace={trace} Meta={Meta} />
            </Route>
          </Switch>
        </Grid>
      </Grid>
    </div>
  );
}

let VarMeta = (notesMap) => (props) => {
  const [pnt, [tp, pos], ] = props.data;
  var style = {};

  if(pos && posKey(pos) in notesMap) {
    style.background = 'lightCoral';
  }

  return <span style={style}><Type data={tp}/>(<Pnt pnt={pnt} />)</span>;
};

function TraceEpochs(props) {
  let { curMeta } = useParams();
  let {trace, Meta} = props;
  let traces = trace.map((t, index) => {
    return (
      <div key={index}>
        <h3>{index}</h3>
        <Trace trace={t} Meta={Meta} />
      </div>
    );
  });
  return (
    <div>
      <h2>Pnt {curMeta}</h2>
      {traces}
    </div>
  );
}

function Trace(props) {
  let {trace, Meta} = props;
  let { curMeta } = useParams();
  curMeta = parseInt(curMeta);
  return trace.map((constraintPair, constraintIndex) => {
    let [constraint, updates] = constraintPair;

    let filteredUpdates = updates.filter(update => update[0] === curMeta);
    if(filteredUpdates.length === 0) {
      return null;
    }

    let showUpdates = filteredUpdates.map((update, updateIndex) => <Scheme key={updateIndex} scheme={update[1]} />);

    return (
      <div key={constraintIndex}>
        <b><Constraint constraint={constraint} Meta={Meta}/></b>
        {showUpdates}
      </div>
    );
  });
}

function Constraint(props) {
  let {constraint, Meta} = props;
  switch(constraint.tag) {
  case "EqualsKnown":
    return (<span><Meta data={constraint.contents[0]}/> k== <Type data={constraint.contents[1]}/></span>);
  case "EqPoints":
    return (<span><Meta data={constraint.contents[0]}/> p== <Meta data={constraint.contents[1]}/></span>);
  case "BoundedByKnown":
    return (<span><Meta data={constraint.contents[0]}/> ⊆ <Type data={constraint.contents[1]}/></span>);
  case "BoundedByObjs":
    return (<span>{constraint.contents[0]} <Meta data={constraint.contents[1]}/></span>);
  case "ArrowTo":
    return (<span><Meta data={constraint.contents[0]}/> -&gt; <Meta data={constraint.contents[1]} /></span>);
  case "PropEq":
    return (<span>(<Meta data={constraint.contents[0][0]}/>).{constraint.contents[0][1]} == <Meta data={constraint.contents[1]}/></span>);
  case "VarEq":
    return (<span>(<Meta data={constraint.contents[0][0]}/>).{constraint.contents[0][1]} == <Meta data={constraint.contents[1]}/></span>);
  case "AddArg":
    return (<span>(<Meta data={constraint.contents[0][0]}/>)({constraint.contents[0][1]}) arg== <Meta data={constraint.contents[1]}/></span>);
  case "AddInferArg":
    return (<span>(<Meta data={constraint.contents[0]}/>)(?) iarg== <Meta data={constraint.contents[1]}/></span>);
  case "PowersetTo":
    return (<span>P(<Meta data={constraint.contents[0]}/>) ps== <Meta data={constraint.contents[1]}/></span>);
  case "UnionOf":
    return (<span>UnionOf <Meta data={constraint.contents[0]} /></span>);
  default:
    console.error("Unknown renderConstraint", constraint);
    return "";
  }
}

function Scheme(props) {
  let {scheme} = props;
  switch (scheme.tag) {
  case "CRes":
    const [notes, [upper, lower, desc]] = scheme.contents;
    return <div>
             <Notes notes={notes}/>
             <Type data={upper}/> ⊇ {desc} ⊇ <Type data={lower}/>
           </div>;
  case "CErr":
    return <Notes notes={scheme.contents}/>;
  default:
    console.error("Unknown renderScheme", scheme);
    return "";
  }
}

function Pnt(props) {
  let {pnt} = props;
  return <Link to={`/constrain/${pnt}`}>{pnt}</Link>;
}

export default Constrain;
