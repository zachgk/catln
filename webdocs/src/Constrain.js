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

import {useApi, Loading, Notes, Type} from './Common';
import {ObjMap} from './ListProgram';

function Constrain() {
  let apiResult = useApi("/constrain");

  return (
    <Loading status={apiResult}>
      <Main data={apiResult.data} notes={apiResult.notes} />
    </Loading>
  );
}

function Main(props) {
  let [, prgm, trace] = props.data;
  let { path } = useRouteMatch();
  return (
    <Switch>
      <Route exact path={path}>
        <Redirect to={`${path}/0`} />
      </Route>
      <Route path={`${path}/:curMeta`}>
        <Notes notes={props.notes}/>
        <Grid container spacing={2} justify="center">
          <Grid item xs>
            <ObjMap objMap={prgm[0]} Meta={VarMeta} showExprMetas={true}/>
          </Grid>
          <Grid item xs>
            <TraceEpochs trace={trace} />
          </Grid>
        </Grid>
      </Route>
    </Switch>
  );
}

function VarMeta(props) {
  const [pnt, [tp, ], ] = props.data;
  return <span><Type data={tp}/>(<Pnt pnt={pnt} />)</span>;
}

function TraceEpochs(props) {
  let { curMeta } = useParams();
  let {trace} = props;
  let traces = trace.map((t, index) => {
    return (
      <div key={index}>
        <h3>{index}</h3>
        <Trace trace={t} />
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
  let {trace} = props;
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
        <b><Constraint constraint={constraint}/></b>
        {showUpdates}
      </div>
    );
  });
}

function Constraint(props) {
  let {constraint} = props;
  switch(constraint.tag) {
  case "EqualsKnown":
    return (<span><VarMeta data={constraint.contents[0]}/> k== <Type data={constraint.contents[1]}/></span>);
  case "EqPoints":
    return (<span><VarMeta data={constraint.contents[0]}/> p== <VarMeta data={constraint.contents[1]}/></span>);
  case "BoundedByKnown":
    return (<span><VarMeta data={constraint.contents[0]}/> ⊆ <Type data={constraint.contents[1]}/></span>);
  case "BoundedByObjs":
    return (<span>{constraint.contents[0]} <VarMeta data={constraint.contents[1]}/></span>);
  case "ArrowTo":
    return (<span><VarMeta data={constraint.contents[0]}/> -&gt; <VarMeta data={constraint.contents[1]} /></span>);
  case "PropEq":
    return (<span>(<VarMeta data={constraint.contents[0][0]}/>).{constraint.contents[0][1]} == <VarMeta data={constraint.contents[1]}/></span>);
  case "VarEq":
    return (<span>(<VarMeta data={constraint.contents[0][0]}/>).{constraint.contents[0][1]} == <VarMeta data={constraint.contents[1]}/></span>);
  case "AddArg":
    return (<span>(<VarMeta data={constraint.contents[0][0]}/>)({constraint.contents[0][1]}) arg== <VarMeta data={constraint.contents[1]}/></span>);
  case "AddInferArg":
    return (<span>(<VarMeta data={constraint.contents[0]}/>)(?) iarg== <VarMeta data={constraint.contents[1]}/></span>);
  case "PowersetTo":
    return (<span>P(<VarMeta data={constraint.contents[0]}/>) ps== <VarMeta data={constraint.contents[1]}/></span>);
  case "UnionOf":
    return (<span>UnionOf <VarMeta data={constraint.contents[0]} /></span>);
  default:
    console.error("Unknown renderConstraint", constraint);
    return "";
  }
}

function Scheme(props) {
  let {scheme} = props;
  switch (scheme.tag) {
  case "TypeCheckResult":
    let [upper, lower, desc] = scheme.contents[1];
    return <div><Type data={upper}/> ⊇ {desc} ⊇ <Type data={lower}/></div>;
  case "TypeCheckResE":
    return <pre>{JSON.stringify(scheme.contents, null, 2)}</pre>;
  default:
    console.error("Unknown renderScheme");
    return "";
  }
}

function Pnt(props) {
  let {pnt} = props;
  return <Link to={`/constrain/${pnt}`}>{pnt}</Link>;
}

export default Constrain;
