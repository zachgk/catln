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

import {useApi, Loading, Type} from './Common';
import {ObjMap} from './ListProgram';

function Constrain() {
  let apiResult = useApi("/constrain");

  return (
    <Loading status={apiResult}>
      <Main data={apiResult.data} />
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
        <Grid container spacing={2} justify="center">
          <Grid item xs>
            <ObjMap objMap={prgm[0]} Meta={VarMeta}/>
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
  const m = props.data;
  return <span><Type data={m[1]}/>(<Pnt pnt={m[0]} />)</span>;
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
  return trace.map(constraintPair => {
    let [constraint, updates] = constraintPair;

    let filteredUpdates = updates.filter(update => update[0] === curMeta);
    if(filteredUpdates.length === 0) {
      return null;
    }

    let showUpdates = filteredUpdates.map(update => <Scheme scheme={update[1]} />);

    return (
      <div>
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
    return (<span><Pnt pnt={constraint.contents[0]}/> == <Type data={constraint.contents[1]}/></span>);
  case "EqPoints":
    return (<span><Pnt pnt={constraint.contents[0]}/> == <Pnt pnt={constraint.contents[1]}/></span>);
  case "BoundedByKnown":
    return (<span><Pnt pnt={constraint.contents[0]}/> ⊆ <Type data={constraint.contents[1]}/></span>);
  case "BoundedByObjs":
    return (<span>{constraint.contents[0]} <Pnt pnt={constraint.contents[1]}/></span>);
  case "ArrowTo":
    return (<span><Pnt pnt={constraint.contents[0]}/> -&gt; <Pnt pnt={constraint.contents[1]} /></span>);
  case "PropEq":
    return (<span>(<Pnt pnt={constraint.contents[0][0]}/>).{constraint.contents[0][1]} == <Pnt pnt={constraint.contents[1]}/></span>);
  case "VarEq":
    return (<span>(<Pnt pnt={constraint.contents[0][0]}/>).{constraint.contents[0][1]} == <Pnt pnt={constraint.contents[1]}/></span>);
  case "AddArg":
    return (<span>(<Pnt pnt={constraint.contents[0][0]}/>)({constraint.contents[0][1]}) == <Pnt pnt={constraint.contents[1]}/></span>);
  case "AddInferArg":
    return (<span>(<Pnt pnt={constraint.contents[0]}/>)(?) == <Pnt pnt={constraint.contents[1]}/></span>);
  case "PowersetTo":
    return (<span>P(<Pnt pnt={constraint.contents[0]}/>) == <Pnt pnt={constraint.contents[1]}/></span>);
  case "UnionOf":
    return (<span>UnionOf <Pnt pnt={constraint.contents[0]} /></span>);
  default:
    console.error("Unknown renderConstraint", constraint);
    return "";
  }
}

function Scheme(props) {
  let {scheme} = props;
  switch (scheme.tag) {
  case "TypeCheckResult":
    let stype = scheme.contents[1];

    switch(stype.tag) {
    case "SType":
      let [upper, lower, desc] = stype.contents;
      return <div><Type data={upper}/> ⊇ {desc} ⊇ <Type data={lower}/></div>;
    case "SVar":
      return <div>Var <Pnt pnt={stype.contents[1]} /></div>;
    default:
      console.error("Unknown stype");
      return "";
    }

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
