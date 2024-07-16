import React, {useContext} from 'react';

import { makeStyles } from '@material-ui/core/styles';
import Grid from '@material-ui/core/Grid';
import {
  Switch,
  Route,
  Redirect,
  Link,
  useParams
} from 'react-router-dom';

import {useApi, posKey, Loading, Notes, Type, PartialKey} from './Common/Common';
import {ObjMap} from './ListProgram';

const useStyles = makeStyles({
  holdSideBySide: {
    maxWidth: '100vw'
  },
  sideBySide: {
    maxHeight: '100vh',
    overflow: 'auto'
  }
});

const PrgmNameContext = React.createContext(undefined);

function TypeInfer() {
  const { prgmName } = useParams();
  let apiResult = useApi(`/api/constrain?prgmName=${prgmName}`);

  return (
    <PrgmNameContext.Provider value={prgmName}>
      <Loading status={apiResult}>
        <Main data={apiResult.data} notes={apiResult.notes} />
      </Loading>
    </PrgmNameContext.Provider>
  );
}

function Main(props) {
  let {notes, data: [, prgm, trace]} = props;
  let prgmName = useContext(PrgmNameContext);
  const classes = useStyles();

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
      <Grid container spacing={2} justify="center" className={classes.holdSideBySide}>
        <Grid item xs className={classes.sideBySide}>
          <ObjMap objMap={prgm[0]} Meta={Meta} showExprMetas />
        </Grid>
        <Grid item xs className={classes.sideBySide}>
          <Switch>
            <Route exact path={`/typeinfer/${prgmName}`}>
              <Redirect to={`/typeinfer/${prgmName}/0`} />
            </Route>
            <Route path={`/typeinfer/${prgmName}/:curMeta`}>
              <TraceEpochs trace={trace} Meta={Meta} />
            </Route>
          </Switch>
        </Grid>
      </Grid>
    </div>
  );
}

let VarMeta = (notesMap) => (props) => {
  const [tp, pos, [pnt,]] = props.data;
  var style = {};

  let showPos;
  if(pos) {

    if(props.withPos) {
      showPos = <span>&nbsp;-&nbsp;<Pos pos={pos} /></span>;
    }

    if(posKey(pos) in notesMap) {
      style.background = 'lightCoral';
    }
  }

  return (
    <span style={style}>
      <Type data={tp}/>
      (
        <Pnt pnt={pnt} />
        {showPos}
      )
    </span>
  );
};

function TraceEpochs(props) {
  let { curMeta } = useParams();
  let {trace, Meta} = props;
  const forwardTrace = [].concat(trace).reverse(); // Trace defaults to reverse order
  let traces = forwardTrace.map((t, index) => {
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
  const forwardTrace = [].concat(trace).reverse();
  return forwardTrace.map((constraintPair, constraintIndex) => {
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
  const {constraint, Meta} = props;
  const {conDat} = constraint;
  switch(conDat.tag) {
  case "EqualsKnown":
    return (<span><Meta data={conDat.contents[1]} withPos /> k== <Type data={conDat.contents[2]}/></span>);
  case "EqPoints":
    return (<span><Meta data={conDat.contents[1]} withPos /> p== <Meta data={conDat.contents[2]} withPos /></span>);
  case "BoundedByKnown":
    return (<span><Meta data={conDat.contents[1]} withPos /> ⊆ <Type data={conDat.contents[2]}/></span>);
  case "BoundedByObjs":
    return (<span>BoundedByObjs <Meta data={conDat.contents[1]} withPos /></span>);
  case "ArrowTo":
    return (<span><Meta data={conDat.contents[1]} withPos /> -&gt; <Meta data={conDat.contents[2]} withPos /></span>);
  case "PropEq":
    return (<span>(<Meta data={conDat.contents[1][0]} withPos />).<PartialKey data={conDat.contents[1][1].contents}/> == <Meta data={conDat.contents[2]} withPos /></span>);
  case "VarEq":
    return (<span>(<Meta data={conDat.contents[1][0]} withPos />).{conDat.contents[1][1]} == <Meta data={conDat.contents[2]} withPos /></span>);
  case "AddArg":
    return (<span>(<Meta data={conDat.contents[1][0]} withPos />)(<PartialKey data={conDat.contents[1][1]}/>) arg== <Meta data={conDat.contents[2]} withPos /></span>);
  case "AddInferArg":
    return (<span>(<Meta data={conDat.contents[1]} withPos />)(?) iarg== <Meta data={conDat.contents[2]} withPos /></span>);
  case "SetArgMode":
    return (<span>P(<Meta data={conDat.contents[1]} withPos />) ps== <Meta data={conDat.contents[2]} withPos /></span>);
  case "UnionOf":
    return (<span>UnionOf <Meta data={conDat.contents[1]} withPos /></span>);
  default:
    console.error("Unknown renderConstraint dat", conDat);
    return "";
  }
}

function Scheme(props) {
  let {scheme} = props;
  switch (scheme.tag) {
  case "CRes":
    const [notes, {stypeAct, stypeReq, stypeDesc}] = scheme.contents;
    return <div>
             <Notes notes={notes}/>
             &#123;{stypeDesc} :: ACT <Type data={stypeAct}/>; REQ <Type data={stypeReq}/>&#125;
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
  let prgmName = useContext(PrgmNameContext);
  return <Link to={`/typeinfer/${prgmName}/${pnt}`}>{pnt}</Link>;
}

function Pos(props) {
  const {pos: [start, end, label]} = props;
  return `${start.name}: ${start.line}:${start.col} - ${end.line}:${end.col} ${label}`;
}

export default TypeInfer;
