import React, {useContext} from 'react';

import { makeStyles } from '@material-ui/core/styles';
import Grid from '@material-ui/core/Grid';
import Tooltip from '@material-ui/core/Tooltip';
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
  let {notes, data: [, prgm]} = props;
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
              <TraceEpochs Meta={Meta} />
            </Route>
          </Switch>
        </Grid>
      </Grid>
    </div>
  );
}

let VarMeta = (notesMap) => (props) => {
  const {withPos} = props;
  const [, pos, [pnt,]] = props.data;
  var style = {};

  let showPos;
  if(pos) {

    if(withPos) {
      showPos = <span>&nbsp;-&nbsp;<Pos pos={pos} /></span>;
    }

    if(posKey(pos) in notesMap) {
      style.background = 'lightCoral';
    }
  }

  if (!withPos) {
    style.fontSize = "xx-small";
  }

  return (
    <span style={style}>
      (
        <Pnt pnt={pnt} />
        {showPos}
      )
    </span>
  );
};

function TraceEpochs(props) {
  let { curMeta } = useParams();
  let {Meta} = props;
  let prgmName = useContext(PrgmNameContext);
  let apiResult = useApi(`/api/constrain/pnt/${curMeta}?prgmName=${prgmName}`);
  return (
    <div>
      <h2>Pnt {curMeta}</h2>
      <Loading status={apiResult}>
        <Traces traceData={apiResult.data} curMeta={curMeta} Meta={Meta} />
      </Loading>
    </div>
  );
}

function Traces(props) {
  const {traceData, curMeta, Meta} = props;
  const {tcEpochs, tcCons, tcAllObjs, tcInitial} = traceData;
  let showInitial;
  if (curMeta in tcInitial) {
    showInitial = (
      <div>
        <h3>Initial</h3>
        <Scheme scheme={tcInitial[curMeta]} />
      </div>
    );
  } else {
    console.log("Missing tcInitial", tcInitial);
  }
  let showTraces = tcEpochs.map((t, index) => {
    return (
      <div key={index}>
        <h3>{index}</h3>
        <Trace trace={t} Meta={Meta} tcAllObjs={tcAllObjs} />
      </div>
    );
  });
  let showCons = tcCons.map((c, index) => {
    return <div key={index}><Constraint constraint={c} Meta={Meta} tcAllObjs={tcAllObjs}/></div>;
  });
  return (
    <div>
      {showInitial}
      {showTraces}
      <br/>
      <h3>Constraints</h3>
      {showCons}
    </div>
  );
}

function Trace(props) {
  let {trace, Meta, tcAllObjs} = props;
  return trace.map((constraintPair, constraintIndex) => {
    let [constraint, updates] = constraintPair;

    if(updates.length === 0) {
      return null;
    }

    let showUpdates = updates.map((update, updateIndex) => <Scheme key={updateIndex} scheme={update[1]} />);

    return (
      <div key={constraintIndex}>
        <b><Constraint constraint={constraint} Meta={Meta} tcAllObjs={tcAllObjs} /></b>
        {showUpdates}
      </div>
    );
  });
}

function Constraint(props) {
  const {constraint, Meta, tcAllObjs} = props;
  const {conDat} = constraint;
  switch(conDat.tag) {
  case "EqualsKnown":
    return (<span><Meta data={conDat.contents[1]} withPos /> <COp i={conDat.contents[0]}>k==</COp> <Type data={conDat.contents[2]}/></span>);
  case "EqPoints":
    return (<span><Meta data={conDat.contents[1]} withPos /> <COp i={conDat.contents[0]}>p==</COp> <Meta data={conDat.contents[2]} withPos /></span>);
  case "BoundedByKnown":
    return (<span><Meta data={conDat.contents[1]} withPos /> <COp i={conDat.contents[0]}>⊆</COp> <Type data={conDat.contents[2]}/></span>);
  case "BoundedByObjs":
    return (<span><Meta data={conDat.contents[1]} withPos /> <COp i={conDat.contents[0]}>BoundedByObjs</COp> <Meta data={tcAllObjs}/></span>);
  case "NoReturnArg":
    return (<span><COp i={conDat.contents[0]}>NoReturnArg</COp> <Meta data={conDat.contents[1]} withPos /></span>);
  case "ArrowTo":
    return (<span><Meta data={conDat.contents[1]} withPos /> <COp i={conDat.contents[0]}>-&gt;</COp> <Meta data={conDat.contents[2]} withPos /></span>);
  case "PropEq":
    return (<span>(<Meta data={conDat.contents[1][0]} withPos />).<PartialKey data={conDat.contents[1][1].contents}/> <COp i={conDat.contents[0]}> ==</COp> <Meta data={conDat.contents[2]} withPos /></span>);
  case "AddArg":
    return (<span>(<Meta data={conDat.contents[1][0]} withPos />)(<PartialKey data={conDat.contents[1][1].contents}/>) <COp i={conDat.contents[0]}>arg==</COp> <Meta data={conDat.contents[2]} withPos /></span>);
  case "AddInferArg":
    return (<span>(<Meta data={conDat.contents[1]} withPos />)(?) <COp i={conDat.contents[0]}>iarg==</COp> <Meta data={conDat.contents[2]} withPos /></span>);
  case "SetArgMode":
    if (conDat.contents[1]) {
      return (<span>P(<Meta data={conDat.contents[2]} withPos />) <COp i={conDat.contents[0]}>ps==</COp> <Meta data={conDat.contents[3]} withPos /></span>);
    } else {
      return (<span>(<Meta data={conDat.contents[2]} withPos />).. <COp i={conDat.contents[0]}>ps==</COp> <Meta data={conDat.contents[3]} withPos /></span>);
    }
  case "ConWhere":
    return (<span><Meta data={conDat.contents[1]} withPos /> | <Meta data={conDat.contents[2]}/><COp i={conDat.contents[0]}> = </COp><Meta data={conDat.contents[3]} withPos /></span>);
  case "UnionOf":
    return (<span><COp i={conDat.contents[0]}>UnionOf</COp> <Meta data={conDat.contents[1]} withPos /></span>);
  default:
    console.error("Unknown renderConstraint dat", conDat);
    return "";
  }
}

function COp(props) {
  const {children, i} = props;
  return (
    <Tooltip title={i.toString()}>
      <span>{children}</span>
    </Tooltip>
  );
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
