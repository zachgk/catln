import React, {useContext, useMemo, useState} from 'react';

import makeStyles from '@mui/styles/makeStyles';
import Grid from '@mui/material/Grid2';
import Tooltip from '@mui/material/Tooltip';

import {tagJoin, Notes, Type, PartialKey, ReachesTree, PrgmNameContext} from '../Common';
import {ObjMap} from './ListProgram';
import {AnnotPage} from '../../DocsPage';

const useStyles = makeStyles({
  holdSideBySide: {
    maxWidth: '100vw'
  },
  sideBySide: {
    maxHeight: '100vh',
    overflow: 'auto'
  }
});

// Which point the trace pane is showing, and how to render a meta. Both are set by the TypeInfer
// page and read by the point page embedded in it, which is a separately compiled annotation and so
// reaches them by context rather than by props. A point page opened on its own gets neither: no
// page to re-target, and no notes to highlight against.
const CurMetaContext = React.createContext({curMeta: undefined, setCurMeta: () => {}});
const MetaContext = React.createContext(undefined);

function TypeInfer(props) {
  const classes = useStyles();
  const prgmName = useContext(PrgmNameContext);
  const [curMeta, setCurMeta] = useState(undefined);

  // The only field of Show/TypeInfer, so a compiled value always carries the program whose
  // inference it shows, together with the notes that typecheck produced. Nothing is fetched: a
  // point's trace is a second annotation about the same page, not a second route.
  const constrain = props["/constrain"];
  const [prgm, notes] = useMemo(() => JSON.parse(constrain.contents), [constrain]);

  // The meta of an expression the typecheck complained about is highlighted, so the renderer is
  // built from the notes. Memoised because it is a component identity: a fresh one every render
  // would remount every meta, including those of the point page embedded below.
  const Meta = useMemo(() => {
    var notesMap = {};
    notes.forEach(note => {
      if(note.id) {
        let k = note.id;
        if(k in notesMap) {
          notesMap[k].push(note);
        } else {
          notesMap[k] = [note];
        }
      }
    });
    return VarMeta(notesMap);
  }, [notes]);

  return (
    <CurMetaContext.Provider value={{curMeta, setCurMeta}}>
      <MetaContext.Provider value={Meta}>
        <Notes notes={notes} noPosOnly />
        <Grid container spacing={2} justify="center" className={classes.holdSideBySide}>
          <Grid size={{xs:6}} className={classes.sideBySide}>
            <ObjMap objMap={prgm[0]} Meta={Meta} showExprMetas />
          </Grid>
          <Grid size={{xs:6}} className={classes.sideBySide}>
            <TraceEpochsArea prgmName={prgmName} />
          </Grid>
        </Grid>
      </MetaContext.Provider>
    </CurMetaContext.Provider>
  );
}

let VarMeta = (notesMap) => (props) => {
  const {withPos} = props;
  const {getMetaID, getMetaPos, getMetaDat: [pnt,]} = props.data;
  var style = {};

  let showPos;
  if(getMetaPos) {

    if(withPos) {
      showPos = <span>&nbsp;-&nbsp;<Pos pos={getMetaPos} /></span>;
    }

    if(getMetaID in notesMap) {
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

// The trace of the selected point is a second annotation about the same page, so the pane is
// another AnnotPage rather than another fetch. Nothing is shown until a point is picked, because
// there is no trace to ask for until then.
function TraceEpochsArea(props) {
  const {prgmName} = props;
  let { curMeta } = useContext(CurMetaContext);

  if (curMeta === undefined) {
    return "";
  } else {
    return <AnnotPage prgmName={prgmName} annot={`#typeInferPnt(pnt= ${curMeta})`} />;
  }
}

// Shows how one point was solved. Rendered from the value of a #typeInferPnt annotation, either
// embedded in a TypeInfer page or on its own.
function TypeInferPnt(props) {
  const pnt = props["/pnt"].contents;
  const trace = props["/trace"];
  const traceData = useMemo(() => JSON.parse(trace.contents), [trace]);

  // The embedding page renders metas against its own notes; on its own there are none to
  // highlight. Memoised because a fresh component identity would remount the whole trace.
  const ctxMeta = useContext(MetaContext);
  const Meta = useMemo(() => ctxMeta || VarMeta({}), [ctxMeta]);

  return (
    <div>
      <h2>Pnt {pnt}</h2>
      <Traces traceData={traceData} Meta={Meta} pnt={pnt} />
    </div>
  );
}

function Traces(props) {
  const {traceData, Meta, pnt} = props;
  const {tcEpochs, tcCons, tcAllObjs, tcInitial} = traceData;
  let showInitial;
  if (pnt in tcInitial) {
    showInitial = (
      <div>
        <h3>Initial</h3>
        <Scheme scheme={tcInitial[pnt]} />
      </div>
    );
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
  const {conDat, conVaenv} = constraint;

  let showConVaenv;
  if (conVaenv.length > 0) {
    let vs = conVaenv.map((v, index) => <span key={index}><PartialKey data={v[0].contents}/> = (<Meta data={v[1][0]}/>, <Meta data={v[1][1]}/>)</span>);
    showConVaenv = <span>&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp; env({tagJoin(vs, ", ")})</span>;
  }
  return <span><ConDat conDat={conDat} Meta={Meta} tcAllObjs={tcAllObjs}/>{showConVaenv}</span>;
}

function ConDat(props) {
  const {conDat, Meta, tcAllObjs} = props;
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
    const [notes, {stypeAct, stypeReq, stypeTree, stypeDesc}] = scheme.contents;

    let showTree;
    if (stypeTree) {
      showTree = <div><ReachesTree tree={stypeTree}/></div>;
    }
    return <div>
             <Notes notes={notes}/>
             &#123;{stypeDesc} :: ACT <Type data={stypeAct}/>; REQ <Type data={stypeReq}/>&#125;
             {showTree}
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
  let { setCurMeta } = useContext(CurMetaContext);
  const link = () => {
    setCurMeta(pnt);
  };
  // TODO Consider replacing this with a new element such as btn without a link
  return <a href={`#pnt${pnt}`} onClick={link} >{pnt}</a>;
}

function Pos(props) {
  const {pos: [start, end]} = props;
  return `${start.name}: ${start.line}:${start.col} - ${end.line}:${end.col}`;
}

export default TypeInfer;
export {TypeInferPnt};
