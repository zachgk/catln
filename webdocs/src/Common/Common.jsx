import React, {useState, useEffect} from 'react';

import { styled } from '@mui/material/styles';
import LinearProgress from '@mui/material/LinearProgress';
import Typography from '@mui/material/Typography';
import {SimpleTreeView} from '@mui/x-tree-view/SimpleTreeView';
import ExpandMoreIcon from '@mui/icons-material/ExpandMore';
import ChevronRightIcon from '@mui/icons-material/ChevronRight';
import {TreeItem} from '@mui/x-tree-view/TreeItem';
import makeStyles from '@mui/styles/makeStyles';
import {useLocation, Link} from 'react-router-dom';


function tagJoin(lst, joinWith) {
  return lst.reduce((acc, x) => acc == null ? [x] : <>{acc}{joinWith}{x}</>, null);
}

const ErrorPre = styled('pre')(({ theme }) => ({
  whiteSpace: 'pre-wrap',
  background: theme.palette.error.main,
}));

const WarningPre = styled('pre')(({ theme }) => ({
  whiteSpace: 'pre-wrap',
  background: theme.palette.warning.main,
}));

const useStyles = makeStyles({
  partialNameTp: {
    color: 'green'
  },
  partialNameClass: {
    color: 'purple'
  },
  partialNameRelative: {
    color: 'black'
  },
  keyword: {
    fontWeight: 'bold'
  },
  typevar: {
    fontWeight: 'bold'
  }
});

const ConfigContext = React.createContext({});

// The page an annotation is being evaluated with respect to. A Value is rendered from deep inside
// RawExpr, which threads no page name, so a value that wants to ask the compiler a follow-up
// question about the same page reads it from here. Empty means the evaluation was not scoped to a
// page, which is what an annotation about the whole program wants.
const PrgmNameContext = React.createContext("");

// The directory the page being shown was imported relative to, which is what a relative link in its
// documentation resolves against. It arrives with the page, so a value shown outside one -- an
// annotation, an editor's view zone -- has no directory, and such a link is left as it was written.
const PageDirContext = React.createContext("");

// Asks whatever is displaying a value to read itself again. A value that writes to the compiler is
// showing what the compiler answered with, so after a write it is showing the answer to the question
// before it; the display refetches rather than the value patching itself, so what is on screen is
// always what the files now say. The default does nothing, for a value displayed by something with
// nothing to refetch.
const ReloadContext = React.createContext(() => {});

function useQuery() {
  return new URLSearchParams(useLocation().search);
}

// What an answer from the compiler means, whether it was asked for once or arrived down a stream of
// them. Shared so that a display which follows its page and one which asks about it once cannot read
// the same answer differently.
function apiResult(res) {
  switch(res.tag) {
  case "Success":
    return {isLoaded: true, data: res.contents[0], notes: res.contents[1]};
  case "ResFail":
    return {isLoaded: true, notes: res.contents};
  default:
    return {isLoaded: true, notes: [], error: "Unknown result type"};
  }
}

const emptyResult = {error: null, isLoaded: false, data: null, notes: []};

// reloadKey is optional. Changing it refetches the same path, which is how a save
// picks up the rebuilt page.
function useApi(path, reloadKey) {
  const [result, setResult] = useState(emptyResult);
  useEffect(() => {
    // A falsy path means the caller already has its data (such as a value that carries it) and
    // has nothing to fetch.
    if(!path) {
      return;
    }
    // The answer to a question no longer being asked is dropped rather than shown. Asking again
    // does not cancel what was already asked, and a question asked earlier can be answered later —
    // a search re-asked at each keystroke asks the compiler to do more work for a shorter query
    // than for a longer one — so without this a reader would be shown the results of what they had
    // typed a moment ago.
    let asked = true;
    fetch(path)
      .then(res => {
        if(res.status !== 200) {
          throw new Error(res.statusText);
        } else {
          return res;
        }
      })
      .then(res => res.json())
      .then(
        (res) => { if(asked) { setResult(apiResult(res)); } },
        // Note: it's important to handle errors here
        // instead of a catch() block so that we don't swallow
        // exceptions from actual bugs in components.
        (error) => {
          if(asked) {
            setResult({
              isLoaded: true,
              error
            });
          }
        }
      );
    return () => { asked = false; };
  }, [path, reloadKey]);
  return result;
}

// A value as it was a moment ago, once it has stopped changing for that long.
//
// What is asked of the server should be what a reader is asking, not each step of them typing it: a
// reader types a letter at a time and is looking for a word, so a question per letter is a question
// per answer nobody wanted. Waiting for the typing to stop is what makes the word the question.
function useDebounced(value, ms) {
  const [settled, setSettled] = useState(value);
  useEffect(() => {
    const timer = setTimeout(() => setSettled(value), ms);
    return () => clearTimeout(timer);
  }, [value, ms]);
  return settled;
}

// The same as useApi, but asked to keep answering: the server holds the question open and answers it
// again whenever a file it was compiled from is written to.
//
// Which changes are worth answering again is the server's decision, not this one's. What a page is
// compiled from is the whole graph of what it imports, so a write low in the standard library reaches
// nearly every page there is — asking the server to say what changed would be asking it for a message
// about the program in order to learn about the one page here. So the URL is the question, the server
// keeps what it is a question about, and this only ever hears about this page.
//
// The previous answer stays on screen until the next one arrives, so a change to a page costs the
// reader the wait for it to compile rather than the page going blank.
function useLiveApi(path, reloadKey) {
  const [result, setResult] = useState(emptyResult);
  useEffect(() => {
    if(!path) {
      return;
    }
    const source = new EventSource(path);
    source.onmessage = e => setResult(apiResult(JSON.parse(e.data)));
    // A connection that drops is one the browser opens again by itself, and what comes back is the
    // page as it now is. So a display with something on screen keeps it: a blink of "Error:" in
    // place of the page, for a connection which is about to come back, is worse for the reader than
    // the page being a moment out of date. A display which never got an answer has nothing to keep
    // and says so, since a doc server that never answers is not something to wait at a blank page
    // for.
    source.onerror = () => setResult(r => r.isLoaded ? r
      : {...r, isLoaded: true, error: new Error("The doc server is not answering")});
    // Closed when the display goes, which is what takes it off the list of what the server is
    // following: a question left open is one the server answers again on every change, for nobody.
    return () => source.close();
  }, [path, reloadKey]);
  return result;
}

// What the compiler is working on, as one connection for the whole app.
//
// A browser allows six connections to a server and the app already holds two of them open — the
// navigation follows its own annotation and a page follows itself (see AnnotPage in DocsPage.jsx).
// A stream per waiting display would be the app running out of connections to fetch the very page it
// is telling the reader it is waiting for. It is also one question with one answer: the compiler
// builds one component at a time behind one lock, so what it is working on is what every display
// waiting on it is waiting for.
const PROGRESS_PATH = '/api/progress';

// How long a display waits before asking what is being done about it.
//
// A page that arrives at once should arrive at once rather than flash a bar on its way, and a wait
// too short to read is a wait not worth reporting. It is also what keeps a display that was never
// really waiting from opening a connection at all.
const PROGRESS_DELAY = 500;

let progressSource = null;
let progressLast = null;
const progressListeners = new Set();

function progressSubscribe(listener) {
  progressListeners.add(listener);
  if(progressSource) {
    return;
  }
  progressSource = new EventSource(PROGRESS_PATH);
  progressSource.onmessage = e => {
    // Whatever arrives here is shown to a reader who is already waiting, so a frame that cannot be
    // read is passed over rather than allowed to take the display down with it.
    let now = null;
    try {
      const res = JSON.parse(e.data);
      const p = res.tag === "Success" ? res.contents[0] : null;
      if(p && typeof p.prDone === 'number' && typeof p.prTotal === 'number') {
        now = p;
      }
    } catch(err) {
      return;
    }
    progressLast = now;
    progressListeners.forEach(l => l(now));
  };
  // Closed rather than left for the browser to open again, which is the opposite of what a display
  // following its page does: there, what comes back is the page as it now is and is worth waiting
  // for, while a bar is worth nothing to reopen a connection for. The reader is left waiting at an
  // indeterminate bar, which is what they had before there was anything to say. A display that waits
  // later opens one again, so a doc server that has come back is picked up without this having to
  // watch for it.
  progressSource.onerror = () => {
    progressSource.close();
    progressSource = null;
    progressLast = null;
    progressListeners.forEach(l => l(null));
  };
}

function progressUnsubscribe(listener) {
  progressListeners.delete(listener);
  if(progressListeners.size === 0 && progressSource) {
    progressSource.close();
    progressSource = null;
    // Dropped with the connection, so that the next display to wait is not shown what the compiler
    // was doing the last time somebody waited.
    progressLast = null;
  }
}

// What the compiler says it is doing, for a display that has been waiting long enough to say so.
function useProgress() {
  const [progress, setProgress] = useState(progressLast);
  useEffect(() => {
    const timer = setTimeout(() => progressSubscribe(setProgress), PROGRESS_DELAY);
    return () => {
      clearTimeout(timer);
      progressUnsubscribe(setProgress);
    };
  }, []);
  return progress;
}

// What a step of the compiler's says about itself, as a line to read.
function progressText(progress) {
  const {prStep, prDone, prTotal, prAt} = progress;
  const at = prAt ? ` ${prAt}` : '';
  switch(prStep) {
  case "StepReading":
    return `Read ${prDone} of ${prTotal} pages`;
  case "StepBuilding":
    return `Compiled ${prDone} of ${prTotal} pages`;
  case "StepTypechecking":
    return `Type checking${at}`;
  case "StepEvaluating":
    return `Evaluating${at}`;
  default:
    return '';
  }
}

// How far the compiler has got with what is being waited for.
//
// The bar is only determinate while there is something left to count. A step with no count of its
// own — an evaluation, which is a program the reader wrote — has no honest fraction to show, and a
// count which has reached its total is a step that has FINISHED rather than a wait that is over: the
// compiler can start a build from inside an evaluation, which leaves the pages it built standing at
// their total while the evaluation goes on. Either reads as still working, which is what it is.
function LoadingProgress() {
  const progress = useProgress();
  const working = progress && progress.prWorking;
  const counted = working && progress.prTotal > 0 && progress.prDone < progress.prTotal;
  return (
    <>
      <LinearProgress
        variant={counted ? "determinate" : "indeterminate"}
        value={counted ? 100 * progress.prDone / progress.prTotal : undefined}
      />
      {working && (
        // Broken anywhere rather than at spaces, because what it is mostly made of is a path. The
        // navigation is a narrow column, and a path which will not break is one clipped at its edge
        // — which cuts off the end, and the end of a path is the part that says which file it is.
        <Typography
          variant="caption"
          color="textSecondary"
          component="div"
          sx={{overflowWrap: 'anywhere'}}
          data-testid="loading-progress"
        >
          {progressText(progress)}
        </Typography>
      )}
    </>
  );
}

function Loading(props) {
  const {error, isLoaded, data, notes} = props.status;

  if (error) {
    return <div>Error: {error.message}</div>;
  } else if (!isLoaded) {
    // The words stay the sole contents of their own element. What a display says while it waits is
    // asserted against by name throughout the suites, and text put beside them here would be read as
    // part of them.
    return <div><div>Loading...</div><LoadingProgress /></div>;
  } else if (!data){
    return <div><Notes notes={notes} /></div>;
  } else if (notes && notes.length > 0) {
    // A request can succeed and still report failures: an annotation that could not be evaluated
    // becomes an error note and the rest of the response is returned without it. Showing the notes
    // above what did arrive is what keeps that partial answer from reading as a complete one.
    return <div><Notes notes={notes} />{props.children}</div>;
  } else {
    return props.children;
  }
}

function Notes(props) {
  let {notes, noPosOnly} = props;

  if(noPosOnly) {
    notes = notes.filter(note => !note.pos);
  }

  return (
    <div>
      {notes.map((note, noteIndex) => <Note key={noteIndex} note={note} />)}
    </div>
  );
}

function Note(props) {
  let note = props.note;

  switch(note.tp) {
  case "CNoteError":
    return <ErrorPre>{note.msg}</ErrorPre>;
  case "CNoteWarning":
    return <WarningPre>{note.msg}</WarningPre>;
  default:
    console.error("Unknown note type", note);
  }
}

function PartialType(props) {
  const {ptName, ptVars, ptArgs, ptPreds, ptArgMode} = props.data;

  let showVars = "";
  if(Object.keys(ptVars).length > 0) {
    showVars = (
      <span>
        [
        {tagJoin(ptVars.map((v, index) => <span key={index}><PartialKey data={v[0]}/><TypeWithPrefix prefix=": " data={v[1]}/></span>), ", ")}
        ]
      </span>
    );
  }

  let showArgs = "";
  if(Object.keys(ptArgs).length > 0) {
    showArgs = (
        <span>
        (
          {tagJoin(ptArgs.map((v, index) => <span key={index}><PartialKey data={v[0]}/><TypeWithPrefix prefix=": " data={v[1]}/></span>), ", ")}
        )
      </span>
    );
  }

  // showPtArgMode: (lb, ub) is (T,T) exactly the listed args, (T,F) extras allowed,
  // (F,T) any subset, (F,F) unconstrained.
  let showArgMode = "";
  if (!ptArgMode.ptArgLb) {
    showArgMode = ptArgMode.ptArgUb ? "??" : "..??";
  } else if (!ptArgMode.ptArgUb) {
    showArgMode = "..";
  }

  let showPreds;
  if (isPredsNone(ptPreds)) {
    showPreds = "";
  } else if (ptPreds.tag === "PredsOne") {
    showPreds = <span> | <TypePredicate data={ptPreds.contents}/></span>;
  } else {
    showPreds = <span> | (<TypePredicates data={ptPreds}/>)</span>;
  }

  return (
    <span><PTypeName name={ptName}/>{showVars}{showArgs}{showArgMode}{showPreds}</span>
  );
}

function PartialKey(props) {
  const {pkName, pkArgs, pkVars} = props.data;
  if (pkArgs.length > 0 || pkVars.length > 0) {
    return "PartialKeyWith";
  }
  return <PTypeName name={pkName} />;
}

function TypePredicate(props) {
  const {data} = props;
  switch(data.tag) {
  case "PredClass":
    return <span>Class[{<PartialType data={data.contents}/>}]</span>;
  case "PredRel":
    return <span>Rel[{<PartialType data={data.contents}/>}]</span>;
  case "PredExpr":
    return <PartialType data={data.contents}/>;
  default:
    console.error("Unknown TypePredicate", data);
    return "???";
  }
}

function TypePredicates(props) {
  const {data} = props;
  switch(data.tag) {
  case "PredsOne":
    return <TypePredicate data={data.contents}/>;
  case "PredsAnd":
    return tagJoin(data.contents.map((ps, index) => <TypePredicates key={index} data={ps}/>), " && ");
  case "PredsNot":
    return <span>!<TypePredicate data={data.contents}/></span>;
  default:
    console.error("Unknown TypePredicates", data);
    return "???";
  }
}

// PredsNone is the empty conjunction.
function isPredsNone(preds) {
  return preds.tag === "PredsAnd" && preds.contents.length === 0;
}

// tryPredsToList flattens a conjunction into its predicates, or gives null for any negation.
function tryPredsToList(preds) {
  switch(preds.tag) {
  case "PredsOne":
    return [preds.contents];
  case "PredsAnd": {
    let out = [];
    for(const sub of preds.contents) {
      const subList = tryPredsToList(sub);
      if(subList === null) return null;
      out = out.concat(subList);
    }
    return out;
  }
  default:
    return null;
  }
}

// The partials of a PartialLeafs map: a name maps to each option's [vars, args, preds, argMode].
function partialLeafs(leafs) {
  let partials = [];
  Object.entries(leafs).forEach(([partialName, options]) => {
    options.forEach((partialData, partialIndex) => {
      let [partialVars, partialArgs, preds, argMode] = partialData;
      let ptData = {ptName: partialName, ptVars: partialVars, ptArgs: partialArgs, ptPreds: preds, ptArgMode: argMode};
      partials.push(<PartialType key={`${partialName}/${partialIndex}`} data={ptData}/>);
    });
  });
  return partials;
}

// showJoinParts: no parts is the empty set, one shows bare, several are parenthesised.
function joinParts(parts) {
  if(parts.length === 0) return "∅";
  if(parts.length === 1) return parts[0];
  return <span>({tagJoin(parts, " + ")})</span>;
}

// Mirrors showTypeD's TypeVar cases. The aux is TVVar or TVArg over a PartialKey, or TVApplied
// wrapping another aux with per-key overrides; the location is TVInt (internal), TVParent
// (nearest enclosing partial), or TVExt (ambient object scope). The third component is the
// occurrence's inner knowledge (read as var ∩ inner), rendered as a ∩{…} suffix when non-top.
function TypeVarType(props) {
  const [aux, loc, inner] = props.data;
  const innerSfx = (inner === undefined || inner === null || isTopType(inner))
    ? null : <span>∩{"{"}<Type data={inner}/>{"}"}</span>;
  switch(aux.tag) {
  case "TVVar":
    return <TypeVar>{loc === "TVExt" ? "$_" : (loc === "TVParent" ? "$^" : "$")}<PartialKey data={aux.contents}/>{innerSfx}</TypeVar>;
  case "TVArg":
    return <TypeVar>{loc === "TVExt" ? "TVArg(ext) " : (loc === "TVParent" ? "TVArg(parent) " : "TVArg ")}<PartialKey data={aux.contents}/>{innerSfx}</TypeVar>;
  case "TVApplied": {
    const [base, overrides] = aux.contents;
    const showOverrides = tagJoin(overrides.map((ovr, index) =>
      <span key={index}><PartialKey data={ovr[0]}/>=<Type data={ovr[1]}/></span>), ", ");
    return <span><TypeVarType data={[base, loc, null]}/>[{showOverrides}]{innerSfx}</span>;
  }
  default:
    console.error("Unknown TypeVarAux", aux);
    return "???";
  }
}

function Constant(props) {
  const {data} = props;
  switch(data.tag) {
  case "CStr":
    return <span>{JSON.stringify(data.contents)}</span>;
  case "CChar":
    return <span>{`'${data.contents}'`}</span>;
  case "CInt":
  case "CFloat":
    return <span>{String(data.contents)}</span>;
  default:
    console.error("Unknown Constant", data);
    return "???";
  }
}

// PTopType: the unconstrained top, with no predicates, negative partials, partials or constants.
function isTopType(t) {
  if (t.tag !== "UnionType") return false;

  const [top, leafs, consts] = t.contents;
  if (top === null) return false;

  const [preds, negPartials] = top;
  return isPredsNone(preds)
    && Object.keys(negPartials).length === 0
    && Object.keys(leafs).length === 0
    && consts.length === 0;
}

function TypeWithPrefix(props) {
  const {prefix, data} = props;
  if (isTopType(data)) {
    return "";
  }
  return <span>{prefix}<Type data={data}/></span>;
}

// Mirrors showTypeD in src/Semantics/Types.hs. A Type is
// UnionType (Maybe (TypePredicates, PartialLeafs)) PartialLeafs [Constant]: an optional top part
// (Top narrowed by predicates, less its negative partials), the positive partials, and the
// constants. There is no separate TopType constructor.
function Type(props) {
  let t = props.data;
  switch(t.tag) {
  case "TypeVar":
    return <TypeVarType data={t.contents}/>;
  case "UnionType": {
    const [top, leafs, consts] = t.contents;

    if (top === null && consts.length === 0) {
      return joinParts(partialLeafs(leafs));
    }

    if (top !== null && Object.keys(leafs).length === 0 && consts.length === 0) {
      const [preds, negPartials] = top;

      let showTop;
      const predList = tryPredsToList(preds);
      if (predList !== null && predList.length === 1 && predList[0].tag === "PredClass") {
        showTop = <span>∀<PartialType data={predList[0].contents}/></span>;
      } else if (predList !== null && predList.length === 1 && predList[0].tag === "PredRel") {
        showTop = <span>~<PartialType data={predList[0].contents}/></span>;
      } else if (predList !== null && predList.length === 0) {
        // A bare top says nothing about the value, so the webdocs leave it out entirely.
        if (Object.keys(negPartials).length === 0) {
          return "";
        }
        showTop = "TopType";
      } else {
        showTop = <span>(TopType | <TypePredicates data={preds}/>)</span>;
      }

      let showNeg;
      if (Object.keys(negPartials).length !== 0) {
        showNeg = <span> - {joinParts(partialLeafs(negPartials))}</span>;
      }
      return <span>{showTop}{showNeg}</span>;
    }

    // The mixed forms have no shorthand; showTypeD spells the three components out.
    const showTopPart = top === null
      ? "Nothing"
      : <span>Just(<TypePredicates data={top[0]}/>, {joinParts(partialLeafs(top[1]))})</span>;
    const showConsts = tagJoin(consts.map((c, index) => <Constant key={index} data={c}/>), ", ");
    return (
      <span>UnionType({showTopPart}, {joinParts(partialLeafs(leafs))}, [{showConsts}])</span>
    );
  }
  default:
    console.error("Unknown type", t);
    return "";
  }
}

function KeyWord(props) {
  const classes = useStyles();
  return <span className={classes.keyword}>{props.children}</span>;
}

function TypeVar(props) {
  const classes = useStyles();
  return <span className={classes.typevar}>{props.children}</span>;
}

function PTypeName(props) {
  return <PartialName name={{tag: "PTypeName", contents: props.name}}/>;
}

function PClassName(props) {
  return <PartialName name={{tag: "PClassName", contents: props.name}}/>;
}

// Links to a page rendered by evaluating an annotation, rather than to a route that fetches for
// itself. Naming a page scopes the evaluation to it, which is how an annotation that reports on
// "the program it is in" is pointed at a particular one.
function annotLink(annot, prgmName) {
  const scope = prgmName ? `&prgmName=${encodeURIComponent(prgmName)}` : "";
  return `/annot?annot=${encodeURIComponent(annot)}${scope}`;
}

// The call that evaluates an annotation, which is what an annotLink route resolves to. It scopes
// the same way: naming a page compiles the annotation as though it were written there, and no name
// compiles it against every loaded file.
function annotApi(annot, prgmName) {
  const scope = prgmName ? `&prgmName=${encodeURIComponent(prgmName)}` : "";
  return `/api/annot?annot=${encodeURIComponent(annot)}${scope}`;
}

// The call that evaluates a function of the program, named, with what to compute over sent as its
// arguments. This is what a display driven by what a reader is DOING asks for: an annotation says
// what it is about when it is written, so it can only carry what was written with it, while a call
// carries whatever has been typed or picked since.
//
// The arguments are sent as JSON rather than written into the annotation source the way #type and
// #debugger name theirs. What a reader types is not source, and quoting it into some would make a
// query holding a quote into a parse error rather than a search that finds nothing.
//
// Scoped like annotApi: naming a page calls the function as though it were written there, which is
// what keeps a call from compiling every page there is.
function callApi(fun, args, prgmName) {
  const scope = prgmName ? `&prgmName=${encodeURIComponent(prgmName)}` : "";
  return `/api/call?fun=${encodeURIComponent(fun)}&args=${encodeURIComponent(JSON.stringify(args))}${scope}`;
}

// Replaces one expression of a page and saves the page back as the file it was read from. The page
// is named per call rather than being the page doing the writing: a task shown in a list of every
// note's tasks is written back to the note it was read from, which is not the page the list is on.
//
// The server writes the file in whatever syntax the page is written in, so checking a task of an
// Obsidian note writes markdown without the caller having to know that it is markdown. Answers with
// what to report rather than throwing, since a failed save is something to show beside the value
// rather than an error to escape into the app.
async function writeExpr(prgmName, metaID, src) {
  try {
    const res = await fetch(`/api/write-expr?prgmName=${encodeURIComponent(prgmName)}&metaId=${metaID}`, {
      method: 'POST',
      headers: {'Content-Type': 'text/plain'},
      body: src
    });
    const json = await res.json();
    if (res.status === 200 && json.tag === 'Success') {
      return {ok: true};
    }
    const notes = json.tag === 'ResFail' ? json.contents : null;
    return {ok: false, message: notes?.map(n => n.msg).join('; ') || res.statusText || 'unknown error'};
  } catch (e) {
    return {ok: false, message: e.message};
  }
}

// A type or class page is a #type annotation, so linking to one means asking the compiler to
// evaluate that annotation rather than routing to a page that fetches for itself.
function typeAnnotLink(name) {
  return annotLink(`#type(name= "${name}")`);
}

function PartialName(props) {
  const {name} = props;
  const classes = useStyles();

  let cls, link;
  switch(name.tag) {
  case "PRelativeName":
    cls = classes.partialNameRelative;
    link = `/relative/${encodeURIComponent(name.contents)}`;
    break;
  case "PTypeName":
    cls = classes.partialNameTp;
    link = typeAnnotLink(name.contents);
    break;
  case "PClassName":
    cls = classes.partialNameClass;
    link = typeAnnotLink(name.contents);
    break;
  default:
    console.error("Unknown partial name", name);
  }
  return <Link to={link} className={cls}>{name.contents}</Link>;
}

// The values of an evaluated `/Data/List`, which is a `/Data/List/Cons` chain ending in a `/Data/List/Nil`.
//
// A tail which is neither is carried through as an entry rather than ending the walk, so that it is
// shown. A result quietly missing its end reads as a result that ended there, which is the one
// thing a display of it must not say.
//
// Here rather than beside either of the two displays that walk a list, because both do: a build
// result is a list of files and a JSON array is a list of values, and neither is the other. It
// cannot live in `Value.jsx`, which `BuildResult.jsx` imports from — that would be a cycle — and
// this module imports no display at all.
function valList(val) {
  const items = [];
  let rest = val;
  while (rest && rest.tag === 'TupleVal' && rest.name === '/Data/List/Cons') {
    items.push(rest.args['/head']);
    rest = rest.args['/tail'];
  }
  if (!(rest && rest.tag === 'TupleVal' && rest.name === '/Data/List/Nil')) {
    items.push(rest);
  }
  return items;
}

function ReachesTree(props) {
  const {tree} = props;
  return (
      <SimpleTreeView slots={{collapseIcon: ExpandMoreIcon, expandIcon: ChevronRightIcon}}>
        <ReachesTreeNode tree={tree} id={"0"} />
      </SimpleTreeView>
  );
}

function ReachesTreeNode(props) {
  const {tree, id} = props;
  switch(tree.tag) {
  case "ReachesPartialTree":
    return tree.contents.map((node, nodeIndex) => {
      let lab = <PartialType data={node[0]}/>;
      let showSubNode;
      if (node[1].tag !== "ReachesLeaf") {
        showSubNode = <ReachesTreeNode id={id + nodeIndex + "-"} tree={node[1]}/>;
      }
      return (
        <TreeItem itemId={id + nodeIndex} label={lab}>
          {showSubNode}
        </TreeItem>
      );
    });
  case "ReachesTypeTree":
    return tree.contents.map((node, nodeIndex) => {
      let lab = <span><Type data={node[0]}/> by {node[1][0]}</span>;
      let subNodeDat = node[1][1];
      if (node[0].tag === "UnionType" && subNodeDat.tag === "ReachesPartialTree" && subNodeDat.contents.length === 1) {
        subNodeDat = subNodeDat.contents[0][1];
      }
      let showSubNode;
      if (subNodeDat.tag !== "ReachesLeaf") {
        showSubNode = <ReachesTreeNode id={id + nodeIndex + "-"} tree={subNodeDat}/>;
      }
      return (
        <TreeItem itemId={id + nodeIndex} label={lab}>
          {showSubNode}
        </TreeItem>
      );
    });
  case "ReachesLeaf":
    return (
      <TreeItem itemId={id} label={"ReachesLeaf"}>
      </TreeItem>
    );
  default:
    console.error("Unknown ReachesTreeNode", tree);
    return (
      <TreeItem itemId={id} label={"TREE"}>
      </TreeItem>
    );
  }
}

export {
  ConfigContext,
  PrgmNameContext,
  PageDirContext,
  ReloadContext,
  annotLink,
  annotApi,
  callApi,
  writeExpr,
  tagJoin,
  useQuery,
  useApi,
  useLiveApi,
  useDebounced,
  useProgress,
  Loading,
  LoadingProgress,
  Notes,
  PartialKey,
  PartialType,
  isTopType,
  Type,
  KeyWord,
  TypeVar,
  PTypeName,
  PClassName,
  PartialName,
  ReachesTree,
  valList
};
