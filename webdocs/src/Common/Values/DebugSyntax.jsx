import React, {createContext, useContext, useState} from 'react';

import Box from '@mui/material/Box';
import Link from '@mui/material/Link';
import {SimpleTreeView} from '@mui/x-tree-view/SimpleTreeView';
import ExpandMoreIcon from '@mui/icons-material/ExpandMore';
import ChevronRightIcon from '@mui/icons-material/ChevronRight';

import {RawExpr, RawObjArr} from '../Syntax';
import {InlineValue} from '../Value';
import {rawExprMeta, statementBody} from '../RawPrgm';

// A run read by the functions its pages write (the server's `drSyntax`, see `Eval.SyntaxTree`): a
// tree of calls of functions as written, each holding what each of the function's lines came to.
//
// The evaluation tree (`erTreebug`) is still what a line is broken down by. A line points at the
// nodes it was evaluated through, and those nodes are drawn by the Debugger's own `TreebugNode`,
// which stops where a node is a call or a line of its own (`stops` below) and links there instead of
// drawing it a second time.

// How the parts of a debugger reach each other: which node stands for a call or a line of one, what
// each node's expression came to, and how to go to a call or a line.
const DebugNavContext = createContext({stops: {}, resolved: {}, selectCall: () => {}, focusLine: () => {}});

// The name the compiler gives the body of a declaration written over several lines, which is written
// nowhere and is not shown (`CtConstants.nestedDeclaration`).
const NESTED_DECLARATION = "/Catln/nestedDeclaration";

// What a run's syntax says about each node and statement, gathered once for the whole run.
function syntaxIndex(drSyntax, drResult) {
  const calls = {};       // call node -> SyntaxCall
  const stops = {};       // node -> {call} for a call, {call, stmt} for a line of one
  const statements = {};  // statement id -> SyntaxStatement
  const parents = {};     // call node -> the call node it was made in

  (drSyntax?.srStatements ?? []).forEach(s => {statements[s.ssId] = s;});

  const walk = (call, parent) => {
    calls[call.scNode] = call;
    parents[call.scNode] = parent;
    stops[call.scNode] = {call: call.scNode};
    call.scLines.forEach(line => line.slRuns.forEach(run => {
      if (run.lrNode) {
        stops[run.lrNode] = {call: call.scNode, stmt: line.slStmt};
      }
    }));
    call.scCalls.forEach(c => walk(c, call.scNode));
  };
  (drSyntax?.srCalls ?? []).forEach(c => walk(c, null));

  // What each expression came to, keyed by the call it was written as: a node's own output is the
  // term its arrow rewrote to, which a caller then goes on rewriting (see `EvalExprRun`).
  const resolved = {};
  (drResult.erExprs ?? []).forEach(e => {
    if (e.exSteps.length) {
      resolved[e.exSteps[0]] = e.exValue;
    }
  });

  return {calls, stops, statements, parents, resolved};
}

// The calls of a run as the tree they were made in. Labelled as the evaluation tree labels a call --
// what it was given, and what it came to -- since the given value already names the function.
function CallItem(props) {
  const {call, nodes, TreeItem} = props;
  const node = nodes[call.scNode];
  const own = call.scLines.find(line => line.slStmt === call.scStmt);
  const value = own?.slRuns[0]?.lrValue;
  const label = (
    <span>
      {node ? <InlineValue data={node[1]} /> : call.scNode}
      {value ? <> =&gt; <InlineValue data={value} /></> : null}
    </span>
  );
  return (
    <TreeItem itemId={call.scNode} label={label}>
      {call.scCalls.map(c => <CallItem key={c.scNode} call={c} nodes={nodes} TreeItem={TreeItem} />)}
    </TreeItem>
  );
}

function CallTree(props) {
  const {calls, nodes, expanded, setExpanded, selected, setSelected, TreeItem} = props;
  return (
    <SimpleTreeView
      slots={{collapseIcon: ExpandMoreIcon, expandIcon: ChevronRightIcon}}
      expandedItems={expanded}
      onExpandedItemsChange={(event, items) => setExpanded(items)}
      selectedItems={selected}
      onSelectedItemsChange={(event, item) => item && setSelected(item)}
    >
      {calls.map(c => <CallItem key={c.scNode} call={c} nodes={nodes} TreeItem={TreeItem} />)}
    </SimpleTreeView>
  );
}

// The name an expression is written under, reading through what is applied to it.
function rawPath(expr) {
  switch (expr?.tag) {
  case "RawValue":        return expr.rvName;
  case "RawTupleApply":   return rawPath(expr.rtaBase[1]);
  case "RawVarsApply":    return rawPath(expr.rvaBase);
  case "RawContextApply": return rawPath(expr.rcaBase[1]);
  case "RawParen":        return rawPath(expr.rpnExpr);
  case "RawWhere":        return rawPath(expr.rwBase);
  case "RawMethod":       return rawPath(expr.rmValue);
  default:                return null;
  }
}

// The id a statement is named by, as the server names it (`Eval.SyntaxTree.statementId`).
function statementId(statement) {
  const body = statementBody(statement);
  if (statement.tag === "RawAnnot") {
    return rawExprMeta(body)?.getMetaId;
  }
  return body?.roaObj ? rawExprMeta(body.roaObj)?.getMetaId : null;
}

// A comment written inside a function, which is not evaluated and so has no value to show.
function isComment(statement) {
  return statement.tag === "RawAnnot" && (rawPath(statementBody(statement)) ?? "").endsWith("#md");
}

// A statement as it is written. A declaration written over several lines is shown as its head: the
// lines under it are its body, and the name the compiler gives that body is written nowhere.
function StatementText(props) {
  const {statement} = props;
  const body = statementBody(statement);
  switch (statement.tag) {
  case "RawDeclStatement": {
    const [arrExpr, arrType] = body.roaArr ?? [null, null];
    const roa = arrExpr && rawPath(arrExpr) === NESTED_DECLARATION ? {...body, roaArr: [null, arrType]} : body;
    return <RawObjArr roa={roa} />;
  }
  case "RawBindStatement":
    return <span><RawExpr expr={body.roaObj} /> &lt;- {body.roaArr?.[0] ? <RawExpr expr={body.roaArr[0]} /> : null}</span>;
  case "RawAnnot":
    return <RawExpr expr={body} />;
  default:
    return null;
  }
}

// What the runs of a line came to. A line evaluated the same way every time it was used says so once,
// with how many times; one that came to different things lists them.
function RunValues(props) {
  const {runs} = props;
  const shown = [];
  const counts = {};
  runs.forEach(run => {
    const key = JSON.stringify(run.lrValue);
    if (!(key in counts)) {
      counts[key] = 0;
      shown.push(run.lrValue);
    }
    counts[key] += 1;
  });
  return (
    <span>
      {shown.map((v, i) => (
        <span key={i}>
          {i ? " | " : null}
          {v ? <InlineValue data={v} /> : <em>no value recorded</em>}
          {counts[JSON.stringify(v)] > 1 ? <span className="catln-debug-count"> ×{counts[JSON.stringify(v)]}</span> : null}
        </span>
      ))}
    </span>
  );
}

// One statement of the function being shown, with what it came to in this call, and the lines under it.
//
// A line of the function with no run was not evaluated in this call: desugar evaluates a line where it
// is used, so an unused one never is. A statement that is no line of this function -- a helper nested
// in it, which takes parameters of its own and is a call of its own -- is shown without values, which
// are in its calls instead.
function StatementLine(props) {
  const {tree, call, lines, own, depth, InnerTree, focused} = props;
  const statement = tree.rstStatement;
  const sid = statementId(statement);
  const runs = lines[sid];
  const isLine = own.has(sid) && !isComment(statement);
  const [open, setOpen] = useState(false);
  const [runIndex, setRunIndex] = useState(0);
  const inner = runs?.[Math.min(runIndex, runs.length - 1)]?.lrInner ?? [];

  let value = null;
  if (runs?.length) {
    value = (
      <Link component="button" type="button" underline="hover" onClick={() => setOpen(!open)} sx={{marginLeft: '1em', verticalAlign: 'baseline'}}>
        =&gt; <RunValues runs={runs} />
      </Link>
    );
  } else if (isLine) {
    value = <em className="catln-debug-unevaluated" style={{marginLeft: '1em', opacity: 0.6}}>not evaluated</em>;
  }

  return (
    <div data-stmt={sid}>
      <Box sx={{
        opacity: own.has(sid) || sid === call.scStmt ? 1 : 0.55,
        background: focused === sid ? 'rgba(255, 213, 79, 0.35)' : undefined,
        fontFamily: 'monospace',
      }}>
        <StatementText statement={statement} />
        {value}
      </Box>
      {open && runs?.length ? (
        <Box sx={{marginLeft: '2em', borderLeft: '1px solid rgba(127, 127, 127, 0.4)', paddingLeft: '0.5em'}}>
          {runs.length > 1 ? (
            <div>
              {runs.map((r, i) => (
                <Link key={i} component="button" type="button" onClick={() => setRunIndex(i)} sx={{marginRight: '0.5em', fontWeight: i === runIndex ? 'bold' : 'normal'}}>
                  run {i + 1}
                </Link>
              ))}
            </div>
          ) : null}
          {inner.length ? <InnerTree roots={inner} /> : <p><em>Evaluated without calling anything.</em></p>}
        </Box>
      ) : null}
      <Box sx={{marginLeft: '2em'}}>
        {tree.rstChildren.map((child, i) => (
          <StatementLine key={i} tree={child} call={call} lines={lines} own={own} depth={depth + 1} InnerTree={InnerTree} focused={focused} />
        ))}
      </Box>
    </div>
  );
}

// The function a call is a call of, as it is written, with what each of its lines came to in the call.
function FunctionPanel(props) {
  const {call, statement, InnerTree, focused} = props;
  if (!statement) {
    return <p>The source of this function is not available.</p>;
  }
  const lines = {};
  call.scLines.forEach(line => {lines[line.slStmt] = line.slRuns;});
  const own = new Set(statement.ssLines ?? []);
  return (
    <div className="catln-debug-function">
      <div style={{opacity: 0.7}}>{statement.ssFile.split("/").pop()}</div>
      <StatementLine key={call.scNode} tree={statement.ssTree} call={call} lines={lines} own={own} depth={0} InnerTree={InnerTree} focused={focused} />
    </div>
  );
}

// A node of the evaluation tree that is a call or a line of its own, drawn as a link to it rather than
// as its own breakdown, which is where it is gone to.
function StopLabel(props) {
  const {stop, children} = props;
  const {selectCall, focusLine} = useContext(DebugNavContext);
  const go = event => {
    event.stopPropagation();
    if (stop.stmt) {
      focusLine(stop.call, stop.stmt);
    } else {
      selectCall(stop.call);
    }
  };
  return (
    <span>
      <Link component="button" type="button" onClick={go} title={stop.stmt ? "Go to the line" : "Go to the call"}>
        {stop.stmt ? "↑ line" : "→ call"}
      </Link>{" "}
      {children}
    </span>
  );
}

export {CallTree, DebugNavContext, FunctionPanel, StopLabel, syntaxIndex};
