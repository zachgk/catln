import React from 'react';

import {RawExpr, RawObjArr} from '../Syntax';
import {InlineValue} from '../Value';
import {rawExprMeta, statementBody} from '../RawPrgm';

// A run read by the functions its pages write (the server's `drSyntax`, see `Eval.SyntaxTree`), laid
// out as the one tree the Debugger shows.
//
// Every call of a function is shown as the function is written, a statement per line with what the
// line came to in the call. A line opens onto the steps it was evaluated through -- the evaluation
// tree (`erTreebug`) below it -- and where one of those steps is a call of a function, that function
// is shown there in the same way, so the tree reads from the function called down through the lines
// of each function it reached. A step that is a line of a function already shown leads back to that
// line rather than being shown twice.

// The name the compiler gives the body of a declaration written over several lines, which is written
// nowhere and is not shown (`CtConstants.nestedDeclaration`).
const NESTED_DECLARATION = "/Catln/nestedDeclaration";

// What a run's syntax says about each node and statement, gathered once for the whole run.
function syntaxIndex(drSyntax, drResult) {
  const calls = {};       // call node -> SyntaxCall
  const stops = {};       // node -> {call} for a call, {call, stmt} for a line of one
  const statements = {};  // statement id -> SyntaxStatement

  (drSyntax?.srStatements ?? []).forEach(s => {statements[s.ssId] = s;});

  const walk = call => {
    calls[call.scNode] = call;
    stops[call.scNode] = {call: call.scNode};
    call.scLines.forEach(line => line.slRuns.forEach(run => {
      if (run.lrNode) {
        stops[run.lrNode] = {call: call.scNode, stmt: line.slStmt};
      }
    }));
    call.scCalls.forEach(walk);
  };
  (drSyntax?.srCalls ?? []).forEach(walk);

  // What each expression came to, keyed by the call it was written as: a node's own output is the
  // term its arrow rewrote to, which a caller then goes on rewriting (see `EvalExprRun`).
  const resolved = {};
  (drResult.erExprs ?? []).forEach(e => {
    if (e.exSteps.length) {
      resolved[e.exSteps[0]] = e.exValue;
    }
  });

  return {calls, stops, statements, resolved};
}

// The item a statement of a call is shown as.
function stmtItemId(call, stmt) {
  return `s:${call}:${stmt}`;
}

// The tree the Debugger shows, as items: each with the id the tree knows it by, what kind of thing it
// is, and the items under it. Built once for the run, so that what is drawn and what a link reveals
// are the same tree: `parents` is read off the items rather than worked out again.
//
//   call    -- a call of a function, shown as the function's statements
//   stmt    -- a statement of a function in one call of it, opening onto the steps it was evaluated
//              through and then onto the statements written under it
//   run     -- one of several evaluations of a line, which desugar makes each time the line is used
//   step    -- a node of the evaluation tree that is no call of a function or line of one
//   lineref -- a node that is a line of a function shown elsewhere in the tree, leading to it
//
// A step and a call keep the id of their node, which is what a #debug record names.
function buildTree(roots, syntax) {
  const {calls, stops, statements} = syntax;
  const parents = {};
  const byNode = {};

  const item = (fields, children) => {
    const it = {...fields, children};
    children.forEach(c => {parents[c.id] = it.id;});
    return it;
  };

  const stepItem = node => {
    const id = node[4];
    const stop = stops[id];
    const call = calls[id];
    let it;
    // A call with a display of its own is shown by that display, which speaks for the whole call.
    if (stop && !stop.stmt && call && statements[call.scStmt] && !node[5]) {
      it = callItem(call, node);
    } else if (stop && stop.stmt) {
      it = item({kind: 'lineref', id: `l:${id}`, node, stop}, []);
    } else {
      it = item({kind: 'step', id, node}, node[3].map(stepItem));
    }
    byNode[id] = it;
    return it;
  };

  // The steps a run of a line was evaluated through. Every one is a node of the run, which the server
  // guarantees by building both from the same evaluation.
  const innerItems = run => run.lrInner.map(n => stepItem(syntax.nodes[n]));
  // A line evaluated several times opens onto each evaluation, unless none of them called anything, in
  // which case there is nothing under the line but the values its label already shows.
  const runItems = (callNode, sid, runs) => {
    if (runs.every(run => !run.lrInner.length)) {
      return [];
    }
    if (runs.length === 1) {
      return innerItems(runs[0]);
    }
    return runs.map((run, i) => item({kind: 'run', id: `r:${callNode}:${sid}:${i}`, run, index: i}, innerItems(run)));
  };

  const stmtItem = (call, lines, own, tree) => {
    const sid = statementId(tree.rstStatement);
    const runs = lines[sid] ?? [];
    return item(
      {kind: 'stmt', id: stmtItemId(call.scNode, sid), call, sid, tree, runs, own: own.has(sid)},
      [...runItems(call.scNode, sid, runs), ...tree.rstChildren.map(t => stmtItem(call, lines, own, t))]
    );
  };

  function callItem(call, node) {
    const statement = statements[call.scStmt];
    const lines = {};
    call.scLines.forEach(line => {lines[line.slStmt] = line.slRuns;});
    const own = new Set(statement.ssLines ?? []);
    const head = lines[call.scStmt] ?? [];
    return item(
      {kind: 'call', id: call.scNode, call, node, statement, runs: head},
      [...runItems(call.scNode, call.scStmt, head), ...statement.ssTree.rstChildren.map(t => stmtItem(call, lines, own, t))]
    );
  }

  const items = roots.map(stepItem);
  return {items, parents, byNode};
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

// The line of its file a statement is written on: the first line anything in it is placed on.
//
// Read over everything in the statement rather than off its own meta, because the parser gives an
// operator or a method call no position of its own (`mkOp2` in src/Syntax/Ct/Parser/Expr.hs), and a
// result line is usually one: `scale(v= …) + sum` is placed only by the parts it is made of.
function statementLine(statement) {
  const body = statementBody(statement);
  const expr = statement.tag === "RawAnnot" ? body : body?.roaObj;
  let first = null;
  const walk = value => {
    if (Array.isArray(value)) {
      value.forEach(walk);
    } else if (value && typeof value === 'object') {
      if ('getMetaPos' in value) {
        const line = value.getMetaPos?.[0]?.line;
        if (line && (first === null || line < first)) {
          first = line;
        }
        return;
      }
      Object.values(value).forEach(walk);
    }
  };
  walk(expr);
  return first;
}

// The line number a statement is shown beside, so each line of a block says where it is written.
function LineNumber(props) {
  const line = statementLine(props.statement);
  return (
    <span className="catln-debug-lineno" style={{display: 'inline-block', minWidth: '2.5em', marginRight: '0.75em', textAlign: 'right', opacity: 0.45, fontFamily: 'monospace', fontSize: '0.85em'}}>
      {line ?? ""}
    </span>
  );
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
  let text;
  switch (statement.tag) {
  case "RawDeclStatement": {
    const [arrExpr, arrType] = body.roaArr ?? [null, null];
    const roa = arrExpr && rawPath(arrExpr) === NESTED_DECLARATION ? {...body, roaArr: [null, arrType]} : body;
    text = <RawObjArr roa={roa} />;
    break;
  }
  case "RawBindStatement":
    text = <span><RawExpr expr={body.roaObj} /> &lt;- {body.roaArr?.[0] ? <RawExpr expr={body.roaArr[0]} /> : null}</span>;
    break;
  case "RawAnnot":
    text = <RawExpr expr={body} />;
    break;
  default:
    text = null;
  }
  return <span style={{fontFamily: 'monospace'}}>{text}</span>;
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
    <span className="catln-debug-value">
      {shown.map((v, i) => (
        <span key={i}>
          {i ? " | " : " => "}
          {v ? <InlineValue data={v} /> : <em>no value recorded</em>}
          {counts[JSON.stringify(v)] > 1 ? <span className="catln-debug-count"> ×{counts[JSON.stringify(v)]}</span> : null}
        </span>
      ))}
    </span>
  );
}

export {LineNumber, RunValues, StatementText, buildTree, isComment, statementLine, stmtItemId, syntaxIndex};
