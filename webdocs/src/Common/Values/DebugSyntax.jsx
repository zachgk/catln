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
  //
  // And each expression by where it was evaluated and what it is written as, which is how a line's
  // parts are found among what its run evaluated (`breakdownOf`).
  const resolved = {};
  const exprsAt = {};
  (drResult.erExprs ?? []).forEach(e => {
    if (e.exSteps.length) {
      resolved[e.exSteps[0]] = e.exValue;
    }
    (e.exIds ?? []).forEach(id => {
      const key = `${e.exNode}|${id}`;
      (exprsAt[key] = exprsAt[key] ?? []).push(e);
    });
  });

  return {calls, stops, statements, resolved, exprsAt};
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
//   stmt    -- a statement of a function in one call of it, opening onto its breakdown and then
//              onto the statements written under it
//   step    -- a node of the evaluation tree that is no call of a function or line of one
//   lineref -- a node that is a line of a function shown elsewhere in the tree, leading to it
//
// A statement's breakdown (`breakdownOf`) is not items but rows: the parts of the line as written,
// one under another, each with what it came to. A part that was a call of a function holds that
// call's item, which the reader opens in place under the part (`inLine`); a part that was a use of
// another line leads to it. What the line evaluated that no part of it accounts for -- a call made
// while dispatching one, say -- follows the rows as items.
//
// A step and a call keep the id of their node, which is what a #debug record names.
function buildTree(roots, syntax) {
  const {calls, stops, statements, exprsAt} = syntax;
  const parents = {};
  const byNode = {};
  const byId = {};

  const item = (fields, children) => {
    const it = {...fields, children};
    children.forEach(c => {parents[c.id] = it.id;});
    byId[it.id] = it;
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

  // Where a part of a line leads: the call of a function it was, opened in place; the line it was a
  // use of; or nowhere, for a part evaluated without either.
  const targetOf = records => {
    const first = records.find(r => r.exSteps.length)?.exSteps[0];
    if (first === undefined || !syntax.nodes[first]) {
      return null;
    }
    const stop = stops[first];
    if (stop?.stmt) {
      return {kind: 'line', stop};
    }
    const it = stepItem(syntax.nodes[first]);
    if (it.kind === 'call') {
      it.inLine = true;
    }
    return {kind: 'call', item: it};
  };

  // The breakdown of one run of a statement: the rows for the parts of `expr` evaluated in the run's
  // node, and the items for what the run evaluated that none of them accounts for.
  const breakdownOf = (expr, run) => {
    const recordsOf = id => (run.lrIn != null && exprsAt[`${run.lrIn}|${id}`]) || [];
    const rows = [];
    const claimed = new Set();
    const row = (depth, part, records) => {
      records.forEach(r => r.exSteps.forEach(s => claimed.add(s)));
      rows.push({depth, expr: part, records, target: targetOf(records)});
    };
    const walk = (e, depth, top) => {
      if (!e) {
        return;
      }
      switch (e.tag) {
      case "RawTupleApply": {
        // The whole line is the statement's own label, so its parts start at the top of the rows.
        const records = top ? [] : recordsOf(rawExprMeta(e).getMetaId);
        const inner = records.length ? depth + 1 : depth;
        if (records.length) {
          row(depth, e, records);
        }
        // The name applied is the call itself rather than a part of it; a base that is an expression
        // of its own (a call applied again) is a part.
        const base = e.rtaBase[1];
        if (base.tag !== "RawValue") {
          walk(base, inner, false);
        }
        e.rtaArgs.forEach(([, arg]) => walk(arg.roaArr?.[0] ?? arg.roaObj, inner, false));
        break;
      }
      case "RawValue": {
        // A name is a part worth a row only where using it called something -- a line, or a function
        // taking no arguments -- rather than reading an argument.
        const records = top ? [] : recordsOf(rawExprMeta(e).getMetaId).filter(r => r.exSteps.length);
        if (records.length) {
          row(depth, e, records);
        }
        break;
      }
      case "RawMethod":
        walk(e.rmBase, depth, false);
        walk(e.rmValue, depth, top);
        break;
      case "RawParen":
        walk(e.rpnExpr, depth, top);
        break;
      case "RawWhere":
        walk(e.rwBase, depth, top);
        walk(e.rwCond, depth, false);
        break;
      case "RawAliasExpr":
        walk(e.raliasBase, depth, top);
        break;
      case "RawList":
        e.rlValues.forEach(v => walk(v, depth, false));
        break;
      default:
        break;
      }
    };
    walk(expr, 0, true);
    const rest = run.lrInner.filter(n => !claimed.has(n) && syntax.nodes[n]).map(n => stepItem(syntax.nodes[n]));
    return {run, rows, rest};
  };

  // The breakdowns of the runs of a statement, leaving out any with nothing to show.
  const breakdownsOf = (statement, runs) => {
    const expr = evaluatedExpr(statement);
    return runs.map((run, index) => ({...breakdownOf(expr, run), index}))
      .filter(b => b.rows.length || b.rest.length);
  };

  // The items a statement's breakdowns hold, which are the items under it.
  const breakdownItems = breakdowns => breakdowns.flatMap(b => [
    ...b.rows.flatMap(r => r.target?.kind === 'call' ? [r.target.item] : []),
    ...b.rest,
  ]);

  const stmtItem = (call, lines, own, tree) => {
    const sid = statementId(tree.rstStatement);
    const runs = lines[sid] ?? [];
    const breakdowns = breakdownsOf(tree.rstStatement, runs);
    return item(
      {kind: 'stmt', id: stmtItemId(call.scNode, sid), call, sid, tree, runs, breakdowns, own: own.has(sid)},
      [...breakdownItems(breakdowns), ...tree.rstChildren.map(t => stmtItem(call, lines, own, t))]
    );
  };

  function callItem(call, node) {
    const statement = statements[call.scStmt];
    const lines = {};
    call.scLines.forEach(line => {lines[line.slStmt] = line.slRuns;});
    const own = new Set(statement.ssLines ?? []);
    const head = lines[call.scStmt] ?? [];
    const breakdowns = breakdownsOf(statement.ssTree.rstStatement, head);
    return item(
      {kind: 'call', id: call.scNode, call, node, statement, runs: head, breakdowns},
      [...breakdownItems(breakdowns), ...statement.ssTree.rstChildren.map(t => stmtItem(call, lines, own, t))]
    );
  }

  const items = roots.map(stepItem);
  return {items, parents, byNode, byId};
}

// The expression a statement evaluates: what a declaration is defined as, the expression a result is
// written as, or an annotation. None for a declaration written over several lines, whose lines are
// what it evaluates, or for a bind, whose value is what it bound.
function evaluatedExpr(statement) {
  const body = statementBody(statement);
  switch (statement.tag) {
  case "RawDeclStatement": {
    const arrExpr = body.roaArr?.[0];
    if (arrExpr) {
      return rawPath(arrExpr) === NESTED_DECLARATION ? null : arrExpr;
    }
    return body.roaArr ? null : body.roaObj;
  }
  case "RawAnnot":
    return body;
  default:
    return null;
  }
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
