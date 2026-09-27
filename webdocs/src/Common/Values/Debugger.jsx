import React, {createContext, useCallback, useContext, useEffect, useMemo, useRef, useState} from 'react';

import {SimpleTreeView} from '@mui/x-tree-view/SimpleTreeView';
import ExpandMoreIcon from '@mui/icons-material/ExpandMore';
import ChevronRightIcon from '@mui/icons-material/ChevronRight';
import {TreeItem} from '@mui/x-tree-view/TreeItem';
import Link from '@mui/material/Link';

import {ExprStyleContext, exprContents, runStyle} from '../Semantics';
import {NameLinkContext, PTypeName} from '../Common';
import {InlineValue, Value} from '../Value';
import {CallStepsContext} from './Steps';
import {SourceButton, SourceContext, SourcePanel} from './DebugSource';
import {LineNumber, RunValues, StatementText, buildTree, isComment, statementLine, stmtItemId, syntaxIndex} from './DebugSyntax';

// How an item of the tree reaches the rest of it: what each node's expression came to, the items by
// the node they stand for, and how to go to another item.
const DebugTreeContext = createContext({resolved: {}, byNode: {}, byId: {}, goTo: () => {}, nodeRefs: {current: {}}, isOpen: () => false, toggleOpen: () => {}});

// What marks a call of a function opened in place under the part of a line it was, among the tree's
// expanded items: the call's own id says whether its lines are shown, and this whether it is.
function openKey(id) {
  return `open:${id}`;
}

function Debugger(props) {
  const fun = props["/fun"].contents;
  const run = props["/run"];

  // The run rides on the compiled value, so nothing is fetched. It was made under a mock IO handle,
  // which is why the output below is here to show rather than on the doc server's own stdout.
  const {drExit, drOutput, drValue, drResult, drSources, drSyntax} = useMemo(() => JSON.parse(run.contents), [run]);

  // An empty tree means the run never entered an arrow, which a program that failed before its first
  // call can do. Say so rather than rendering an undefined node.
  //
  // All of them and not the first: a run can enter more than one arrow at the top, which is what an
  // expression built as a sequence of calls does. Showing only the first showed the whole of a run
  // whose top was one call and a misleading fraction of one whose top was several.
  const roots = drResult.erTreebug;
  const debugs = drResult.erDebugs;

  // One tree for the run: each call of a function shown as the function is written, each line opening
  // onto the steps it was evaluated through, and each call of a function among those steps shown the
  // same way in its place (see DebugSyntax). A run the syntax does not cover is its evaluation tree.
  const tree = useMemo(() => {
    const syntax = {...syntaxIndex(drSyntax, drResult), nodes: indexNodes(roots)};
    return {...buildTree(roots, syntax), resolved: syntax.resolved, stops: syntax.stops};
  }, [drSyntax, drResult, roots]);

  // The tree is controlled so that a link can reveal what it leads to: a #debug record names a node,
  // and a step names a line of a function, and either can be inside a collapsed item. Expansion and
  // selection the reader does themselves is kept, which is why both changes are handled rather than
  // only ours -- revealing an item adds to what they have open instead of replacing it.
  //
  // The calls the run started with are open, so what is on screen first is the function that was run,
  // line by line.
  const [expanded, setExpanded] = useState(() => tree.items.filter(it => it.kind === 'call').map(it => it.id));
  const [selected, setSelected] = useState(null);
  const nodeRefs = useRef({});

  // The call whose source is open, if one is: which node, its arrow, and the line to lay it under.
  // Opening the one already open closes it, which is what a second click on its book means.
  const [source, setSource] = useState(null);
  const toggleSource = useCallback((id, oa, anchor) => {
    setSource(open => (!oa || open?.id === id) ? null : {id, oa, anchor});
  }, []);
  const sourceContext = useMemo(() => ({open: source, toggle: toggleSource, sources: drSources ?? {}}), [source, toggleSource, drSources]);

  // An item inside a call opened under the part of a line it was is shown only once that call is,
  // so going to it opens each such call above it as well as expanding it.
  const goTo = useCallback(itemId => {
    const above = [];
    const opened = id => tree.byId[id]?.inLine ? [openKey(id)] : [];
    above.push(...opened(itemId));
    for (let p = tree.parents[itemId]; p !== undefined; p = tree.parents[p]) {
      above.push(p, ...opened(p));
    }
    setExpanded(open => {
      const closed = above.filter(id => !open.includes(id));
      return closed.length ? [...open, ...closed] : open;
    });
    setSelected(itemId);
  }, [tree]);

  // A #debug record names the call it was recorded in, which is either a call of a function or the
  // arrow of a line of one; the record leads to whichever it is.
  const reveal = useCallback(node => {
    const stop = tree.stops[node];
    goTo(stop?.stmt ? stmtItemId(stop.call, stop.stmt) : node);
  }, [tree, goTo]);

  // Opening a call in place shows it with its lines, since its lines are what it was opened to read.
  const toggleOpen = useCallback(id => {
    setExpanded(open => open.includes(openKey(id))
      ? open.filter(i => i !== openKey(id))
      : [...open, openKey(id), ...(open.includes(id) ? [] : [id])]);
  }, []);
  const isOpen = useCallback(id => expanded.includes(openKey(id)), [expanded]);
  const treeContext = useMemo(() => ({resolved: tree.resolved, byNode: tree.byNode, byId: tree.byId, goTo, nodeRefs, isOpen, toggleOpen}),
                              [tree, goTo, isOpen, toggleOpen]);

  // After the ancestors are open, since an item inside a collapsed one has nothing on screen to
  // bring into view. Optional chained because jsdom implements no scrolling at all.
  useEffect(() => {
    if (selected) {
      nodeRefs.current[selected]?.scrollIntoView?.({block: 'nearest'});
    }
  }, [selected, expanded]);

  // Written for reading a run rather than a program, with its types on hover and without the call
  // trees the tree below already shows as calls: see ExprStyleContext.
  return (
    <ExprStyleContext.Provider value={runStyle}>
      <DebugTreeContext.Provider value={treeContext}>
      <SourceContext.Provider value={sourceContext}>
        <div>
          {/* What the run ended with, which is not the same question for the two kinds of run.

              An expression was run for its value, so the heading reads as input and output the way
              every call in the tree below is labelled: what was run, then what it became. It was
              given no IO handle to print with, so it has neither output nor an exit code to report.

              A program ran for what it printed and how it exited, which is more than a heading holds,
              so it is shown straight under the heading, before the tree says how the run got there. */}
          <h2>{fun}{drValue ? <> =&gt; <InlineValue data={drValue} /></> : null}</h2>
          {drValue ? null : (
            <>
              <h3>Output</h3>
              {drOutput ? <pre>{drOutput}</pre> : <p>Printed nothing.</p>}
              <p>Exited with {drExit}.</p>
            </>
          )}
          {roots.length ? (
            <NameLinkContext.Provider value={false}>
            <SimpleTreeView
              slots={{collapseIcon: ExpandMoreIcon, expandIcon: ChevronRightIcon}}
              sx={{marginTop: 2}}
              expandedItems={expanded}
              onExpandedItemsChange={(event, items) => setExpanded(items)}
              selectedItems={selected}
              onSelectedItemsChange={(event, item) => setSelected(item)}
            >
              {tree.items.map(item => <DebugItem item={item} key={item.id} />)}
            </SimpleTreeView>
            </NameLinkContext.Provider>
          ) : <p>Evaluated no arrows.</p>}
          {/* Every #debug the run recorded, in the order it recorded them: one list over the whole run,
              however many definitions and however many calls they came from. Each says where it is
              written, since that is what the compiler can name it by, and leads to the call it was
              recorded in, which is what the tree above cannot be searched for by hand.

              Left out when the run recorded none, which is most runs: a program only records what
              a #debug in it asks for, so an empty section says nothing about the run. */}
          {debugs.length ? (
            <>
              <h3>Debug</h3>
              <ol>
                {debugs.map((d, i) => (
                  <li key={i}>
                    <Link component="button" type="button" onClick={() => reveal(d.edNode)}>
                      {showDebugPos(d.edPos)}
                    </Link>
                    {" ↦ "}
                    <InlineValue data={d.edValue} />
                  </li>
                ))}
              </ol>
            </>
          ) : null}
          <SourcePanel />
        </div>
      </SourceContext.Provider>
      </DebugTreeContext.Provider>
    </ExprStyleContext.Provider>
  );
}

// Every node of a run by its id.
function indexNodes(roots) {
  const byId = {};
  const walk = node => {
    byId[node[4]] = node;
    node[3].forEach(walk);
  };
  roots.forEach(walk);
  return byId;
}

// Where a #debug is written, as the file and line it is on. The file is shown by its name alone: a
// run reaches whatever it imports, so the paths are absolute and mostly the same prefix, which is
// the part that says least about which record this is.
// A record whose annotation the compiler gave no position says so, rather than being left with a
// blank where every other record has a place.
function showDebugPos(pos) {
  if (!pos) {
    return "unknown location";
  }
  const [start] = pos;
  return `${start.name.split("/").pop()}:${start.line}`;
}

// The definition a node is a call of, as the name its arrow is written under. A node carries that
// arrow in either of two forms -- the program's own, or the one built for the call -- and the name is
// read the same way out of both.
function nodeFun(node) {
  const [eitherOa] = node;
  const oa = "Left" in eitherOa ? eitherOa["Left"] : eitherOa["Right"];
  const obj = oa && oa.oaObj;
  return obj ? exprPath(obj) : null;
}

// The head of a name expression, walking down through however many arguments were applied to it and
// through whatever was written around it. Both the program's expressions and the built ones are
// walked, since a node carries its arrow in either form -- which is what `exprContents` is for.
function exprPath(expr) {
  let e = expr;
  for (;;) {
    const parts = e && exprContents(e);
    if (!parts) {
      return null;
    }
    switch (e.tag) {
    case "TupleApply":
    case "TTupleApply":
      e = parts[1][1];
      break;
    case "EWhere":
    case "TWhere":
      e = parts[1];
      break;
    case "AliasExpr":
    case "TAliasExpr":
      e = parts[0];
      break;
    case "Value":
    case "TValue":
      return parts[1];
    default:
      return null;
    }
  }
}

// A call with a display of its own stands for the whole recursion of calls of the same definition
// underneath it, because that is what its display describes: a mapping over three elements is four
// nested calls of one definition, and one table of three rows.
//
// What each row opens onto is collected here. At each level, everything that is NOT the recursion
// continuing is what that level did -- for an fmap, the one call of the mapped function -- and those
// calls are one row. A level that did more than one such thing hands over all of them rather than
// picking, since a row that quietly showed one of two would be a row that lied about what happened.
//
// The recursion need not be a chain. A definition over a tree calls itself on both subtrees, and its
// rows are wanted in the order its levels did their own work, which is the order it made its calls
// in: a level's row goes after the rows of the recursive calls it made before its own work and before
// the rows of those it made after. A mapping over a map's values calls itself on the left subtree,
// then the mapped function, then itself on the right, so its rows come out in key order; a list's
// chain has its own work before its one recursive call, so its rows come out in element order as
// they always have. Nothing here knows which of those it is walking.
function chainSteps(node, fun) {
  const steps = levelSteps(node, fun);
  // The last level of a chain is the one that ended it -- an fmap's Nil arm -- and did nothing of its
  // own, so it contributes no row. Dropping it here rather than in the walk keeps the walk about the
  // shape of the recursion and this about what a table has rows for.
  while (steps.length && !steps[steps.length - 1].length) {
    steps.pop();
  }
  return steps;
}

// The rows of one level and of every level below it, in the order the calls were made.
//
// A level that made no call at all -- a leaf of the recursion, such as a map's empty subtree --
// contributes nothing, where one that recursed without doing anything of its own still contributes
// its empty row, which keeps every row after it opening onto its own level.
function levelSteps(node, fun) {
  const children = node[3];
  const own = children.filter(child => nodeFun(child) !== fun);
  const recursive = children.filter(child => nodeFun(child) === fun);
  if (!own.length && !recursive.length) {
    return [];
  }
  const firstOwn = children.findIndex(child => nodeFun(child) !== fun);
  const before = firstOwn < 0 ? 0 : recursive.filter(child => children.indexOf(child) < firstOwn).length;
  return [
    ...recursive.slice(0, before).flatMap(child => levelSteps(child, fun)),
    own,
    ...recursive.slice(before).flatMap(child => levelSteps(child, fun)),
  ];
}

// The arrow a node entered, in whichever of its two forms it carries: the program's own, or the one
// built for the call.
function nodeArrow(node) {
  const [eitherOa] = node;
  return "Left" in eitherOa ? eitherOa["Left"] : eitherOa["Right"];
}

// What a call was given and what it gave back. The definition it was a call of is behind the book, as
// where it is written: spelled out here it was most of the line, and the input already names the call.
//
// Where the expression the call was written as went on being rewritten after the call returned -- an
// arrow returns the term its body rewrites to, and the caller applies what is left -- what it came to
// follows, which is the step a line's value is reached by.
function StepLabel(props) {
  const {node} = props;
  const [, input, val, , id] = node;
  const {resolved} = useContext(DebugTreeContext);
  const came = resolved[id];
  const cameTo = came && JSON.stringify(came) !== JSON.stringify(val) ? <> ⇒ <InlineValue data={came} /></> : null;
  return <span><SourceButton id={id} oa={nodeArrow(node)} /><InlineValue data={input} /> =&gt; <InlineValue data={val} />{cameTo}</span>;
}

// How the two kinds of item are told apart. A call of a function is a block of source: a header naming
// the function and the file it is written in, over a tinted body holding its statements, each beside
// the line it is written on. A step of the evaluation between them is smaller and plainer, as the
// working that connects one piece of source to the next rather than source itself.
const BLOCK_EDGE = 'rgba(25, 118, 210, 0.45)';
const callSx = {
  margin: '6px 0',
  '& > .MuiTreeItem-content': {
    border: `1px solid ${BLOCK_EDGE}`,
    borderRadius: '6px',
    backgroundColor: 'rgba(25, 118, 210, 0.10)',
  },
  '&[aria-expanded="true"] > .MuiTreeItem-content': {
    borderRadius: '6px 6px 0 0',
  },
  '& > .MuiTreeItem-groupTransition': {
    borderLeft: `3px solid ${BLOCK_EDGE}`,
    borderBottom: `1px solid ${BLOCK_EDGE}`,
    borderRadius: '0 0 0 6px',
    backgroundColor: 'rgba(25, 118, 210, 0.035)',
    marginLeft: '6px',
    paddingLeft: '4px',
    paddingBottom: '2px',
  },
};
const stepSx = {
  '& > .MuiTreeItem-content': {
    fontSize: '0.85em',
    color: 'text.secondary',
    paddingTop: 0,
    paddingBottom: 0,
  },
};

// The file a call's function is written in, and where, heading its block. The name alone, with the
// whole path on hover: a run reaches whatever it imports, and the paths mostly share a prefix.
function FileTag(props) {
  const {statement} = props;
  const line = statementLine(statement.ssTree.rstStatement);
  const name = statement.ssFile.split("/").pop();
  return (
    <span className="catln-debug-file" title={statement.ssFile}
      style={{fontSize: '0.75em', fontWeight: 600, letterSpacing: '0.02em', marginRight: '0.75em', padding: '1px 6px', borderRadius: '3px', backgroundColor: 'rgba(25, 118, 210, 0.18)'}}>
      {name}{line ? `:${line}` : ""}
    </span>
  );
}

// The breakdown of a statement: for each run of it, the parts of the line as written, one under
// another and indented under the part they are part of, each with what it came to. A part that was a
// call of a function opens that function's block in place, under it; a part that was a use of
// another line leads to that line. What the run evaluated that no part accounts for follows.
//
// Rows rather than items of the tree: they are the one line read closely, not further steps of the
// evaluation, and only the blocks opened among them are the tree again.
function Breakdown(props) {
  const {breakdowns, runCount} = props;
  return (
    // A click in here is the breakdown's: without stopping it, the statement it is under takes the mouse
    // down as a click on its own row, and the link pressed is re-rendered from under the click.
    <div className="catln-debug-breakdown" style={{margin: '2px 0 6px 2.2em', paddingLeft: '0.75em', borderLeft: '2px solid rgba(127, 127, 127, 0.35)'}}
      onMouseDown={event => event.stopPropagation()} onClick={event => event.stopPropagation()}>
      {breakdowns.map(b => (
        <div key={b.index}>
          {runCount > 1 ? <div style={{fontSize: '0.8em', opacity: 0.7, marginTop: '2px'}}>run {b.index + 1}<RunValues runs={[b.run]} /></div> : null}
          {b.rows.map((row, i) => <BreakdownRow row={row} key={i} />)}
          {b.rest.length ? (
            <div style={{marginTop: '2px'}}>
              <div style={{fontSize: '0.75em', opacity: 0.6}}>also evaluated</div>
              {b.rest.map(child => <DebugItem item={child} key={child.id} />)}
            </div>
          ) : null}
        </div>
      ))}
    </div>
  );
}

function BreakdownRow(props) {
  const {row} = props;
  const {byId, goTo, isOpen, toggleOpen} = useContext(DebugTreeContext);
  const {depth, expr, records, target} = row;

  let action = null;
  let opened = null;
  if (target?.kind === 'open') {
    // Named by where a function's block is written, and otherwise by the function the call is of.
    const {item} = target;
    const open = isOpen(item.id);
    // A closure has no name of its own, and is named as what it is.
    const fun = item.kind === 'call' ? null : nodeFun(item.node);
    const name = item.kind === 'call' ? <FileTag statement={item.statement} /> : (fun ? <PTypeName name={fun} /> : "closure");
    action = (
      <Link component="button" type="button" underline="hover" onClick={() => toggleOpen(item.id)}
        aria-expanded={open} title={open ? "Close the call" : "Open the call"} className="catln-debug-open">
        {open ? "▾" : "▸"} {name}
      </Link>
    );
    opened = open ? <div style={{marginLeft: `${depth * 1.5}em`}}><DebugItem item={item} /></div> : null;
  } else if (target?.kind === 'line') {
    const stmtId = stmtItemId(target.stop.call, target.stop.stmt);
    const line = byId[stmtId] ? statementLine(byId[stmtId].tree.rstStatement) : null;
    action = (
      <Link component="button" type="button" onClick={() => goTo(stmtId)} title="Go to the line">
        ↑ line{line ? ` ${line}` : ""}
      </Link>
    );
  }

  return (
    <>
      <div className="catln-debug-part" style={{paddingLeft: `${depth * 1.5}em`, display: 'flex', alignItems: 'baseline', gap: '0.75em', lineHeight: 1.7}}>
        <StatementText statement={{tag: "RawAnnot", ranAnnot: expr}} />
        <RunValues runs={records.map(r => ({lrValue: r.exValue}))} />
        {action}
      </div>
      {opened}
    </>
  );
}

// One item of the run's tree (see `buildTree` in DebugSyntax for the kinds).
function DebugItem(props) {
  const {item} = props;
  const {goTo, nodeRefs} = useContext(DebugTreeContext);
  const ref = el => {nodeRefs.current[item.id] = el};
  // A statement's breakdown holds the items opened in it, so only the statements under it, and a
  // step's own children, are items of their own here.
  const itemChildren = item.breakdowns
    ? item.children.filter(child => child.kind === 'stmt')
    : item.children;
  const children = [
    // Only a breakdown with something in it, since any child at all makes the row one that opens.
    ...(item.breakdowns?.length ? [<Breakdown key="breakdown" breakdowns={item.breakdowns} runCount={item.runs.length} />] : []),
    ...itemChildren.map(child => <DebugItem item={child} key={child.id} />),
  ];

  switch (item.kind) {
  case 'call': {
    // The function as it is written, with what this call was given and what it came to. What it was
    // given is by the parts of the call rather than by the function's parameters, which are the
    // written statement's.
    const {call, node, statement, runs} = item;
    const label = (
      <span>
        <FileTag statement={statement} />
        <SourceButton id={call.scNode} oa={nodeArrow(node)} />
        <StatementText statement={statement.ssTree.rstStatement} />
        {runs.length ? <RunValues runs={runs} /> : null}
        <span className="catln-debug-input" style={{marginLeft: '1em', opacity: 0.6}}>
          <InlineValue data={node[1]} />
        </span>
      </span>
    );
    return <TreeItem itemId={item.id} label={label} ref={ref} className="catln-debug-call" sx={callSx}>{children}</TreeItem>;
  }
  case 'stmt': {
    // A line of the function with no run was not evaluated in this call: desugar evaluates a line where
    // it is used, so an unused one never is. A statement that is no line of this function -- a helper
    // nested in it, which takes parameters of its own and is a call of its own -- is shown without
    // values, which are in its calls instead.
    const {tree, runs, own} = item;
    const statement = tree.rstStatement;
    let value = null;
    if (runs.length) {
      value = <RunValues runs={runs} />;
    } else if (own && !isComment(statement)) {
      value = <em className="catln-debug-unevaluated" style={{marginLeft: '1em', opacity: 0.6}}>not evaluated</em>;
    }
    const label = (
      <span style={{opacity: own || runs.length ? 1 : 0.55}}>
        <LineNumber statement={statement} />
        <StatementText statement={statement} />
        {value}
      </span>
    );
    return <TreeItem itemId={item.id} label={label} ref={ref} className="catln-debug-stmt" data-stmt={item.sid}>{children}</TreeItem>;
  }
  case 'lineref': {
    // A line of a function shown elsewhere in the tree, where it opens onto how it was evaluated.
    const {stop, node} = item;
    const target = stmtItemId(stop.call, stop.stmt);
    const go = event => {
      event.stopPropagation();
      goTo(target);
    };
    const label = (
      <span>
        <Link component="button" type="button" onClick={go} title="Go to the line">↑ line</Link>{" "}
        <StepLabel node={node} />
      </span>
    );
    return <TreeItem itemId={item.id} label={label} ref={ref} className="catln-debug-step" sx={stepSx} />;
  }
  default: {
    const {node} = item;
    const [, , , , id, show] = node;
    // A call the library gave a display of its own. The display replaces the generic label -- which
    // is the point of it -- and it goes in the item's children rather than its label so that each of
    // its rows can be an item that opens.
    //
    // The chain of calls this one stands for is folded into those rows and is not rendered beside
    // them. It is still in the tree; what is gone is a second, worse display of the same thing next
    // to the good one.
    if (show) {
      return <ShowItem item={item} fun={nodeFun(node)} id={id} show={show} itemRef={ref} />;
    }
    return <TreeItem itemId={item.id} label={<StepLabel node={node} />} ref={ref} className="catln-debug-step" sx={stepSx}>{children}</TreeItem>;
  }
  }
}

// A call with a display of its own, whose rows open onto the items of the calls each row stands for.
function ShowItem(props) {
  const {item, fun, id, show, itemRef} = props;
  const {byNode} = useContext(DebugTreeContext);
  const steps = chainSteps(item.node, fun).map(
    level => level.map(child => byNode[child[4]] ? <DebugItem item={byNode[child[4]]} key={child[4]} /> : null)
  );
  // Named rather than written out. The generic label spells the whole definition because that is all
  // it has to say about the call; this one has the display below it saying what the call did, so the
  // definition it was is a heading rather than the content.
  return (
    <TreeItem itemId={item.id} label={<span><SourceButton id={id} oa={nodeArrow(item.node)} /><PTypeName name={fun} /></span>} ref={itemRef}>
      <CallStepsContext.Provider value={steps}>
        <Value data={show} />
      </CallStepsContext.Provider>
    </TreeItem>
  );
}

export default Debugger;
