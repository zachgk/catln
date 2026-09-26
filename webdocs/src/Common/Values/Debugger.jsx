import React, {useCallback, useEffect, useMemo, useRef, useState} from 'react';

import {SimpleTreeView} from '@mui/x-tree-view/SimpleTreeView';
import ExpandMoreIcon from '@mui/icons-material/ExpandMore';
import ChevronRightIcon from '@mui/icons-material/ChevronRight';
import {TreeItem} from '@mui/x-tree-view/TreeItem';
import Link from '@mui/material/Link';

import {ExprStyleContext, exprContents, runStyle} from '../Semantics';
import {PTypeName} from '../Common';
import {InlineValue, Value} from '../Value';
import {CallStepsContext} from './Steps';
import {SourceButton, SourceContext, SourcePanel} from './DebugSource';

function Debugger(props) {
  const fun = props["/fun"].contents;
  const run = props["/run"];

  // The run rides on the compiled value, so nothing is fetched. It was made under a mock IO handle,
  // which is why the output below is here to show rather than on the doc server's own stdout.
  const {drExit, drOutput, drValue, drResult, drSources} = useMemo(() => JSON.parse(run.contents), [run]);

  // An empty tree means the run never entered an arrow, which a program that failed before its first
  // call can do. Say so rather than rendering an undefined node.
  //
  // All of them and not the first: a run can enter more than one arrow at the top, which is what an
  // expression built as a sequence of calls does. Showing only the first showed the whole of a run
  // whose top was one call and a misleading fraction of one whose top was several.
  const roots = drResult.erTreebug;
  const debugs = drResult.erDebugs;

  // The tree is controlled only so that a debug record can reveal the call it was recorded in: the
  // record names a node, and a node inside a collapsed ancestor is on the page but not on screen.
  // Expansion and selection the reader does themselves is kept, which is why both changes are
  // handled rather than only ours -- revealing a node adds to what they have open instead of
  // replacing it.
  const [expanded, setExpanded] = useState([]);
  const [selected, setSelected] = useState(null);
  const nodeRefs = useRef({});

  // The call whose source is open, if one is: which node, its arrow, and the line to lay it under.
  // Opening the one already open closes it, which is what a second click on its book means.
  const [source, setSource] = useState(null);
  const toggleSource = useCallback((id, oa, anchor) => {
    setSource(open => (!oa || open?.id === id) ? null : {id, oa, anchor});
  }, []);
  const sourceContext = useMemo(() => ({open: source, toggle: toggleSource, sources: drSources ?? {}}), [source, toggleSource, drSources]);

  const reveal = useCallback(node => {
    setExpanded(open => {
      const closed = (ancestorNodes(drResult.erTreebug, node) || []).filter(id => !open.includes(id));
      return closed.length ? [...open, ...closed] : open;
    });
    setSelected(node);
  }, [drResult.erTreebug]);

  // After the ancestors are open, since a node inside a collapsed one has nothing on screen to
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
            <SimpleTreeView
              slots={{collapseIcon: ExpandMoreIcon, expandIcon: ChevronRightIcon}}
              sx={{marginTop: 2}}
              expandedItems={expanded}
              onExpandedItemsChange={(event, items) => setExpanded(items)}
              selectedItems={selected}
              onSelectedItemsChange={(event, item) => setSelected(item)}
            >
              {roots.map(root => <TreebugNode data={root} nodeRefs={nodeRefs} key={root[4]} />)}
            </SimpleTreeView>
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
    </ExprStyleContext.Provider>
  );
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

// The nodes holding a node, which are the items to expand to reveal it, closest to the root first.
// Nothing when the tree does not hold it -- which is not the same as it being a root, and is why
// this is distinguishable from the empty list.
function ancestorNodes(nodes, target) {
  for (const node of nodes) {
    const [, , , children, id] = node;
    if (id === target) {
      return [];
    }
    const below = ancestorNodes(children, target);
    if (below) {
      return [id, ...below];
    }
  }
  return null;
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

function TreebugNode(props) {
  let [eitherOa, input, val, children, id, show] = props.data;
  const {nodeRefs} = props;

  let oa;
  if ("Left" in eitherOa) {
    oa = eitherOa["Left"];
  } else {
    oa = eitherOa["Right"];
  }

  // A call the library gave a display of its own. The display replaces the generic label -- which is
  // the point of it -- and it goes in the item's children rather than its label so that each of its
  // rows can be an item that opens.
  //
  // The chain of calls this one stands for is folded into those rows and is not rendered beside them.
  // It is still in the tree; what is gone is a second, worse display of the same thing next to the
  // good one.
  if (show) {
    const fun = nodeFun(props.data);
    const steps = chainSteps(props.data, fun).map(
      level => level.map(child => <TreebugNode data={child} nodeRefs={nodeRefs} key={child[4]} />)
    );
    // Named rather than written out. The generic label spells the whole definition because that is
    // all it has to say about the call; this one has the display below it saying what the call did,
    // so the definition it was is a heading rather than the content.
    return (
      <TreeItem itemId={id} label={<span><SourceButton id={id} oa={oa} /><PTypeName name={fun} /></span>} ref={el => {nodeRefs.current[id] = el}}>
        <CallStepsContext.Provider value={steps}>
          <Value data={show} />
        </CallStepsContext.Provider>
      </TreeItem>
    );
  }

  // What the call was given and what it gave back. The definition it was a call of is behind the
  // book, as where it is written: spelled out here it was most of the line, and the input already
  // names the call.
  let label = <span><SourceButton id={id} oa={oa} /><InlineValue data={input} /> =&gt; <InlineValue data={val} /></span>;
  return (
    <TreeItem itemId={id} label={label} ref={el => {nodeRefs.current[id] = el}}>
      {children.map(child => <TreebugNode data={child} nodeRefs={nodeRefs} key={child[4]} />)}
    </TreeItem>
  );
}

export default Debugger;
