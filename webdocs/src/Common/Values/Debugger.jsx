import React, {useCallback, useEffect, useMemo, useRef, useState} from 'react';

import {SimpleTreeView} from '@mui/x-tree-view/SimpleTreeView';
import ExpandMoreIcon from '@mui/icons-material/ExpandMore';
import ChevronRightIcon from '@mui/icons-material/ChevronRight';
import {TreeItem} from '@mui/x-tree-view/TreeItem';
import Link from '@mui/material/Link';

import {ObjArr} from '../Semantics';
import {Value} from '../Value';

function Debugger(props) {
  const fun = props["/fun"].contents;
  const run = props["/run"];

  // The run rides on the compiled value, so nothing is fetched. It was made under a mock IO handle,
  // which is why the output below is here to show rather than on the doc server's own stdout.
  const {drExit, drOutput, drValue, drResult} = useMemo(() => JSON.parse(run.contents), [run]);

  // An empty tree means the run never entered an arrow, which a program that failed before its first
  // call can do. Say so rather than rendering an undefined node.
  const [root] = drResult.erTreebug;
  const debugs = drResult.erDebugs;

  // The tree is controlled only so that a debug record can reveal the call it was recorded in: the
  // record names a node, and a node inside a collapsed ancestor is on the page but not on screen.
  // Expansion and selection the reader does themselves is kept, which is why both changes are
  // handled rather than only ours -- revealing a node adds to what they have open instead of
  // replacing it.
  const [expanded, setExpanded] = useState([]);
  const [selected, setSelected] = useState(null);
  const nodeRefs = useRef({});

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

  return (
    <div>
      <h2>{fun}</h2>
      {root ? (
        <SimpleTreeView
          slots={{collapseIcon: ExpandMoreIcon, expandIcon: ChevronRightIcon}}
          expandedItems={expanded}
          onExpandedItemsChange={(event, items) => setExpanded(items)}
          selectedItems={selected}
          onSelectedItemsChange={(event, item) => setSelected(item)}
        >
          <TreebugNode data={root} nodeRefs={nodeRefs} />
        </SimpleTreeView>
      ) : <p>Evaluated no arrows.</p>}
      {/* Every #debug the run recorded, in the order it recorded them: one list over the whole run,
          however many definitions and however many calls they came from. Each says where it is
          written, since that is what the compiler can name it by, and leads to the call it was
          recorded in, which is what the tree above cannot be searched for by hand. */}
      <h3>Debug</h3>
      {debugs.length ? (
        <ol>
          {debugs.map((d, i) => (
            <li key={i}>
              <Link component="button" type="button" onClick={() => reveal(d.edNode)}>
                {showDebugPos(d.edPos)}
              </Link>
              {" ↦ "}
              <Value data={d.edValue} />
            </li>
          ))}
        </ol>
      ) : <p>Recorded nothing.</p>}
      {/* What the run ended with, which is not the same question for the two kinds of run. An
          expression was run for its value and was given no IO handle to print with, so it has
          neither output nor an exit code to report. */}
      {drValue ? (
        <>
          <h3>Value</h3>
          <Value data={drValue} />
        </>
      ) : (
        <>
          <h3>Output</h3>
          {drOutput ? <pre>{drOutput}</pre> : <p>Printed nothing.</p>}
          <p>Exited with {drExit}.</p>
        </>
      )}
    </div>
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

function TreebugNode(props) {
  let [eitherOa, input, val, children, id] = props.data;
  const {nodeRefs} = props;

  let oa;
  if ("Left" in eitherOa) {
    oa = eitherOa["Left"];
  } else {
    oa = eitherOa["Right"];
  }

  let label = <span><ObjArr oa={oa} /> |::| <Value data={input} /> =&gt; <Value data={val} /></span>;
  return (
    <TreeItem itemId={id} label={label} ref={el => {nodeRefs.current[id] = el}}>
      {children.map(child => <TreebugNode data={child} nodeRefs={nodeRefs} key={child[4]} />)}
    </TreeItem>
  );
}

export default Debugger;
