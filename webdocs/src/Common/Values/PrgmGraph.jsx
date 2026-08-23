import React, {useCallback, useEffect, useMemo, useState} from 'react';

import makeStyles from '@mui/styles/makeStyles';
import FormControlLabel from '@mui/material/FormControlLabel';
import Switch from '@mui/material/Switch';
import {ReactFlow, ReactFlowProvider, Controls, MarkerType, getViewportForBounds, useReactFlow, useStore} from '@xyflow/react';
import dagre from '@dagrejs/dagre';
import {useNavigate} from 'react-router-dom';

import '@xyflow/react/dist/style.css';

// The box a page is drawn in. Given to dagre before anything is drawn, because a layered layout has
// to know how much room each node takes to decide where the layers go.
const NODE_WIDTH = 180;
const NODE_HEIGHT = 40;

// Where a click puts the reader: close enough to read the page's neighbours, and not so close that
// the rest of the graph leaves the screen.
const FOCUS_ZOOM = 1.25;
const FOCUS_MS = 400;

// How far the view may be zoomed. Out far enough to hold a program of the standard library and
// everything above it, which is some thirty pages and does not fit at the half scale React Flow
// would otherwise stop at; in no further than a page is drawn at, since there is nothing in a page
// to read closer.
const MIN_ZOOM = 0.2;
const MAX_ZOOM = 1.5;
// The margin left around the graph when the whole of it is shown, as a share of the room it takes.
const FIT_PADDING = 0.1;

const useStyles = makeStyles({
  graph: {
    height: '36em',
    border: '1px solid #ddd',
    // The dots a node would be joined to another by. Nothing here is joined by hand -- the edges are
    // the imports the pages write -- so they are handles for a gesture the graph does not take, and
    // drawing them says a page can be wired to another.
    "& .react-flow__handle": {
      opacity: 0
    }
  },
  controls: {
    display: 'flex',
    alignItems: 'center',
    gap: '1em'
  },
  hint: {
    fontSize: '0.85em',
    opacity: 0.7
  },
  empty: {
    padding: '1em',
    opacity: 0.7
  },
  // The box a page is drawn in. Put on the node itself rather than on something inside it, since
  // React Flow draws a box of its own around whatever a node is given as its label -- so a box laid
  // inside that is two boxes for one page.
  node: {
    width: NODE_WIDTH,
    height: NODE_HEIGHT,
    padding: '4px 8px',
    boxSizing: 'border-box',
    display: 'flex',
    alignItems: 'center',
    justifyContent: 'center',
    fontSize: '0.85em',
    border: '1px solid #999',
    borderRadius: '4px',
    background: '#fff'
  },
  // A page's name is cut rather than wrapped or spilled: the box is one line high, and a long name
  // in it would otherwise be drawn over its neighbours.
  label: {
    overflow: 'hidden',
    whiteSpace: 'nowrap',
    textOverflow: 'ellipsis',
    maxWidth: '100%'
  },
  // A page of the standard library, which is drawn but is not what the reader came for.
  core: {
    background: '#f4f4f4',
    color: '#666',
    borderStyle: 'dashed'
  },
  // The page the graph was drawn for, which is where a reader looking at their own program starts.
  root: {
    borderWidth: '2px',
    borderColor: '#1976d2',
    fontWeight: 'bold'
  },
  selected: {
    borderColor: '#d32f2f',
    borderWidth: '2px',
    background: '#fff3f0'
  },
  dimmed: {
    opacity: 0.35
  }
});

// The pages a graph draws, given which the reader asked to see. The standard library is left out
// unless it was asked for, because a program of three pages resting on twenty of the library reads
// as a graph of the library with something attached.
//
// The root is kept whatever was asked, since a graph of a program with the page it is of missing is
// a graph of something else. A root which is itself the library's is therefore the one library page
// drawn when the library is not being shown.
function visiblePages(pages, showCore) {
  return showCore ? pages : pages.filter(page => !page.pgnCore || page.pgnRoot);
}

// The graph laid out, as the nodes and edges to draw it from.
//
// Edges to pages which are not being drawn are dropped rather than drawn to nothing. Nothing else
// has to be pruned: a page is drawn or it is not, and an edge is between two pages.
//
// dagre is asked top to bottom, so a page sits above what it imports. That is the direction the
// arrows already read in, and it is the direction a reader coming from a page's import list expects
// to find what it named.
function layout(pages) {
  const shown = new Set(pages.map(page => page.pgnPath));
  const graph = new dagre.graphlib.Graph();
  graph.setGraph({rankdir: 'TB', nodesep: 24, ranksep: 64});
  graph.setDefaultEdgeLabel(() => ({}));

  pages.forEach(page => graph.setNode(page.pgnPath, {width: NODE_WIDTH, height: NODE_HEIGHT}));
  const edges = [];
  pages.forEach(page => {
    page.pgnImports.filter(to => shown.has(to)).forEach(to => {
      graph.setEdge(page.pgnPath, to);
      edges.push({id: `${page.pgnPath}->${to}`, source: page.pgnPath, target: to});
    });
  });

  dagre.layout(graph);

  // dagre positions a node by its centre and React Flow by its top left corner.
  const nodes = pages.map(page => {
    const {x, y} = graph.node(page.pgnPath);
    return {
      id: page.pgnPath,
      data: {page},
      position: {x: x - NODE_WIDTH / 2, y: y - NODE_HEIGHT / 2}
    };
  });

  // The room the whole graph takes, in the coordinates the nodes are laid out in. Worked out here
  // because it is the layout that knows it: React Flow works bounds out from the pages it has
  // MEASURED and drops the rest, so it cannot say what a set of pages it has only just been handed
  // takes up -- which is exactly when a view of the whole of them is wanted.
  const bounds = {
    x: Math.min(...nodes.map(node => node.position.x)),
    y: Math.min(...nodes.map(node => node.position.y)),
    width: Math.max(...nodes.map(node => node.position.x)) - Math.min(...nodes.map(node => node.position.x)) + NODE_WIDTH,
    height: Math.max(...nodes.map(node => node.position.y)) - Math.min(...nodes.map(node => node.position.y)) + NODE_HEIGHT
  };

  return {nodes, edges, bounds};
}

// The pages of a program and the imports between them, drawn as a graph.
//
// The whole graph is the compiled value of a #prgmGraph annotation, so nothing is fetched here and
// nothing is asked again when the reader turns it over: showing the standard library and hiding it
// are two drawings of the one answer.
function PrgmGraph(props) {
  return (
    // The provider is what lets the graph below move the view when a page is picked. It is here
    // rather than inside because a hook cannot reach a provider its own component renders.
    <ReactFlowProvider>
      <PrgmGraphView {...props} />
    </ReactFlowProvider>
  );
}

function PrgmGraphView(props) {
  const classes = useStyles();
  const navigate = useNavigate();
  const {setCenter, setViewport} = useReactFlow();
  // How much room the graph has to draw in, which is what a view of the whole of it is fitted into.
  const paneWidth = useStore(store => store.width);
  const paneHeight = useStore(store => store.height);
  const [showCore, setShowCore] = useState(false);
  const [selected, setSelected] = useState(null);

  // The only field of Show/PrgmGraph, so a compiled value always carries the whole graph.
  const graph = props["/graph"];
  const pages = useMemo(() => JSON.parse(graph.contents), [graph]);

  const {nodes, edges, bounds} = useMemo(() => layout(visiblePages(pages, showCore)), [pages, showCore]);

  // The view is brought to the whole graph whenever which pages are drawn changes. Laying the pages
  // out again moves them, so a view left where it was is a view of where the pages were: turning the
  // standard library on makes the graph several times the size it was, and a reader left where they
  // were is looking at the empty canvas beside it.
  //
  // Set outright rather than asked of React Flow's own fitView, which fits the pages it has MEASURED
  // and so fits nothing at all when handed a set it has only just drawn. The layout above is this
  // component's own, so where the pages are and how much room they take is already known and nothing
  // has to be measured to say it.
  //
  // Keyed on the laid-out pages, which change when which pages are drawn changes and not when one is
  // picked -- moving the view then would undo the very move that picking a page makes -- and on the
  // pane, which is what a graph is fitted into and is zero until it has been laid out.
  useEffect(() => {
    if (paneWidth && paneHeight) {
      setViewport(getViewportForBounds(bounds, paneWidth, paneHeight, MIN_ZOOM, MAX_ZOOM, FIT_PADDING));
    }
  }, [bounds, paneWidth, paneHeight, setViewport]);

  // Which pages the picked one is joined to, so that what it imports and what imports it are what
  // stays lit when everything else is dimmed. A graph of thirty pages says nothing about any one of
  // them until the rest is out of the way.
  //
  // A page picked and then hidden -- by turning the library off with one of its pages picked -- is
  // nothing being picked, rather than a pick joined to nothing which would dim the whole graph.
  const neighbours = useMemo(() => {
    if (!selected || !nodes.some(node => node.id === selected)) {
      return null;
    }
    const joined = new Set([selected]);
    edges.forEach(edge => {
      if (edge.source === selected) joined.add(edge.target);
      if (edge.target === selected) joined.add(edge.source);
    });
    return joined;
  }, [selected, nodes, edges]);

  const shownNodes = useMemo(() => nodes.map(node => {
    const {page} = node.data;
    const lit = !neighbours || neighbours.has(node.id);
    const classNames = [
      classes.node,
      page.pgnCore ? classes.core : '',
      page.pgnRoot ? classes.root : '',
      node.id === selected ? classes.selected : '',
      lit ? '' : classes.dimmed
    ].filter(Boolean).join(' ');
    return {
      ...node,
      className: classNames,
      // A page is labelled by its file name, which is all a box this size holds; the path it is at
      // is on the box itself, so pointing at one says which of two same-named pages it is.
      data: {
        label: <span className={classes.label} title={page.pgnPath}>{page.pgnName}</span>
      }
    };
  }), [nodes, selected, neighbours, classes]);

  const shownEdges = useMemo(() => edges.map(edge => {
    const lit = !neighbours || edge.source === selected || edge.target === selected;
    return {
      ...edge,
      markerEnd: {type: MarkerType.ArrowClosed},
      style: lit ? undefined : {opacity: 0.15}
    };
  }), [edges, selected, neighbours]);

  // A click picks a page and brings the view to it, which is what makes a graph too big to read at
  // once readable one page at a time.
  const pick = useCallback((event, node) => {
    setSelected(node.id);
    setCenter(node.position.x + NODE_WIDTH / 2, node.position.y + NODE_HEIGHT / 2,
      {zoom: FOCUS_ZOOM, duration: FOCUS_MS});
  }, [setCenter]);

  // A double click opens the page itself, which is the same link the navigation builds. Picking is
  // what a click does and opening is what a second one does, so a reader who wants to look at a
  // page's neighbours is not taken away from the graph to do it.
  const open = useCallback((event, node) => {
    navigate(`/docs/${encodeURIComponent(node.id)}`);
  }, [navigate]);

  return (
    <div>
      <div className={classes.controls}>
        <FormControlLabel
          control={<Switch checked={showCore} onChange={e => setShowCore(e.target.checked)} size="small" />}
          label="Standard library"
        />
        <span className={classes.hint}>Click a page to focus it, double click to open it.</span>
      </div>
      {shownNodes.length === 0 ? (
        <div className={classes.empty}>Every page of this program is part of the standard library.</div>
      ) : (
        <div className={classes.graph} data-testid="prgm-graph">
          <ReactFlow
            nodes={shownNodes}
            edges={shownEdges}
            onNodeClick={pick}
            onNodeDoubleClick={open}
            onPaneClick={() => setSelected(null)}
            nodesDraggable={false}
            nodesConnectable={false}
            // A double click opens the page and does nothing else. React Flow's own use for the
            // gesture is to zoom the canvas, and it takes the gesture before anything else sees it,
            // so leaving it on means a page never opens and means one gesture doing two things.
            // Zooming is still on the wheel and on the buttons, which is where a reader looks for it.
            zoomOnDoubleClick={false}
            minZoom={MIN_ZOOM}
            maxZoom={MAX_ZOOM}
          >
            <Controls showInteractive={false} />
          </ReactFlow>
        </div>
      )}
    </div>
  );
}

export default PrgmGraph;
export {visiblePages, layout};
