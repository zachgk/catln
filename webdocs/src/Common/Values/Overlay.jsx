import React, {useState, useEffect, useRef, useCallback} from 'react';

import makeStyles from '@mui/styles/makeStyles';

// The value an #overlay annotation compiles to, and the address it carries. Read here rather than at
// each place that wants it, so what the compiled value looks like is known in one place.
const OVERLAY_VALUE = "/Catln/Doc/Show/Overlay";

function overlayValueUrl(value) {
  if (!value || value.tag !== "TupleVal" || value.name !== OVERLAY_VALUE) {
    return null;
  }
  return value.args["/url"]?.contents ?? null;
}

// Where the reader last put the divider. Kept for the whole doc server rather than per page: it is
// how much of the window this reader wants their own writing to have, which is the same wherever
// they are reading.
const SPLIT_KEY = "catln.webdocs.overlaySplit";
const MIN_RATIO = 0.15;
const MAX_RATIO = 0.85;
const DEFAULT_RATIO = 0.5;
const KEY_STEP = 0.05;

function clampRatio(r) {
  return Math.min(MAX_RATIO, Math.max(MIN_RATIO, r));
}

// Anything unreadable opens the two halves even, which is what a first visit with nothing stored
// should do.
function readRatio() {
  const stored = Number(window.localStorage.getItem(SPLIT_KEY));
  return stored > 0 ? clampRatio(stored) : DEFAULT_RATIO;
}

const useStyles = makeStyles({
  marker: {
    fontSize: '0.9em'
  },
  markerLabel: {
    marginRight: '0.5em',
    opacity: 0.7
  },
  split: {
    display: 'flex',
    flexDirection: 'column',
    height: '80vh'
  },
  splitDragging: {
    userSelect: 'none'
  },
  notes: {
    flexBasis: 0,
    minHeight: 0,
    overflow: 'auto',
    display: 'flex',
    flexDirection: 'column'
  },
  divider: {
    flexShrink: 0,
    height: '6px',
    cursor: 'row-resize',
    background: '#ddd',
    "&:hover": {
      background: '#bbb'
    },
    "&:focus-visible": {
      outline: '2px solid #1976d2'
    }
  },
  frame: {
    flexBasis: 0,
    minHeight: 0,
    display: 'flex',
    flexDirection: 'column'
  },
  frameHeader: {
    display: 'flex',
    alignItems: 'baseline',
    gap: '1em',
    padding: '2px 4px',
    fontSize: '0.8em',
    background: '#eee'
  },
  frameHint: {
    opacity: 0.7
  },
  iframe: {
    border: 0,
    width: '100%',
    flexGrow: 1,
    minHeight: 0
  },
  paneDragging: {
    pointerEvents: 'none'
  }
});

// What an #overlay displays as wherever a value is shown: the address it names, as a link.
//
// Never a frame. The page at that address is framed by the page laying itself out around it
// (OverlaySplit, used by DocsPage), so an annotation which appears both in a page's statements and
// as that page's overlay is one frame and one link rather than two frames. It is also what an
// overlay shown on its own compiles to -- through the annotation route, or as a zone in the editor
// -- where there are no notes to write over anything and so nothing to lay out.
function Overlay(props) {
  const url = props["/url"].contents;
  const classes = useStyles();

  return (
    <span className={classes.marker}>
      <span className={classes.markerLabel}>Written over</span>
      <a href={url} target="_blank" rel="noopener noreferrer">{url}</a>
    </span>
  );
}

// The page a set of notes is written over, shown beneath them.
//
// A site may refuse to be shown inside another page, and refuses in a header this page is not
// allowed to read: no error is raised, and reaching into the frame to see whether anything arrived
// throws, because the frame is another origin. So there is no telling a refusal from a slow load,
// and the way through is offered always rather than only once something is known to have gone
// wrong. That is why the address is a link here and why the hint below it is written as a standing
// fact rather than as a report.
function OverlayFrame(props) {
  const {url} = props;
  const classes = useStyles();

  return (
    <>
      <div className={classes.frameHeader}>
        <a href={url} target="_blank" rel="noopener noreferrer">Open in a new tab ↗</a>
        <span className={classes.frameHint}>
          If nothing appears, the site refuses to be shown inside another page — open it in a new tab.
        </span>
      </div>
      <iframe
        className={classes.iframe}
        src={url}
        title={`Overlay of ${url}`}
        referrerPolicy="no-referrer"
      />
    </>
  );
}

// A page written over another one: the writing above, the page it is written over below, and a
// divider the reader moves to give one of them more of the window.
//
// The children are whatever the page shows of itself, which is why the mode a page is being read or
// written in is settled before it gets here: the frame is then a sibling that does not change when
// the notes above it do, so it is never remounted and the page in it keeps its scrolling, its
// session and its place while notes are written against it.
function OverlaySplit(props) {
  const {url, children} = props;
  const [ratio, setRatio] = useState(readRatio);
  const [dragging, setDragging] = useState(false);
  const containerRef = useRef(null);
  const classes = useStyles();

  useEffect(() => {
    window.localStorage.setItem(SPLIT_KEY, String(ratio));
  }, [ratio]);

  const moveTo = useCallback(clientY => {
    const box = containerRef.current?.getBoundingClientRect();
    if (!box || !box.height) return;
    setRatio(clampRatio((clientY - box.top) / box.height));
  }, []);

  // The pointer is followed on the window rather than on the divider, because a drag is meant to
  // carry on wherever it goes -- including over the frame, which is another origin and would
  // otherwise swallow it. While it is live both panes stop taking pointer events at all, so the
  // frame cannot take the pointer away mid-drag and leave the divider stuck to it.
  //
  // Followed from the moment the pointer goes down rather than from a render that knows it has,
  // because a drag begun and finished inside one frame would otherwise be a drag nothing was
  // listening to. So this is attached here rather than by an effect watching dragging, which the
  // pointer can outrun; dragging says only how the panes are to be drawn while it lasts.
  const stopDragRef = useRef(null);

  const startDrag = e => {
    e.preventDefault();
    const onMove = ev => moveTo(ev.clientY);
    const stop = () => {
      window.removeEventListener('mousemove', onMove);
      window.removeEventListener('mouseup', stop);
      stopDragRef.current = null;
      setDragging(false);
    };
    window.addEventListener('mousemove', onMove);
    window.addEventListener('mouseup', stop);
    stopDragRef.current = stop;
    setDragging(true);
  };

  // A page navigated away from mid-drag leaves nothing following the pointer.
  useEffect(() => () => stopDragRef.current?.(), []);

  const onKeyDown = e => {
    if (e.key === 'ArrowUp') {
      e.preventDefault();
      setRatio(r => clampRatio(r - KEY_STEP));
    } else if (e.key === 'ArrowDown') {
      e.preventDefault();
      setRatio(r => clampRatio(r + KEY_STEP));
    }
  };

  const paneClass = dragging ? classes.paneDragging : undefined;

  return (
    <div
      ref={containerRef}
      className={dragging ? `${classes.split} ${classes.splitDragging}` : classes.split}
      data-testid="overlay-split"
    >
      <div className={`${classes.notes} ${paneClass ?? ''}`} style={{flexGrow: ratio}}>
        {children}
      </div>
      <div
        className={classes.divider}
        role="separator"
        aria-orientation="horizontal"
        aria-label="Resize the overlay"
        aria-valuenow={Math.round(ratio * 100)}
        aria-valuemin={Math.round(MIN_RATIO * 100)}
        aria-valuemax={Math.round(MAX_RATIO * 100)}
        tabIndex={0}
        onMouseDown={startDrag}
        onKeyDown={onKeyDown}
      />
      <div className={`${classes.frame} ${paneClass ?? ''}`} style={{flexGrow: 1 - ratio}}>
        <OverlayFrame url={url} />
      </div>
    </div>
  );
}

export default Overlay;
export {OverlayFrame, OverlaySplit, OVERLAY_VALUE, overlayValueUrl};
