import React, {createContext, useContext} from 'react';
import {Link as RouterLink} from 'react-router-dom';

import Box from '@mui/material/Box';
import IconButton from '@mui/material/IconButton';
import Link from '@mui/material/Link';
import Paper from '@mui/material/Paper';
import Popper from '@mui/material/Popper';
import Typography from '@mui/material/Typography';
import CloseIcon from '@mui/icons-material/Close';
import MenuBookIcon from '@mui/icons-material/MenuBook';

import {useApi} from '../Common';
import {ObjArr} from '../Semantics';

// The file a debugged expression is parsed as (`CtService.debugExprRun`). It is no page: its source
// is the expression the debugger was asked to run, which the debugger already holds.
const DEBUG_FILE = "<debug>";

// Which call's source is open, and how a book opens one. Held by the Debugger so that only one is
// open at a time, whichever row it was opened from.
const SourceContext = createContext({open: null, toggle: () => {}, expr: ""});

// Where an arrow is written: the file, and the first and last place in it, as [name, start, end] with
// the start and end being the parser's {line, col}. Null for an arrow with nothing positioned in it.
//
// Taken over every position inside the arrow rather than read off its object. A call is positioned
// by the parser as the argument list it is applied to (`applyTermSuffix` in
// src/Syntax/Ct/Parser/Expr.hs), so the object `foldr(lst= Nil, ...)` alone says it starts at the
// paren and leaves the name out. The name is positioned as a value of its own, inside it.
//
// Only positions in the file the arrow's own head is written in count, so an argument that arrived
// from somewhere else does not stretch the span over a file it is not in.
function arrowSpan(oa) {
  const positions = [];
  collectPositions(oa.oaObj, positions);
  collectPositions(oa.oaArr, positions);
  if (!positions.length) {
    return null;
  }
  const file = positions[0][0].name;
  const inFile = positions.filter(([start]) => start.name === file);
  const start = inFile.map(p => p[0]).reduce((a, b) => before(b, a) ? b : a);
  const end = inFile.map(p => p[1]).reduce((a, b) => before(a, b) ? b : a);
  return [file, start, end];
}

// Every source position held by a meta anywhere in a value, in the order they are reached. A meta's
// type and data are not walked: neither is written anywhere, and the data of a built expression holds
// whole other trees.
function collectPositions(value, into) {
  if (Array.isArray(value)) {
    value.forEach(v => collectPositions(v, into));
  } else if (value && typeof value === 'object') {
    if ('getMetaPos' in value) {
      if (value.getMetaPos) {
        into.push(value.getMetaPos);
      }
      return;
    }
    Object.values(value).forEach(v => collectPositions(v, into));
  }
}

function before(a, b) {
  return a.line < b.line || (a.line === b.line && a.col < b.col);
}

// The page a span is written in, opened on its source with the span selected. None for the debugged
// expression, which is no page.
function sourceUrl(span) {
  if (!span || span[0] === DEBUG_FILE) {
    return null;
  }
  const [file, start, end] = span;
  const at = new URLSearchParams({line: start.line, col: start.col, endLine: end.line, endCol: end.col});
  return `/docs/${encodeURIComponent(file)}?${at}`;
}

function showSpan(span) {
  return `${span[0].split("/").pop()}:${span[1].line}`;
}

// The book at the start of a debugged call's line, standing for where the call's definition is
// written. A click opens that source over the rows below, leaving the line itself -- what the call
// was given and what it gave back -- in view. A ctrl-click is a link's own: the page the definition
// is written in, in a new tab, opened on its source.
function SourceButton(props) {
  const {id, oa} = props;
  const {open, toggle} = useContext(SourceContext);
  const span = arrowSpan(oa);
  const url = sourceUrl(span);

  const onClick = event => {
    // The row is the tree's to open and select, and this is a click on something inside it.
    event.stopPropagation();
    if (url && (event.ctrlKey || event.metaKey)) {
      return;
    }
    event.preventDefault();
    toggle(id, oa, event.currentTarget.closest('.MuiTreeItem-content') ?? event.currentTarget);
  };

  const label = span ? `Source at ${showSpan(span)}` : "Source";
  return (
    <IconButton
      className="catln-debug-source"
      size="small"
      aria-label={label}
      title={url ? `${label} (ctrl-click opens the page)` : label}
      aria-pressed={open?.id === id}
      color={open?.id === id ? "primary" : "default"}
      sx={{padding: '0 2px', marginRight: '0.4em', verticalAlign: 'text-bottom'}}
      onClick={onClick}
      onMouseDown={event => event.stopPropagation()}
      {...(url ? {component: 'a', href: url} : {})}
    >
      <MenuBookIcon fontSize="inherit" />
    </IconButton>
  );
}

// The source of the call opened by a book, laid under the line it was opened from rather than over
// the page: the line is what the reader was reading, and what the call was given and gave back is on
// it. It is not modal -- the tree stays usable underneath -- and there is one at a time.
function SourcePanel() {
  const {open, toggle} = useContext(SourceContext);
  if (!open) {
    return null;
  }
  const span = arrowSpan(open.oa);
  const url = sourceUrl(span);

  return (
    <Popper
      open
      anchorEl={open.anchor}
      placement="bottom-start"
      sx={{zIndex: theme => theme.zIndex.modal}}
      modifiers={[{name: 'offset', options: {offset: [24, 4]}}]}
    >
      <Paper
        elevation={8}
        role="dialog"
        aria-label="Source"
        className="catln-debug-source-panel"
        onKeyDown={event => { if (event.key === 'Escape') toggle(open.id); }}
        sx={{maxWidth: 'min(60em, 90vw)', maxHeight: '50vh', overflow: 'auto'}}
      >
        <Box sx={{display: 'flex', alignItems: 'center', gap: 1, px: 1.5, py: 0.5, borderBottom: 1, borderColor: 'divider'}}>
          <MenuBookIcon fontSize="small" color="action" />
          <Typography variant="body2" sx={{flexGrow: 1, fontFamily: 'monospace'}}>
            {!span ? "No source position" : url
              ? <Link component={RouterLink} to={url} title="Open the page this is written in">{showSpan(span)}</Link>
              : showSpan(span)}
          </Typography>
          <IconButton size="small" aria-label="Close source" onClick={() => toggle(open.id)} autoFocus>
            <CloseIcon fontSize="small" />
          </IconButton>
        </Box>
        <Box sx={{px: 1.5, py: 1}}>
          {span ? <SpanSource span={span} /> : <ObjArr oa={open.oa} />}
        </Box>
      </Paper>
    </Popper>
  );
}

// The lines a span is written on, with the span itself marked. The debugged expression is read from
// the debugger; a page's source is asked of the server, as the page it is.
function SpanSource(props) {
  const {span} = props;
  const {expr} = useContext(SourceContext);
  const isPage = span[0] !== DEBUG_FILE;
  const page = useApi(isPage ? `/api/page?prgmName=${encodeURIComponent(span[0])}` : null);

  if (!isPage) {
    return <SourceLines source={expr} span={span} />;
  }
  if (page.error) {
    return <Typography variant="body2">Could not read {span[0]}: {page.error.message}</Typography>;
  }
  if (!page.isLoaded) {
    return <Typography variant="body2">Loading...</Typography>;
  }
  if (!page.data?.pgSource) {
    return <Typography variant="body2">The server sent no source for {span[0]}.</Typography>;
  }
  return <SourceLines source={page.data.pgSource} span={span} />;
}

// Columns are the parser's: counted from one, with the end just past the last character.
function SourceLines(props) {
  const {source, span} = props;
  const [, start, end] = span;
  const lines = source.split("\n").slice(start.line - 1, end.line);
  const width = String(end.line).length;

  return (
    // Wrapped rather than scrolled, so that a span far along a long line is on screen when it opens.
    <Box component="pre" sx={{margin: 0, fontSize: '0.875rem', lineHeight: 1.5, whiteSpace: 'pre-wrap', overflowWrap: 'anywhere'}}>
      {lines.map((line, i) => {
        const lineNo = start.line + i;
        const from = lineNo === start.line ? start.col - 1 : 0;
        const to = lineNo === end.line ? end.col - 1 : line.length;
        return (
          <div key={lineNo}>
            <Box component="span" sx={{opacity: 0.5, userSelect: 'none'}}>{String(lineNo).padStart(width)}  </Box>
            {line.slice(0, from)}
            <Box component="mark" className="catln-debug-source-span" sx={{backgroundColor: 'action.selected', color: 'inherit'}}>
              {line.slice(from, to)}
            </Box>
            {line.slice(to)}
          </div>
        );
      })}
    </Box>
  );
}

export {SourceContext, SourceButton, SourcePanel, arrowSpan};
