import React, {useState, useEffect, useRef, useContext, useCallback} from 'react';
import {createRoot} from 'react-dom/client';

import makeStyles from '@mui/styles/makeStyles';
import Button from '@mui/material/Button';
import IconButton from '@mui/material/IconButton';
import Menu from '@mui/material/Menu';
import MenuItem from '@mui/material/MenuItem';
import EditIcon from '@mui/icons-material/Edit';
import VisibilityIcon from '@mui/icons-material/Visibility';
import PlayArrowIcon from '@mui/icons-material/PlayArrow';
import Editor from '@monaco-editor/react';
import {initVimMode, VimMode} from 'monaco-vim';
import {Link, useNavigate} from 'react-router-dom';

import {rawExprMeta, RawExpr, RawObjArr} from './Common/Syntax';
import {useQuery, useApi, useLiveApi, Loading, KeyWord, ReachesTree, ConfigContext, PrgmNameContext, PageDirContext, ReloadContext, annotLink, annotApi, writeExpr} from './Common/Common';
import {Value} from './Common/Value';
import DebuggerMenu from './Common/DebuggerMenu';
import {OverlaySplit, overlayValueUrl} from './Common/Values/Overlay';

const DEBUG_NO_EVAL = false;

const useStyles = makeStyles({
  indented: {
    marginLeft: '4em'
  },
  dirButtons: {
    marginTop: '50px',
    width: '100%',
    display: 'flex',
    justifyContent: 'space-evenly'
  },
  playIcon: {
    color: 'green',
    cursor: 'pointer'
  },
  noPlay: {
    marginLeft: '24px'
  },
  // monaco-vim owns the contents of vimStatusBar, so the save status is a sibling
  // inside statusRow rather than a child of it.
  statusRow: {
    display: 'flex',
    justifyContent: 'space-between',
    fontFamily: 'monospace',
    fontSize: '13px',
    padding: '2px 4px',
    background: '#eee'
  },
  vimStatusBar: {
    flexGrow: 1
  },
  saveStatus: {
    marginLeft: '1em',
    whiteSpace: 'pre'
  },
  saveStatusError: {
    color: 'darkred'
  },
  // On a page written over another one the editor is given the pane rather than a height of its
  // own, and the status row keeps the bottom of it.
  editorFilling: {
    display: 'flex',
    flexDirection: 'column',
    flexGrow: 1,
    minHeight: 0
  },
  editorPane: {
    flexGrow: 1,
    minHeight: 0
  }
});

// Every value the compiler hung off a statement of a page, beside where the page writes it, in the
// order the page writes them. What a page says about itself is read from here rather than from a
// walk of its own, so a page is walked once however many of its values are wanted.
function statementValues(statementTrees) {
  const values = [];

  function walkStatementTree(statementTree) {
    const [statement, subStatements] = statementTree;

    const expr = statement.tag === "RawDeclStatement" ? statement.contents.roaObj
               : statement.tag === "RawAnnot" ? statement.contents
               : null;
    if (expr) {
      const m = rawExprMeta(expr);
      if (m.getMetaDat[2]) {
        values.push({pos: m.getMetaPos, value: m.getMetaDat[2]});
      }
    }

    subStatements.forEach(walkStatementTree);
  }

  statementTrees.forEach(walkStatementTree);
  return values;
}

// Collect [{line, startLine, value}] for all expressions that have a pre-computed annotation value.
// line: the afterLineNumber for the ViewZone (last content line)
// startLine: the first line of the annotation (for cursor-based hiding)
//
// A zone hangs off a line, so only a value the page was parsed for has one. What a syntax builds
// rather than parses -- a note's writing, its tasks, its properties -- carries no position and gets
// no zone.
function extractLineValues(statementTrees) {
  return statementValues(statementTrees)
    .filter(({pos}) => pos)
    .map(({pos, value}) => ({line: pos[1].line, startLine: pos[0].line, value}));
}

// The page this page is written over, if it says it is written over one. The first it names: a page
// is laid out around one other page, so a page writing two overlays has to be read as one of them.
function overlayUrl(statementTrees) {
  return statementValues(statementTrees).map(({value}) => overlayValueUrl(value)).find(Boolean) ?? null;
}

function DocsPage(props) {
  const {prgmName} = props;

  const [reloadKey, setReloadKey] = useState(0);
  // Followed rather than asked about once: the server holds the question open and answers it again
  // whenever the page, or a page it imports, is written to. A change to any other page is one the
  // server never sends here, so the window a reader is reading is not disturbed by an edit elsewhere.
  let apiResult = useLiveApi(`/api/page?prgmName=${prgmName}`, reloadKey);
  const [mode, setMode] = useState('view');
  const [editedSource, setEditedSource] = useState(null);
  const [editorInstance, setEditorInstance] = useState(null);
  const [lastSaved, setLastSaved] = useState(null);
  const [saveState, setSaveState] = useState(null);
  const classes = useStyles();

  const config = useContext(ConfigContext);
  const canEdit = config.isLoaded && !config.data?.readonly;

  // Map from zone id -> {root, observer, zone, line, naturalHeight, hidden}
  const viewZonesRef = useRef(new Map());
  const cursorListenerRef = useRef(null);
  const vimStatusBarRef = useRef(null);
  const saveRef = useRef(null);

  const toggleMode = () => setMode(m => m === 'view' ? 'edit' : 'view');

  // Reading the page again is what a value shown on it does after writing to the compiler, so it is
  // offered to every value rather than only to the ones this page hands an updateExpr to.
  const reload = useCallback(() => setReloadKey(k => k + 1), []);

  // The page, and what the server knows about where it sits: the pages either side of it in the
  // order the navigation shows them, and the directory its documentation's relative links resolve
  // against. Either neighbour is absent at the ends of that order.
  const prgmData = apiResult.data?.pgPrgm ?? null;
  const sourceText = apiResult.data?.pgSource ?? null;
  const prevPage = apiResult.data?.pgPrev ?? null;
  const nextPage = apiResult.data?.pgNext ?? null;
  const pageDir = apiResult.data?.pgDir ?? "";
  const currentSource = editedSource !== null ? editedSource : sourceText;
  const isDirty = currentSource !== (lastSaved !== null ? lastSaved : sourceText);

  // Reset edit state whenever the page changes
  useEffect(() => {
    setEditedSource(null);
    setLastSaved(null);
    setSaveState(null);
  }, [prgmName]);

  // Reassigned every render so the keybindings, which are registered once, always
  // run against the current buffer.
  saveRef.current = async () => {
    if (!canEdit || !isDirty || currentSource === null) return;
    const src = currentSource;
    setSaveState({status: 'saving'});
    try {
      const res = await fetch(`/api/write?prgmName=${prgmName}`, {
        method: 'POST',
        headers: {'Content-Type': 'text/plain'},
        body: src
      });
      const json = await res.json();
      if (res.status === 200 && json.tag === 'Success') {
        setLastSaved(src);
        setSaveState({status: 'saved'});
        setReloadKey(k => k + 1); // refetch the page so notes and values reflect the save
      } else {
        const notes = json.tag === 'ResFail' ? json.contents : null;
        const msg = notes?.map(n => n.msg).join('; ') || res.statusText || 'unknown error';
        setSaveState({status: 'error', message: msg});
      }
    } catch (e) {
      setSaveState({status: 'error', message: e.message});
    }
  };

  // Replaces one expression of the page and saves the page back as the file it was read from. This
  // is what a value shown on the page edits: it says what its expression should now be, as Catln
  // source, and the server writes the file in whatever syntax the page is written in. So checking a
  // task of an Obsidian note writes markdown, without the page having to know that it is markdown.
  //
  // The save is reported where a save from the editor is reported, and the page is refetched after
  // it so the value shows what the file now says rather than what was asked for.
  const updateExpr = async (metaID, src) => {
    setSaveState({status: 'saving'});
    const res = await writeExpr(prgmName, metaID, src);
    if (res.ok) {
      setSaveState({status: 'saved'});
      setReloadKey(k => k + 1);
    } else {
      setSaveState({status: 'error', message: res.message});
    }
  };

  const handleEditorMount = (editor, monaco) => {
    editor.addCommand(monaco.KeyMod.CtrlCmd | monaco.KeyCode.KeyS, () => saveRef.current?.());
    setEditorInstance(editor);
    editor.onDidDispose(() => {
      setEditorInstance(null);
      viewZonesRef.current.forEach(({root, observer}) => {
        try { root.unmount(); } catch(e) {}
        try { observer.disconnect(); } catch(e) {}
      });
      viewZonesRef.current.clear();
      if (cursorListenerRef.current) {
        cursorListenerRef.current.dispose();
        cursorListenerRef.current = null;
      }
    });
  };

  // Attach vim keybindings for as long as the editor is mounted. defineEx registers on
  // the shared Vim singleton rather than this editor, so it dispatches through saveRef.
  useEffect(() => {
    if (!editorInstance || mode !== 'edit' || !vimStatusBarRef.current) return;
    const vimMode = initVimMode(editorInstance, vimStatusBarRef.current);
    VimMode.Vim.defineEx('write', 'w', () => saveRef.current?.());
    return () => {
      try { vimMode.dispose(); } catch(e) {}
    };
  }, [editorInstance, mode]);

  // Add/remove ViewZones whenever the editor, mode, or data changes
  useEffect(() => {
    const editor = editorInstance;

    // Clean up existing zones and cursor listener
    if (cursorListenerRef.current) {
      cursorListenerRef.current.dispose();
      cursorListenerRef.current = null;
    }
    if (editor && viewZonesRef.current.size > 0) {
      try {
        editor.changeViewZones(acc => {
          viewZonesRef.current.forEach((_, id) => acc.removeZone(id));
        });
      } catch(e) {}
    }
    viewZonesRef.current.forEach(({root, observer}) => {
      try { root.unmount(); } catch(e) {}
      try { observer.disconnect(); } catch(e) {}
    });
    viewZonesRef.current.clear();

    if (!editor || mode !== 'edit' || !prgmData) return;

    const [, statements] = prgmData;
    const allValues = extractLineValues(statements);

    if (allValues.length === 0) return;

    editor.changeViewZones(accessor => {
      allValues.forEach(({line, startLine, value}) => {
        const domNode = document.createElement('div');
        Object.assign(domNode.style, {
          paddingLeft: '60px',
          fontFamily: 'monospace',
          fontSize: '13px',
          color: '#2a7a2a',
        });

        const root = createRoot(domNode);
        root.render(<Value data={value} />);

        const zone = {afterLineNumber: line, heightInPx: 19, domNode};
        const id = accessor.addZone(zone);
        const zd = {root, observer: null, zone, line, startLine, naturalHeight: 19, hidden: false};
        viewZonesRef.current.set(id, zd);

        const observer = new ResizeObserver(() => {
          if (zd.hidden) return;
          const h = domNode.scrollHeight;
          if (h > 0 && h !== zd.naturalHeight) {
            zd.naturalHeight = h;
            zone.heightInPx = h;
            try { editor.changeViewZones(acc => acc.layoutZone(id)); } catch(e) {}
          }
        });
        observer.observe(domNode);
        zd.observer = observer;
      });
    });

    // Hide a zone when the cursor enters its line; restore when it leaves
    cursorListenerRef.current = editor.onDidChangeCursorPosition(e => {
      const cursorLine = e.position.lineNumber;
      viewZonesRef.current.forEach((zd, id) => {
        const shouldHide = cursorLine >= zd.startLine && cursorLine <= zd.line;
        if (shouldHide === zd.hidden) return;
        zd.hidden = shouldHide;
        zd.zone.heightInPx = shouldHide ? 0 : zd.naturalHeight;
        zd.zone.domNode.style.display = shouldHide ? 'none' : '';
        try { editor.changeViewZones(acc => acc.layoutZone(id)); } catch(e) {}
      });
    });
  }, [editorInstance, mode, prgmData]);

  // The page this one is written over, which is a shape to lay the page out in rather than a value
  // to show in the flow of it. The annotation naming it still shows where it was written, as the
  // address it names; the frame is here.
  const overlay = prgmData ? overlayUrl(prgmData[1]) : null;

  // What the page shows of itself, with the mode it is being read or written in already settled. It
  // is settled before the layout so that, on a page written over another one, the frame is a
  // sibling which does not change when this does: reading, writing and saving all leave the framed
  // page mounted, holding its scrolling and its session while notes are written against it.
  const shown = mode === 'view' ? (
    <Loading status={apiResult}>
      <Main data={prgmData} prgmName={prgmName} updateExpr={canEdit ? updateExpr : undefined}/>
    </Loading>
  ) : (
    <Loading status={apiResult}>
      {currentSource !== null && (
        <div className={overlay ? classes.editorFilling : undefined}>
          <div className={overlay ? classes.editorPane : undefined}>
            <Editor
              // Inside a pane the editor fills it, which is the pane the divider is sizing. On a
              // page with no overlay nothing is sizing it, so it keeps the height it always had.
              height={overlay ? "100%" : "80vh"}
              language="plaintext"
              theme="vs"
              value={currentSource}
              onChange={value => setEditedSource(value)}
              // Monaco does not watch the size of what holds it, so without this it keeps the size
              // it was laid out at and the divider moves the pane out from under it.
              options={{ minimap: { enabled: false }, wordWrap: 'on', fontSize: 14, automaticLayout: true }}
              onMount={handleEditorMount}
            />
          </div>
          <div className={classes.statusRow}>
            <div ref={vimStatusBarRef} className={classes.vimStatusBar} data-testid="vim-status-bar" />
            <SaveStatus saveState={saveState} isDirty={isDirty} />
          </div>
        </div>
      )}
    </Loading>
  );

  // An annotation written in this page's source is local to this page, so a value it displays can
  // ask the compiler a follow-up question scoped the same way, and a relative link it displays
  // resolves against the directory this page was imported relative to.
  return (
    <PrgmNameContext.Provider value={prgmName}>
      <PageDirContext.Provider value={pageDir}>
       <ReloadContext.Provider value={reload}>
        <div>
          {canEdit && (
            <div style={{textAlign: 'right'}}>
              {/* In edit mode the save status belongs to the buffer and sits in the editor's own
                  status row, so it is shown here only while reading, where a value is what saves. */}
              {mode === 'view' && <SaveStatus saveState={saveState} isDirty={false} />}
              <IconButton onClick={toggleMode} size="small" aria-label={mode === 'view' ? 'Edit' : 'View'}>
                {mode === 'view' ? <EditIcon fontSize="small" /> : <VisibilityIcon fontSize="small" />}
              </IconButton>
            </div>
          )}
          {overlay ? <OverlaySplit url={overlay}>{shown}</OverlaySplit> : shown}
          <PageNav prev={prevPage} next={nextPage} />
        </div>
       </ReloadContext.Provider>
      </PageDirContext.Provider>
    </PrgmNameContext.Provider>
  );
}

// The pages either side of the one being read. They come with the page rather than being worked out
// from a list of every page the doc server holds, so a page knows its own neighbours. Either side
// may be missing: at the ends of the reading order there is no page to offer, and there is none at
// all until the page arrives.
function PageNav(props) {
  const {prev, next} = props;
  const classes = useStyles();

  return (
    <div className={classes.dirButtons}>
      {prev && (
        <Link to={`/docs/${encodeURIComponent(prev)}`}>
          <Button variant="contained" color="primary">Previous</Button>
        </Link>
      )}
      {next && (
        <Link to={`/docs/${encodeURIComponent(next)}`}>
          <Button variant="contained" color="primary">Next</Button>
        </Link>
      )}
    </div>
  );
}

function SaveStatus(props) {
  const {saveState, isDirty} = props;
  const classes = useStyles();

  let text;
  if (saveState?.status === 'error') {
    text = `save failed: ${saveState.message}`;
  } else if (saveState?.status === 'saving') {
    text = 'saving…';
  } else if (isDirty) {
    text = '● unsaved — Ctrl+S or :w';
  } else if (saveState?.status === 'saved') {
    text = 'saved';
  } else {
    text = '';
  }

  const className = saveState?.status === 'error'
    ? `${classes.saveStatus} ${classes.saveStatusError}`
    : classes.saveStatus;

  return <span className={className} data-testid="save-status">{text}</span>;
}

// Shows a single annotation compiled by the server to the value it displays as. The annotation comes
// either from the ?annot= query, or as a prop from a page embedding this one as a drill-down (such as
// a single point of a type inference).
//
// An annotation is local to the program it is written in, so a prgmName says which program to
// compile this one as though it belonged to. Without one it is compiled against every loaded file,
// which is what an annotation about the whole program wants.
//
// The displayed value can edit the annotation it came from: updating replaces the annotation
// source, which refetches and so recompiles the page. useApi keeps the previous data while the
// new annotation compiles, so the page updates in place rather than flashing back to "Loading".
//
// Which expression is being replaced does not arise here, since the page is the one annotation, so
// the id it is named by is passed over. Nothing is written: an annotation compiled from a query has
// no file to be written back to.
//
// A live annotation is followed rather than asked about once, so a navigation shows the pages there
// now are. Only the annotations which are the whole of what is on screen take it -- the navigation
// and the annotation route -- because following one holds a connection open for as long as it is
// shown, and a browser allows six to a server. An annotation drilled into from inside a page is made
// when a reader picks something and thrown away when they pick the next thing, so it is never shown
// long enough to go stale.
function AnnotPage(props) {
  const query = useQuery();
  const routeAnnot = props.annot || query.get("annot");
  const [annot, setAnnot] = useState(routeAnnot);
  const prgmName = props.prgmName ?? query.get("prgmName") ?? "";
  const [reloadKey, setReloadKey] = useState(0);

  // A new route or query replaces whatever the page edited its way to.
  useEffect(() => {
    setAnnot(routeAnnot);
  }, [routeAnnot]);

  // Compiling the annotation again, which is what a value that wrote to a file asks for: the
  // annotation is unchanged but what it compiles to is not. A #taskList whose task was just checked
  // is the case this exists for.
  const reload = useCallback(() => setReloadKey(k => k + 1), []);

  // Both are asked, one of them about nothing, because which one answers must not change how many
  // hooks this renders. A hook given no path asks nothing and follows nothing.
  const annotUrl = annotApi(annot, prgmName);
  const asked = useApi(props.live ? null : annotUrl, reloadKey);
  const followed = useLiveApi(props.live ? annotUrl : null, reloadKey);
  let apiResult = props.live ? followed : asked;

  return (
    <PrgmNameContext.Provider value={prgmName}>
      <ReloadContext.Provider value={reload}>
        <Loading status={apiResult}>
          <Main data={apiResult.data} prgmName={prgmName} updateExpr={(_metaID, src) => setAnnot(src)}/>
        </Loading>
      </ReloadContext.Provider>
    </PrgmNameContext.Provider>
  );
}

// Shows only what an annotation compiled to, with none of the listing an AnnotPage puts around it
// and nothing at all until it arrives. That is what an annotation shown as a control rather than as
// a page wants: the navbar's search box has no room for a program listing, and a "Loading..." or a
// block of compiler notes would be laid out as part of the navbar rather than reported anywhere.
// The failure is logged instead, so a control that never appears is still explained.
function AnnotValue(props) {
  const {annot, prgmName = ""} = props;
  const apiResult = useApi(annotApi(annot, prgmName));
  const value = apiResult.data && annotStatementValue(apiResult.data);

  useEffect(() => {
    if (apiResult.isLoaded && !value) {
      console.error("Annotation compiled to no value", annot, apiResult.error, apiResult.notes);
    }
  }, [annot, apiResult, value]);

  if (!value) {
    return null;
  }
  return (
    <PrgmNameContext.Provider value={prgmName}>
      <Value data={value} />
    </PrgmNameContext.Provider>
  );
}

// The value an annotation compiled to. /api/annot answers with a one-statement program whose
// annotation expression carries it, in the same meta slot a page's own annotations arrive in.
function annotStatementValue(data) {
  const [, statements] = data;
  const [statementTree] = statements;
  if (!statementTree) {
    return null;
  }
  const [statement] = statementTree;
  return statement.tag === "RawAnnot" ? rawExprMeta(statement.contents).getMetaDat[2] : null;
}

function Main(props) {
  const {data, prgmName, updateExpr} = props;
  const [imports, statements] = data;

  return (
    <div>
      {imports.map((imp, ind) => <Import key={ind} imp={imp}/>)}
      <br/>
      <Statements statements={statements} root={true} prgmName={prgmName} updateExpr={updateExpr} />
    </div>
  );
}

function Import(props) {
  const {imp} = props;
  const classes = useStyles();
  return (
    <div className={classes.noPlay}><KeyWord>import</KeyWord> <RawExpr expr={imp.impRaw} /></div>
  );
}

function Statements(props) {
  return props.statements.map((statement, index) => <StatementTree key={index} statementTree={statement} prgmName={props.prgmName} root={props.root} updateExpr={props.updateExpr} />);
}

function StatementTree(props) {
  const {statementTree, prgmName, updateExpr} = props;
  const [statement, subStatements] = statementTree;
  const classes = useStyles();

  // The sub-statements do inherit updateExpr, which names the expression it replaces rather than
  // leaving it to be understood: a nested statement is a different expression, not an uneditable
  // one. A note's tasks are sub-statements of the page they are declared in.
  return (
    <div>
      <Statement statement={statement} prgmName={prgmName} updateExpr={updateExpr}/>
      <div className={classes.indented}>
        {subStatements.map((subStatement, index) => <StatementTree key={index} statementTree={subStatement} prgmName={prgmName} updateExpr={updateExpr} />)}
      </div>
    </div>
  );
}

function Statement(props) {
  const {statement, prgmName, updateExpr} = props;
  const classes = useStyles();

  switch(statement.tag) {
  case "RawDeclStatement":
    let md = rawExprMeta(statement.contents.roaObj).getMetaDat;
    if (md[1] && md[1].tag !== "NoEval") {
      return (
        <div>
          <PlayButton prgmName={prgmName} fun={md[1].contents}/>
          <RawObjArr roa={statement.contents} updateExpr={updateExpr} />
        </div>
      );
    } else {
      let showNoEval;
      if (DEBUG_NO_EVAL && md[1]) {
        showNoEval = (
          <div>
            <ReachesTree tree={md[1].contents[0]}/>
            <ReachesTree tree={md[1].contents[1]}/>
          </div>
        );
      }
      return (
        <div className={classes.noPlay}>
          <RawObjArr roa={statement.contents} updateExpr={updateExpr} />
          {showNoEval}
        </div>
      );
    }
  case "RawAnnot":
    // A print is shown as the value it printed, so what it computed is nowhere on screen to click.
    // The page carries that computation's source (getMetaDat[3], set only for a print), which is
    // what the debugger is asked for. An annotation that is not a print has none and offers no menu.
    return (
      <div className={classes.noPlay}>
        <DebuggerMenu expr={rawExprMeta(statement.contents).getMetaDat[3]}>
          <RawExpr expr={statement.contents} updateExpr={updateExpr} />
        </DebuggerMenu>
      </div>
    );
  default:
    console.error("Unknown renderStatement", statement);
    return "Statement";
  }
}

function PlayButton(props) {
  const {fun, prgmName} = props;
  let navigate = useNavigate();
  const [open, setOpen] = useState(false);
  const [anchorEl, setAnchorEl] = useState(null);
  const classes = useStyles();

  let handleClick = (event) => {
    setOpen(!open);
    setAnchorEl(event.currentTarget);
  };

  let handleClose = () => {
    setOpen(false);
  };

  let linkClose = (link) => () => {
    setOpen(false);
    navigate(link);
  };

  let button = <PlayArrowIcon fontSize='small' className={classes.playIcon} aria-controls="fade-menu" aria-haspopup="true" onClick={handleClick} />;

  return (
    <span>
      {button}
      <Menu
        anchorEl={anchorEl}
        keepMounted
        open={open}
        onClose={handleClose}
      >
        <MenuItem onClick={linkClose(annotLink(`#debugger(fun= "${fun}")`, prgmName))}>Debug</MenuItem>
      </Menu>
    </span>
  );
}


export default DocsPage;
export {AnnotPage, AnnotValue};
