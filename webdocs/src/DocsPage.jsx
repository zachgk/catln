import React, {useState, useEffect, useRef, useContext} from 'react';
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
import {useQuery, useApi, Loading, KeyWord, ReachesTree, ConfigContext, PrgmNameContext, PageDirContext, annotLink, annotApi} from './Common/Common';
import {Value} from './Common/Value';

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
  }
});

// Walk statement trees and collect [{line, startLine, value}] for all expressions
// that have a pre-computed annotation value in getMetaDat[2].
// line: the afterLineNumber for the ViewZone (last content line)
// startLine: the first line of the annotation (for cursor-based hiding)
function extractLineValues(statementTrees) {
  const values = [];

  function walkStatementTree(statementTree) {
    const [statement, subStatements] = statementTree;

    if (statement.tag === "RawDeclStatement") {
      const objMeta = rawExprMeta(statement.contents.roaObj);
      const pos = objMeta.getMetaPos;
      if (pos && objMeta.getMetaDat[2]) {
        values.push({line: pos[1].line, startLine: pos[0].line, value: objMeta.getMetaDat[2]});
      }
    } else if (statement.tag === "RawAnnot") {
      const m = rawExprMeta(statement.contents);
      if (m.getMetaPos && m.getMetaDat[2]) {
        values.push({line: m.getMetaPos[1].line, startLine: m.getMetaPos[0].line, value: m.getMetaDat[2]});
      }
    }

    subStatements.forEach(walkStatementTree);
  }

  statementTrees.forEach(walkStatementTree);
  return values;
}

function DocsPage(props) {
  const {prgmName} = props;

  const [reloadKey, setReloadKey] = useState(0);
  let apiResult = useApi(`/api/page?prgmName=${prgmName}`, reloadKey);
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

  // An annotation written in this page's source is local to this page, so a value it displays can
  // ask the compiler a follow-up question scoped the same way, and a relative link it displays
  // resolves against the directory this page was imported relative to.
  return (
    <PrgmNameContext.Provider value={prgmName}>
      <PageDirContext.Provider value={pageDir}>
        <div>
          {canEdit && (
            <div style={{textAlign: 'right'}}>
              <IconButton onClick={toggleMode} size="small" aria-label={mode === 'view' ? 'Edit' : 'View'}>
                {mode === 'view' ? <EditIcon fontSize="small" /> : <VisibilityIcon fontSize="small" />}
              </IconButton>
            </div>
          )}
          {mode === 'view' ? (
            <Loading status={apiResult}>
              <Main data={prgmData} prgmName={prgmName}/>
            </Loading>
          ) : (
            <Loading status={apiResult}>
              {currentSource !== null && (
                <div>
                  <Editor
                    height="80vh"
                    language="plaintext"
                    theme="vs"
                    value={currentSource}
                    onChange={value => setEditedSource(value)}
                    options={{ minimap: { enabled: false }, wordWrap: 'on', fontSize: 14 }}
                    onMount={handleEditorMount}
                  />
                  <div className={classes.statusRow}>
                    <div ref={vimStatusBarRef} className={classes.vimStatusBar} data-testid="vim-status-bar" />
                    <SaveStatus saveState={saveState} isDirty={isDirty} />
                  </div>
                </div>
              )}
            </Loading>
          )}
          <PageNav prev={prevPage} next={nextPage} />
        </div>
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
// The displayed value can edit the annotation it came from: update replaces the annotation
// source, which refetches and so recompiles the page. useApi keeps the previous data while the
// new annotation compiles, so the page updates in place rather than flashing back to "Loading".
function AnnotPage(props) {
  const query = useQuery();
  const routeAnnot = props.annot || query.get("annot");
  const [annot, setAnnot] = useState(routeAnnot);
  const prgmName = props.prgmName ?? query.get("prgmName") ?? "";

  // A new route or query replaces whatever the page edited its way to.
  useEffect(() => {
    setAnnot(routeAnnot);
  }, [routeAnnot]);

  let apiResult = useApi(annotApi(annot, prgmName));

  return (
    <PrgmNameContext.Provider value={prgmName}>
      <Loading status={apiResult}>
        <Main data={apiResult.data} prgmName={prgmName} update={setAnnot}/>
      </Loading>
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
  const {data, prgmName, update} = props;
  const [imports, statements] = data;

  return (
    <div>
      {imports.map((imp, ind) => <Import key={ind} imp={imp}/>)}
      <br/>
      <Statements statements={statements} root={true} prgmName={prgmName} update={update} />
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
  return props.statements.map((statement, index) => <StatementTree key={index} statementTree={statement} prgmName={props.prgmName} root={props.root} update={props.update} />);
}

function StatementTree(props) {
  const {statementTree, prgmName, update} = props;
  const [statement, subStatements] = statementTree;
  const classes = useStyles();

  // The sub-statements do not inherit update: it names one expression to replace, and a nested
  // statement is a different expression.
  return (
    <div>
      <Statement statement={statement} prgmName={prgmName} update={update}/>
      <div className={classes.indented}>
        {subStatements.map((subStatement, index) => <StatementTree key={index} statementTree={subStatement} prgmName={prgmName} />)}
      </div>
    </div>
  );
}

function Statement(props) {
  const {statement, prgmName, update} = props;
  const classes = useStyles();

  switch(statement.tag) {
  case "RawDeclStatement":
    let md = rawExprMeta(statement.contents.roaObj).getMetaDat;
    if (md[1] && md[1].tag !== "NoEval") {
      return (
        <div>
          <PlayButton prgmName={prgmName} fun={md[1].contents}/>
          <RawObjArr roa={statement.contents} />
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
          <RawObjArr roa={statement.contents} />
          {showNoEval}
        </div>
      );
    }
  case "RawAnnot":
    return (
      <div className={classes.noPlay}>
        <RawExpr expr={statement.contents} update={update} />
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
