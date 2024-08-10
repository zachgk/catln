import React, {useState} from 'react';

import { makeStyles } from '@material-ui/core/styles';
import Menu from '@material-ui/core/Menu';
import MenuItem from '@material-ui/core/MenuItem';
import PlayArrowIcon from '@material-ui/icons/PlayArrow';
import {useHistory} from 'react-router-dom';

import {rawExprMeta, RawExpr, RawObjArr} from './Common/Syntax';
import {useApi, Loading, KeyWord, ReachesTree} from './Common/Common';

const DEBUG_NO_EVAL = false;

const useStyles = makeStyles({
  indented: {
    marginLeft: '4em'
  },
  playIcon: {
    color: 'green',
    cursor: 'pointer'
  },
  noPlay: {
    marginLeft: '24px'
  }
});

function DocsPage(props) {
  const {prgmName} = props;

  let apiResult = useApi(`/api/page?prgmName=${prgmName}`);

  return (
    <Loading status={apiResult}>
      <Main data={apiResult.data} prgmName={prgmName}/>
    </Loading>
  );
}

function Main(props) {
  const {data, prgmName} = props;
  const [imports, statements] = data;

  return (
    <div>
      {imports.map((imp, ind) => <Import key={ind} imp={imp}/>)}
      <br/>
      <Statements statements={statements} root={true} prgmName={prgmName} />
    </div>
  );
}

function Import(props) {
  const {imp} = props;
  const classes = useStyles();
  return (
    <div className={classes.noPlay}><KeyWord>import</KeyWord> <RawExpr expr={imp.rawImpRaw} /></div>
  );
}

function Statements(props) {
  return props.statements.map((statement, index) => <StatementTree key={index} statementTree={statement} prgmName={props.prgmName} root={props.root} />);
}

function StatementTree(props) {
  const {statementTree, prgmName} = props;
  const [statement, subStatements] = statementTree;
  const classes = useStyles();

  return (
    <div>
      <Statement statement={statement} prgmName={prgmName}/>
      <div className={classes.indented}>
        {subStatements.map((subStatement, index) => <StatementTree key={index} statementTree={subStatement} prgmName={prgmName} />)}
      </div>
    </div>
  );
}

function Statement(props) {
  const {statement, prgmName} = props;
  const classes = useStyles();

  switch(statement.tag) {
  case "RawDeclStatement":
    let md = rawExprMeta(statement.contents.roaObj).getMetaDat;
    if (md[1] && md[1].tag !== "NoEval") {
      return (
        <div>
          <PlayButton prgmName={prgmName} fun={encodeURIComponent(md[1].contents)}/>
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
        <RawExpr expr={statement.contents} />
      </div>
    );
  default:
    console.error("Unknown renderStatement", statement);
    return "Statement";
  }
}

function PlayButton(props) {
  const {fun, prgmName} = props;
  let history = useHistory();
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
    history.push({pathname: link});
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
        <MenuItem onClick={linkClose(`/build/${prgmName}/${fun}`)}>Build</MenuItem>
        <MenuItem onClick={linkClose(`/debug/${prgmName}/${fun}`)}>Debug</MenuItem>
      </Menu>
    </span>
  );
}


export default DocsPage;
