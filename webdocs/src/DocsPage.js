import React, {useState, useContext} from 'react';

import { makeStyles } from '@material-ui/core/styles';
import Menu from '@material-ui/core/Menu';
import MenuItem from '@material-ui/core/MenuItem';
import PlayArrowIcon from '@material-ui/icons/PlayArrow';
import {useHistory} from 'react-router-dom';

import {RawExpr, RawObjArr} from './Common/Syntax';
import {useApi, Loading, KeyWord, tagJoin} from './Common/Common';

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

const ResMaps = React.createContext({});

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
  const {data} = props;
  const [imports, statements] = data;

  return (
    <div>
      {imports.map((imp, ind) => <Import key={ind} imp={imp}/>)}
      <br/>
      <Statements statements={statements} root={true} />
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
  return props.statements.map((statement, index) => <StatementTree key={index} statementTree={statement} root={props.root} />);
}

function StatementTree(props) {
  const {statementTree} = props;
  const [statement, subStatements] = statementTree;
  const classes = useStyles();

  return (
    <div>
      <Statement statement={statement}/>
      <div className={classes.indented}>
        {subStatements.map((subStatement, index) => <StatementTree key={index} statementTree={subStatement} />)}
      </div>
    </div>
  );
}

function Statement(props) {
  const {statement} = props;
  const classes = useStyles();

  switch(statement.tag) {
  case "RawDeclStatement":
    return (
      <div className={classes.noPlay}>
        <RawObjArr roa={statement.contents} />
      </div>
    );
  case "MultiTypeDefStatement":
    let [mcls, classDatas, mextends] = statement.contents;
    let showClassDatas = tagJoin(classDatas.map((d, dIndex) => <span key={dIndex}><RawExpr expr={d}/></span>), " | ");

    let showMExtends;
    if (mextends.length > 0) {
      let items = tagJoin(mextends.map((ic, icIndex) => <span key={icIndex}><RawExpr expr={ic}/></span>), ", ");
      showMExtends = <span> <KeyWord>isa</KeyWord> {items}</span>;
    }

    return <h3 className={classes.noPlay}><KeyWord>class</KeyWord> <RawExpr expr={mcls}/> = {showClassDatas}{showMExtends}</h3>;
  case "TypeDefStatement":
    return (
      <div className={classes.noPlay}>
        <KeyWord>data</KeyWord> <RawExpr expr={statement.contents} />
      </div>
    );
  case "RawClassDefStatement":
    let classDef = statement.contents[0];
    let [cls, instanceClasses] = classDef;
    let showInstanceClasses = tagJoin(instanceClasses.map((ic, icIndex) => <span key={icIndex}><RawExpr expr={ic}/></span>), ", ");

    return <h3 className={classes.noPlay}><KeyWord>every</KeyWord> <RawExpr expr={cls}/> <KeyWord>isa</KeyWord> {showInstanceClasses}</h3>;
  case "RawClassDeclStatement":
    return (
      <div className={classes.noPlay}>
        <KeyWord>class</KeyWord> <RawExpr expr={statement.contents[0]} />
      </div>
    );
  case "RawAnnot":
    return (
      <div className={classes.noPlay}>
        <RawExpr expr={statement.contents} />
      </div>
    );
  case "RawModule":
    return (
      <div className={classes.noPlay}>
        <KeyWord>module</KeyWord> {statement.contents}
      </div>
    );
  default:
    console.error("Unknown renderStatement", statement);
    return "Statement";
  }
}

function PlayButton(props) {
  const {fun} = props;
  let history = useHistory();
  let {prgmName} = useContext(ResMaps);
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
