import React, {useState} from 'react';

import { makeStyles } from '@material-ui/core/styles';
import Button from '@material-ui/core/Button';
import Grid from '@material-ui/core/Grid';
import TreeView from '@material-ui/lab/TreeView';
import Menu from '@material-ui/core/Menu';
import MenuItem from '@material-ui/core/MenuItem';
import ExpandMoreIcon from '@material-ui/icons/ExpandMore';
import ChevronRightIcon from '@material-ui/icons/ChevronRight';
import MoreVertIcon from '@material-ui/icons/MoreVert';
import TreeItem from '@material-ui/lab/TreeItem';
import {
  Switch,
  Route,
  Redirect,
  Link,
  useParams,
  useHistory,
  useRouteMatch
} from 'react-router-dom';

import {useApi, Loading} from './Common';
import DocsPage from './DocsPage';

const useStyles = makeStyles({
  dirButtons: {
    marginTop: '50px',
    width: '100%',
    display: 'flex',
    justifyContent: 'space-evenly'
  },
  tocFileMain: {
    display: 'flex',
    alignItems: 'center'
  },
  tocFileName: {
    flexGrow: 1
  },
  tocFileMenu: {
    verticalAlign: 'middle',
    "&:hover": {
      background: 'lightgray',
      borderRadius: '2em'
    }
  }
});

function Docs() {
  let apiResult = useApi("/api/toc");

  return (
    <Loading status={apiResult}>
      <Main data={apiResult.data} />
    </Loading>
  );
}

function Main(props) {
  let pages = props.data;
  let { path } = useRouteMatch();

  const startingPage = encodeURIComponent(pages[pages.length - 1]);
  const basePageTree = buildPageTree(pages);
  let pageList = []; // sorted list
  const pageTree = sortPageTree(basePageTree, pageList, "");

  return (
    <Switch>
      <Route exact path={path}>
        <Redirect to={`${path}/${startingPage}`} />
      </Route>
      <Route path={`${path}/:prgmName`}>
        <Grid style={{width: '100%'}} container spacing={2} justify="center">
          <Grid item xs={2}>
            <TreeView
              defaultCollapseIcon={<ExpandMoreIcon />}
              defaultExpandIcon={<ChevronRightIcon />}
            >
              <TableOfContentsNodes pageTree={pageTree} path={path} />
            </TreeView>
          </Grid>
          <Grid item xs={8}>
            <ShowPage pages={pageList} path={path}/>
          </Grid>
        </Grid>
      </Route>
    </Switch>
  );
}

function buildPageTree(pageNames) {
  const tree = {};
  pageNames.forEach(pageName => {
    const pageDirs = pageName.split("/");
    const pageFile = pageDirs.splice(-1, 1);
    let curTree = tree;
    pageDirs.forEach(pageDir => {
      if(!(pageDir in curTree)) {
        curTree[pageDir] = {};
      }
      curTree = curTree[pageDir];
    });
    curTree[pageFile] = 1;
  });
  return tree;
}

function sortPageTree(pageTree, pageList, filePath) {
  let sorted = [];
  let keys = Object.keys(pageTree);
  if(keys.includes("main.ct")) {
    keys.splice(keys.indexOf("main.ct"), 1);
    keys.unshift("main.ct");
  }
  keys.forEach(key => {
    let newFilePath = [filePath, key].join("/");
    if(filePath === "") newFilePath = key;
    if(isNaN(pageTree[key])) {
      // directory
      sorted.push({
        type: "dir",
        key,
        filePath,
        newFilePath,
        children: sortPageTree(pageTree[key], pageList, newFilePath)
      });
    } else {
      // file
      sorted.push({
        type: "file",
        key,
        filePath,
        newFilePath
      });
      pageList.push(newFilePath);
    }
  });
  return sorted;
}

function TableOfContentsNodes(props) {
  const {pageTree, path} = props;
  const classes = useStyles();

  return pageTree.map(tree => {

    if(tree.type === "dir") {
      return (
        <TreeItem key={tree.newFilePath} nodeId={tree.newFilePath} label={tree.key} >
          <TableOfContentsNodes key={tree.newFilePath} pageTree={tree.children} path={path} />
        </TreeItem>
      );
    } else {
      let label = (
        <div className={classes.tocFileMain}>
          <Link to={`${path}/${encodeURIComponent(tree.newFilePath)}`} className={classes.tocFileName}>
            {tree.key}
          </Link>
          <FileMenu prgmName={encodeURIComponent(tree.newFilePath)}/>
        </div>
      );
      return (
        <TreeItem key={tree.newFilePath} nodeId={tree.newFilePath} label={label} />
      );
    }

  });
}

function FileMenu(props) {
  const {prgmName} = props;
  const classes = useStyles();
  const history = useHistory();
  const [open, setOpen] = useState(false);
  const [anchorEl, setAnchorEl] = useState(null);

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

  let button = <MoreVertIcon className={classes.tocFileMenu} aria-controls="fade-menu" aria-haspopup="true" onClick={handleClick} />;

  return (
    <span>
      {button}
      <Menu
        anchorEl={anchorEl}
        keepMounted
        open={open}
        onClose={handleClose}
      >
        <MenuItem onClick={linkClose(`/typeinfer/${prgmName}`)}>Type Inference</MenuItem>
      </Menu>
    </span>
  );
}

function ShowPage(props) {
  const {pages, path} = props;
  const { prgmName } = useParams();
  const classes = useStyles();

  const pageNum = pages.indexOf(decodeURIComponent(prgmName));

  let showPrev;
  if(pageNum > 0) {
    showPrev = (
      <Link to={`${path}/${encodeURIComponent(pages[pageNum - 1])}`}>
        <Button variant="contained" color="primary">Previous</Button>
      </Link>
    );
  }

  let showNext;
  if(pageNum < pages.length - 1) {
    showNext = (
      <Link to={`${path}/${encodeURIComponent(pages[pageNum + 1])}`}>
        <Button variant="contained" color="primary">Next</Button>
      </Link>
    );
  }

  return (
    <div>
      <DocsPage prgmName={prgmName} />
      <div className={classes.dirButtons}>
        {showPrev}
        {showNext}
      </div>
    </div>
  );
}

export default Docs;
