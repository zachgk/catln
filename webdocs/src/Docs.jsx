import React, {useState, useContext} from 'react';

import makeStyles from '@mui/styles/makeStyles';
import Button from '@mui/material/Button';
import Grid from '@mui/material/Grid2';
import {SimpleTreeView} from '@mui/x-tree-view/SimpleTreeView';
import Menu from '@mui/material/Menu';
import MenuItem from '@mui/material/MenuItem';
import ExpandMoreIcon from '@mui/icons-material/ExpandMore';
import ChevronRightIcon from '@mui/icons-material/ChevronRight';
import MoreVertIcon from '@mui/icons-material/MoreVert';
import {TreeItem} from '@mui/x-tree-view/TreeItem';
import {
  Navigate,
  Link,
  useParams,
  useNavigate
} from 'react-router-dom';

import {TocContext, Loading} from './Common/Common';
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
    alignItems: 'center',
    "&:hover $tocFileMenu": {
      visibility: 'visible'
    }
  },
  tocFileName: {
    flexGrow: 1
  },
  tocFileMenu: {
    verticalAlign: 'middle',
    visibility: 'hidden',
    "&:hover": {
      visibility: 'visible',
      background: 'lightgray',
      borderRadius: '2em'
    }
  }
});

function Docs() {
  const tocResult = useContext(TocContext);

  return (
    <Loading status={tocResult}>
      <Main data={tocResult.data} />
    </Loading>
  );
}

function Main(props) {
  let pageExprs = props.data;
  let { prgmName } = useParams();

  let pages = Object.keys(pageExprs);

  const startingPage = encodeURIComponent(pages[pages.length - 1]);
  const basePageTree = buildPageTree(pages);
  let pageList = []; // sorted list
  const pageTree = sortPageTree(basePageTree, pageList);

  if (!prgmName) {
    return <Navigate to={`/docs/${startingPage}`} />;
  }

  return (
    <Grid style={{width: '100%'}} container spacing={2} justify="center">
      <Grid size={{xs:2}}>
        <SimpleTreeView slots={{collapseIcon: ExpandMoreIcon, expandIcon: ChevronRightIcon}}>
          <TableOfContentsNodes pageTree={pageTree} path={prgmName} />
        </SimpleTreeView>
      </Grid>
      <Grid size={{xs:8}}>
        <ShowPage pages={pageList} />
      </Grid>
    </Grid>
  );
}

function buildPageTree(pageNames) {
  const tree = {};
  pageNames.forEach(pageName => {
    const pageDirs = pageName.substring(1).split("/");
    const pageFile = pageDirs.splice(-1, 1);
    let curTree = tree;
    pageDirs.forEach(pageDir => {
      if(!(pageDir in curTree) || curTree[pageDir] === 1) {
        curTree[pageDir] = {};
      }
      curTree = curTree[pageDir];
    });
    if (!(pageFile in curTree)) {
      curTree[pageFile] = 1;
    }
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
    let newFilePath = filePath ? [filePath, key].join("/") : ("/" + key);
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
        <TreeItem key={tree.newFilePath} itemId={tree.newFilePath} label={tree.key} >
          <TableOfContentsNodes key={tree.newFilePath} pageTree={tree.children} path={path} />
        </TreeItem>
      );
    } else {
      let label = (
        <div className={classes.tocFileMain}>
          <Link to={`/docs/${encodeURIComponent(tree.newFilePath)}`} className={classes.tocFileName}>
            {tree.key}
          </Link>
          <FileMenu prgmName={encodeURIComponent(tree.newFilePath)}/>
        </div>
      );
      return (
        <TreeItem key={tree.newFilePath} itemId={tree.newFilePath} label={label} />
      );
    }

  });
}

function FileMenu(props) {
  const {prgmName} = props;
  const classes = useStyles();
  const navigate = useNavigate();
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
    navigate(link);
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
  const {pages} = props;
  const { prgmName } = useParams();
  const classes = useStyles();

  const pageNum = pages.indexOf(decodeURIComponent(prgmName));

  let showPrev;
  if(pageNum > 0) {
    showPrev = (
      <Link to={`/docs/${encodeURIComponent(pages[pageNum - 1])}`}>
        <Button variant="contained" color="primary">Previous</Button>
      </Link>
    );
  }

  let showNext;
  if(pageNum < pages.length - 1) {
    showNext = (
      <Link to={`/docs/${encodeURIComponent(pages[pageNum + 1])}`}>
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
