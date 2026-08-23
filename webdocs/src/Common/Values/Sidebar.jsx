import React, {useState, useMemo, useEffect} from 'react';

import makeStyles from '@mui/styles/makeStyles';
import Menu from '@mui/material/Menu';
import MenuItem from '@mui/material/MenuItem';
import {SimpleTreeView} from '@mui/x-tree-view/SimpleTreeView';
import {TreeItem} from '@mui/x-tree-view/TreeItem';
import ExpandMoreIcon from '@mui/icons-material/ExpandMore';
import ChevronRightIcon from '@mui/icons-material/ChevronRight';
import MoreVertIcon from '@mui/icons-material/MoreVert';
import {Link, useLocation, useNavigate} from 'react-router-dom';

import {annotLink} from '../Common';

const useStyles = makeStyles({
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

// The pages of the doc server, as the tree of directories they sit in. The tree is the compiled
// value of a #sidebar annotation, so nothing is fetched here, and nothing outside this component
// reads it: every other part of the app that needs to know about a page is told by the server.
function Sidebar(props) {
  const pages = props["/pages"];
  const tree = useMemo(() => JSON.parse(pages.contents), [pages]);

  // The tree lives in the layout route now, so it outlives navigating between pages and its
  // expansion is worth keeping. What the route decides is only that the page being read is
  // revealed: the directories holding it are added to whatever the reader has open, and everything
  // else is left as they put it.
  // The page is read off the location rather than the route, because the sidebar sits in the
  // layout, above the route that names a page, and so has no page parameter of its own.
  const path = pageOf(useLocation().pathname);
  const [expanded, setExpanded] = useState(() => ancestorPaths(path));

  useEffect(() => {
    setExpanded(open => {
      const closed = ancestorPaths(path).filter(dir => !open.includes(dir));
      return closed.length ? [...open, ...closed] : open;
    });
  }, [path]);

  return (
    <SimpleTreeView
      slots={{collapseIcon: ExpandMoreIcon, expandIcon: ChevronRightIcon}}
      expandedItems={expanded}
      onExpandedItemsChange={(event, items) => setExpanded(items)}
      selectedItems={path || null}
    >
      <SidebarNodes nodes={tree} />
    </SimpleTreeView>
  );
}

// The page a docs location is reading, or nothing anywhere else in the app.
function pageOf(pathname) {
  const prefix = "/docs/";
  return pathname.startsWith(prefix) ? decodeURIComponent(pathname.slice(prefix.length)) : "";
}

// The directories a page sits in, which are the items to expand to reveal it. A node's path is the
// path of every part of it above the file name, so /a/b/c.ct is held by /a and /a/b.
function ancestorPaths(path) {
  if (!path) {
    return [];
  }
  const parts = path.split("/").slice(1, -1);
  return parts.map((_, i) => "/" + parts.slice(0, i + 1).join("/"));
}

function SidebarNodes(props) {
  const {nodes} = props;
  const classes = useStyles();

  return nodes.map(node => {
    if (node.tag === "SidebarDir") {
      return (
        <TreeItem key={node.sbPath} itemId={node.sbPath} label={node.sbName}>
          <SidebarNodes nodes={node.sbChildren} />
        </TreeItem>
      );
    } else {
      let label = (
        <div className={classes.tocFileMain}>
          <Link to={`/docs/${encodeURIComponent(node.sbPath)}`} className={classes.tocFileName}>
            {node.sbName}
          </Link>
          <FileMenu prgmName={node.sbPath}/>
        </div>
      );
      return (
        <TreeItem key={node.sbPath} itemId={node.sbPath} label={label} />
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
        <MenuItem onClick={linkClose(annotLink("#typeInfer", prgmName))}>Type Inference</MenuItem>
        {/* Scoped to this page, which is what makes it the graph of this page's program. The same
            annotation is offered unscoped in the search box, where it is the graph of every page the
            doc server holds. */}
        <MenuItem onClick={linkClose(annotLink("#prgmGraph", prgmName))}>Program Graph</MenuItem>
      </Menu>
    </span>
  );
}

export default Sidebar;
export {ancestorPaths, pageOf};
