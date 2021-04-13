import React from 'react';

import Grid from '@material-ui/core/Grid';
import TreeView from '@material-ui/lab/TreeView';
import ExpandMoreIcon from '@material-ui/icons/ExpandMore';
import ChevronRightIcon from '@material-ui/icons/ChevronRight';
import TreeItem from '@material-ui/lab/TreeItem';
import {
  Switch,
  Route,
  Redirect,
  Link,
  useParams,
  useRouteMatch
} from 'react-router-dom';

import {useApi, Loading} from './Common';
import DocsPage from './DocsPage';

function Docs() {
  let apiResult = useApi("/toc");

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

  return (
    <Switch>
      <Route exact path={path}>
        <Redirect to={`${path}/${startingPage}`} />
      </Route>
      <Route path={`${path}/:prgmName`}>
        <Grid container spacing={2} justify="center">
          <Grid item xs={2}>
            <TreeView
              defaultCollapseIcon={<ExpandMoreIcon />}
              defaultExpandIcon={<ChevronRightIcon />}
            >
              <TableOfContentsNodes pageTree={buildPageTree(pages)} prevTree="" path={path} />
            </TreeView>
          </Grid>
          <Grid item xs={8}>
            <ShowPage />
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

function TableOfContentsNodes(props) {
  const {pageTree, prevTree, path} = props;

  let trees = Object.keys(pageTree);
  if(trees.includes("main.ct")) {
    trees.splice(trees.indexOf("main.ct"), 1);
    trees.unshift("main.ct");
  }

  return trees.map(tree => {

    let newPrevTree = [prevTree, tree].join("/");
    if(prevTree === "") newPrevTree = tree;

    if(isNaN(pageTree[tree])) {
      // Directory

      return (
        <TreeItem key={tree} nodeId={newPrevTree} label={tree} >
          <TableOfContentsNodes key={tree} pageTree={pageTree[tree]} prevTree={newPrevTree} path={path} />
        </TreeItem>
      );
    } else {
      // File

      let label = <Link to={`${path}/${encodeURIComponent(newPrevTree)}`} >{tree}</Link>;
      return (
        <TreeItem key={tree} nodeId={newPrevTree} label={label} />
      );
    }

  });
}

function ShowPage() {
  const { prgmName } = useParams();

  return <DocsPage prgmName={prgmName} />;
}

export default Docs;
