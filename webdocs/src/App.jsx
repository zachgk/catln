import './App.css';

import {useState, useMemo, useCallback} from 'react';
import AppBar from '@mui/material/AppBar';
import Box from '@mui/material/Box';
import Toolbar from '@mui/material/Toolbar';
import Typography from '@mui/material/Typography';
import IconButton from '@mui/material/IconButton';
import MenuIcon from '@mui/icons-material/Menu';
import ViewList from '@mui/icons-material/ViewList';
import MenuBookIcon from '@mui/icons-material/MenuBook';
import {
  createBrowserRouter,
  createRoutesFromElements,
  Navigate,
  Link,
  Route,
  RouterProvider,
  Outlet
} from "react-router-dom";

import {useApi, TocContext, ConfigContext, SidebarContext, annotLink} from './Common/Common';
import Docs from './Docs';
import {AnnotPage} from './DocsPage';
import {AppBarSearch} from './Common/Values/Search';

const SIDEBAR_WIDTH = 300;
const SIDEBAR_OPEN_KEY = "catln.webdocs.sidebarOpen";

function Root() {

  let configResult = useApi("/api/config");

  // The sidebar is one instance for the whole app rather than one per route, so it survives
  // navigating between pages, and its contents are the compiled #sidebar annotation. The tree that
  // annotation carries is published back here because it is the app's page list too.
  const [pageTree, setPageTree] = useState(null);
  const [sidebarOpen, setSidebarOpen] = useState(readSidebarOpen);

  const toggleSidebar = useCallback(() => {
    setSidebarOpen(open => {
      writeSidebarOpen(!open);
      return !open;
    });
  }, []);

  // Keyed by the path a page is asked for, which is how every reader of it looks a page up, and in
  // the order the sidebar shows them, which is the order the previous/next buttons follow.
  const tocResult = useMemo(() => ({
    isLoaded: pageTree !== null,
    data: pageTree && Object.fromEntries(flattenPages(pageTree)),
    notes: []
  }), [pageTree]);

  return (
    <ConfigContext.Provider value={configResult}>
      <TocContext.Provider value={tocResult}>
        <SidebarContext.Provider value={setPageTree}>
          <Header sidebarOpen={sidebarOpen} toggleSidebar={toggleSidebar} />
          <Box sx={{display: 'flex', alignItems: 'flex-start'}}>
            {/* Contracted the sidebar is zero width rather than unmounted: it still holds the page
                list the search box and the previous/next buttons read. */}
            <Box
              sx={{
                width: sidebarOpen ? SIDEBAR_WIDTH : 0,
                flexShrink: 0,
                overflowX: 'hidden',
                transition: 'width 0.2s',
              }}
              data-testid="sidebar"
            >
              <Box sx={{width: SIDEBAR_WIDTH}}>
                {/* Named with no page on purpose: the sidebar lists every page the doc server
                    holds, not the ones one page reaches. The empty name also stops it from
                    inheriting the page an /annot route is scoped to. */}
                <AnnotPage annot="#sidebar" prgmName="" />
              </Box>
            </Box>
            <Box sx={{flexGrow: 1, minWidth: 0, p: 2}}>
              <Outlet />
            </Box>
          </Box>
        </SidebarContext.Provider>
      </TocContext.Provider>
    </ConfigContext.Provider>
  );
}

// The files of a page tree, in the order the tree shows them, as the [path, page] entries the rest
// of the app reads pages by.
function flattenPages(nodes) {
  return nodes.flatMap(node => node.tag === "SidebarDir"
    ? flattenPages(node.sbChildren)
    : [[node.sbPath, {impDir: node.sbDir}]]);
}

// Whether the sidebar was left open, so the choice survives a reload. Anything but an explicit
// "closed" opens it, which is what a first visit with nothing stored should do.
function readSidebarOpen() {
  return window.localStorage.getItem(SIDEBAR_OPEN_KEY) !== "false";
}

function writeSidebarOpen(open) {
  window.localStorage.setItem(SIDEBAR_OPEN_KEY, String(open));
}

function Header(props) {
  const {sidebarOpen, toggleSidebar} = props;
  return (
    <AppBar position="static">
      <Toolbar>
        <IconButton
          color="inherit"
          edge="start"
          aria-label={sidebarOpen ? "Hide sidebar" : "Show sidebar"}
          aria-expanded={sidebarOpen}
          onClick={toggleSidebar}
          sx={{mr: 1}}
        >
          <MenuIcon />
        </IconButton>
        <Typography variant="h6" color="inherit" noWrap>Catln WebDocs</Typography>
        <Box sx={{ flexGrow: 1, display: { xs: 'none', md: 'flex' } }}>
          <Link to={annotLink("#listProgram(mode= /Catln/Doc/Typecheck)")}>
            <IconButton color="inherit" edge="end"><ViewList /></IconButton>
          </Link>
          <Link to="/docs">
            <IconButton color="inherit" edge="end"><MenuBookIcon /></IconButton>
          </Link>
          <AppBarSearch />
        </Box>
      </Toolbar>
    </AppBar>
  );
}

const router = createBrowserRouter(
  createRoutesFromElements(
    <Route path="/" element={<Root />}>
      <Route path="" element={<Navigate to={"/docs"} />} />
      <Route path="docs" element={<Docs />} />
      <Route path="docs/:prgmName" element={<Docs />} />
      <Route path="annot" element={<AnnotPage />} />
    </Route>
  )
);


function App() {
  return (
    <RouterProvider router={router} />
  );
}

export default App;
export {Root, Header, flattenPages};
