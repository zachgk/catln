import './App.css';

import AppBar from '@mui/material/AppBar';
import Box from '@mui/material/Box';
import Toolbar from '@mui/material/Toolbar';
import Typography from '@mui/material/Typography';
import IconButton from '@mui/material/IconButton';
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

import {useApi, TocContext, ConfigContext, annotLink} from './Common/Common';
import Docs from './Docs';
import {AnnotPage} from './DocsPage';
import {AppBarSearch} from './Common/Values/Search';

function Root() {

  let tocResult = useApi("/api/toc");
  let configResult = useApi("/api/config");
  if (tocResult.isLoaded && Array.isArray(tocResult.data)) {
    let tocResultDataObj = {};
    tocResult.data.forEach(r => {
      tocResultDataObj[r[0]] = r[1];
    });
    tocResult.data = tocResultDataObj;
  }
  return (
    <ConfigContext.Provider value={configResult}>
      <TocContext.Provider value={tocResult}>
        <Header />
        <Outlet />
      </TocContext.Provider>
    </ConfigContext.Provider>
  );
}

function Header() {
  return (
    <AppBar position="static">
      <Toolbar>
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
