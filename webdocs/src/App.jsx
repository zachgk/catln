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
  useParams,
  Link,
  Route,
  RouterProvider,
  Outlet
} from "react-router-dom";

import {useApi, TocContext} from './Common/Common';
import {Value} from './Common/Value';
import Docs from './Docs';
import {AnnotPage} from './DocsPage';
import {AppBarSearch} from './Common/Values/Search';

function Root() {

  let tocResult = useApi("/api/toc");
  if (tocResult.isLoaded && Array.isArray(tocResult.data)) {
    let tocResultDataObj = {};
    tocResult.data.forEach(r => {
      tocResultDataObj[r[0]] = r[1];
    });
    tocResult.data = tocResultDataObj;
  }
  return (
    <TocContext.Provider value={tocResult}>
      <Header />
      <Outlet />
    </TocContext.Provider>
  );
}

function Header() {
  return (
    <AppBar position="static">
      <Toolbar>
        <Typography variant="h6" color="inherit" noWrap>Catln WebDocs</Typography>
        <Box sx={{ flexGrow: 1, display: { xs: 'none', md: 'flex' } }}>
          <Link to="/list">
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

function ValuePage(props) {
  const {name} = props;
  let params = useParams();
  let args = {};
  for(var paramName in params) {
    args[paramName] = decodeURIComponent(params[paramName]);
  }
  return <Value data={{tag: "TupleVal", name: name, args: args}}/>;
}

const router = createBrowserRouter(
  createRoutesFromElements(
    <Route path="/" element={<Root />}>
      <Route path="" element={<Navigate to={"/docs"} />} />
      <Route path="docs" element={<Docs />} />
      <Route path="docs/:prgmName" element={<Docs />} />
      <Route path="annot" element={<AnnotPage />} />
      <Route path="list" element={<ValuePage name={"/Catln/Doc/Show/ListProgram"} />} />
      <Route path="type/:name" element={<ValuePage name={"/Catln/Doc/Show/TypePage"} />} />
      <Route path="typeinfer/:prgmName" element={<ValuePage name={"/Catln/Doc/Show/TypeInfer"} />} />
      <Route path="debug/:prgmName/:fun" element={<ValuePage name={"/Catln/Doc/Show/Debug"} />} />
      <Route path="build/:prgmName/:fun" element={<ValuePage name={"/Catln/Doc/Show/BuildPage"} />} />
    </Route>
  )
);


function App() {
  return (
    <RouterProvider router={router} />
  );
}

export default App;
