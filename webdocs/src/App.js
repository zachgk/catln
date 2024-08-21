import './App.css';

import React, {useState, useContext} from 'react';
import { alpha, makeStyles } from '@material-ui/core/styles';
import AppBar from '@material-ui/core/AppBar';
import TextField from '@material-ui/core/TextField';
import Toolbar from '@material-ui/core/Toolbar';
import Typography from '@material-ui/core/Typography';
import IconButton from '@material-ui/core/IconButton';
import SearchIcon from '@material-ui/icons/Search';
import Autocomplete from '@material-ui/lab/Autocomplete';
import ViewList from '@material-ui/icons/ViewList';
import MenuBookIcon from '@material-ui/icons/MenuBook';
import {
  BrowserRouter as Router,
  Switch,
  Route,
  Link,
  Redirect,
  useHistory
} from 'react-router-dom';

import {useApi, TocContext} from './Common/Common';
import {Value} from './Common/Value';
import Docs from './Docs';
import {AnnotPage} from './DocsPage';

const useStyles = makeStyles((theme) => ({
  search: {
    position: 'relative',
    borderRadius: theme.shape.borderRadius,
    backgroundColor: alpha(theme.palette.common.white, 0.15),
    '&:hover': {
      backgroundColor: alpha(theme.palette.common.white, 0.25),
    },
    marginRight: theme.spacing(2),
    marginLeft: 0,
    width: '100%',
    [theme.breakpoints.up('sm')]: {
      marginLeft: theme.spacing(3),
      width: 'auto',
    },
  },
  searchIcon: {
    padding: theme.spacing(0, 2),
    height: '100%',
    position: 'absolute',
    pointerEvents: 'none',
    display: 'flex',
    alignItems: 'center',
    justifyContent: 'center',
  },
  inputRoot: {
    color: 'inherit',
  },
  inputInput: {
    padding: theme.spacing(1, 1, 1, 0),
    // vertical padding + font size from searchIcon
    paddingLeft: `calc(1em + ${theme.spacing(4)}px)`,
    transition: theme.transitions.create('width'),
    width: '100%',
    [theme.breakpoints.up('md')]: {
      width: '20ch',
    },
  }
}));


function App() {

  let tocResult = useApi("/api/toc");

  return (
    <Router className="App">
      <TocContext.Provider value={tocResult}>
        <Header />
        <Body />
      </TocContext.Provider>
    </Router>
  );
}

function Header() {
  const classes = useStyles();
  const tocResult = useContext(TocContext);
  const history = useHistory();
  const [navValue, setNavValue] = useState('');

  let fileOptions = [];
  if (tocResult.isLoaded && tocResult.data) {
    fileOptions = tocResult.data;
  }

  const searchNavigate = (e, newValue) => {
    setNavValue('');
    if (newValue) {
      history.push({pathname: `/docs/${encodeURIComponent(newValue)}`});
      e.preventDefault();
    }
  };

  return (
    <AppBar position="static">
      <Toolbar>
        <Typography variant="h6" color="inherit" noWrap>Catln WebDocs</Typography>
        <Link to="/list">
          <IconButton color="inherit" edge="end"><ViewList /></IconButton>
        </Link>
        <Link to="/docs">
          <IconButton color="inherit" edge="end"><MenuBookIcon /></IconButton>
        </Link>
        <div className={classes.search}>
          <div className={classes.searchIcon}>
            <SearchIcon />
          </div>
          <Autocomplete
            disablePortal
            id="navigation-portal"
            options={fileOptions}
            className={classes.inputInput}
            inputValue={navValue}
            onInputChange={(e,v) => setNavValue(v)}
            onChange={searchNavigate}
            renderInput={(params) => {
              return <TextField
                       {...params}
                                 />;
            }}
          />
        </div>
      </Toolbar>
    </AppBar>
  );
}

function Body() {
  return (
    <div>
      <Switch>
        <Route exact path="/">
          <Redirect to={"/docs"} />
        </Route>
        <Route path="/docs">
          <Docs />
        </Route>
        <Route path="/annot">
          <AnnotPage />
        </Route>
        <Route path="/list">
          <Value data={{tag: "TupleVal", name: "/Catln/Doc/Show/ListProgram"}} />
        </Route>
        <Route path="/type/:name">
          <Value data={{tag: "TupleVal", name: "/Catln/Doc/Show/TypePage"}} />
        </Route>
        <Route path="/class/:name">
          <Value data={{tag: "TupleVal", name: "/Catln/Doc/Show/ClassPage"}} />
        </Route>
        <Route path="/typeinfer/:prgmName">
          <Value data={{tag: "TupleVal", name: "/Catln/Doc/Show/TypeInfer"}} />
        </Route>
        <Route path="/debug/:prgmName/:fun">
          <Value data={{tag: "TupleVal", name: "/Catln/Doc/Show/Debug"}} />
        </Route>
        <Route path="/build/:prgmName/:fun">
          <Value data={{tag: "TupleVal", name: "/Catln/Doc/Show/BuildPage"}} />
        </Route>
      </Switch>
    </div>
  );
}

export default App;
