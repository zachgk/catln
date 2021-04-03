import './App.css';

import AppBar from '@material-ui/core/AppBar';
import Toolbar from '@material-ui/core/Toolbar';
import Typography from '@material-ui/core/Typography';
import IconButton from '@material-ui/core/IconButton';
import StarIcon from '@material-ui/icons/Star';
import BuildIcon from '@material-ui/icons/Build';
import MenuBookIcon from '@material-ui/icons/MenuBook';
import CompareArrowsIcon from '@material-ui/icons/CompareArrows';
import BugReportIcon from '@material-ui/icons/BugReport';
import {
  BrowserRouter as Router,
  Switch,
  Route,
  Link,
  Redirect
} from 'react-router-dom';

import ListProgram from './ListProgram';
import Build from './Build';
import Docs from './Docs';
import Constrain from './Constrain';
import Debug from './Debug';
import TypePage from './Type';
import Class from './Class';

function App() {
  return (
    <Router className="App">
      <AppBar position="static">
        <Toolbar>
          <Typography variant="h6" color="inherit" noWrap>Catln WebDocs</Typography>
          <Link to="/list">
            <IconButton color="inherit" edge="end"><StarIcon /></IconButton>
          </Link>
          <Link to="/constrain">
            <IconButton color="inherit" edge="end"><CompareArrowsIcon /></IconButton>
          </Link>
          <Link to="/debug">
            <IconButton color="inherit" edge="end"><BugReportIcon /></IconButton>
          </Link>
          <Link to="/build">
            <IconButton color="inherit" edge="end"><BuildIcon /></IconButton>
          </Link>
          <Link to="/docs">
            <IconButton color="inherit" edge="end"><MenuBookIcon /></IconButton>
          </Link>
        </Toolbar>
      </AppBar>
      <div>
        <Switch>
          <Route exact path="/">
            <Redirect to={"/list"} />
          </Route>
          <Route path="/list">
            <ListProgram />
          </Route>
          <Route path="/constrain">
            <Constrain />
          </Route>
          <Route path="/debug">
            <Debug />
          </Route>
          <Route path="/build">
            <Build />
          </Route>
          <Route path="/docs">
            <Docs />
          </Route>
          <Route path="/type/:name">
            <TypePage />
          </Route>
          <Route path="/class/:name">
            <Class />
          </Route>
        </Switch>
      </div>
    </Router>
  );
}

export default App;
