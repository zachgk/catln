import './App.css';

import AppBar from '@material-ui/core/AppBar';
import Toolbar from '@material-ui/core/Toolbar';
import Typography from '@material-ui/core/Typography';
import IconButton from '@material-ui/core/IconButton';
import StarIcon from '@material-ui/icons/Star';
import StarOutlineIcon from '@material-ui/icons/StarOutlined';
import BuildIcon from '@material-ui/icons/Build';
import MenuBookIcon from '@material-ui/icons/MenuBook';
import CompareArrowsIcon from '@material-ui/icons/CompareArrows';
import BugReportIcon from '@material-ui/icons/BugReport';
import {
  BrowserRouter as Router,
  Switch,
  Route,
  Link
} from 'react-router-dom';

import ListProgram from './ListProgram';
import Build from './Build';
import Docs from './Docs';
import Constrain from './Constrain';
import Debug from './Debug';

function App() {
  return (
    <Router className="App">
      <AppBar position="static">
        <Toolbar>
          <Typography>
            <Typography component="hi" variant="h6" color="inherit" noWrap>Catln WebDocs</Typography>
          </Typography>
          <Link to="/desugar">
            <IconButton color="inherit" edge="end"><StarOutlineIcon /></IconButton>
          </Link>
          <Link to="/">
            <IconButton color="inherit" edge="end"><StarIcon /></IconButton>
          </Link>
          <Link to="/constrain">
            <IconButton color="inherit" edge="end"><CompareArrowsIcon /></IconButton>
          </Link>
          <Link to="/debug">
            <IconButton color="inherit" edge="end"><BugReportIcon /></IconButton>
          </Link>
          <Link to="/llvm">
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
            <h2>Typecheck</h2>
            <ListProgram dataPath="/typecheck" />
          </Route>
          <Route path="/desugar">
            <h2>Desugar</h2>
            <ListProgram dataPath="/desugar" />
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
        </Switch>
      </div>
    </Router>
  );
}

export default App;
