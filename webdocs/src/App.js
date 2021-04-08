import './App.css';

import AppBar from '@material-ui/core/AppBar';
import Toolbar from '@material-ui/core/Toolbar';
import Typography from '@material-ui/core/Typography';
import IconButton from '@material-ui/core/IconButton';
import StarIcon from '@material-ui/icons/Star';
import MenuBookIcon from '@material-ui/icons/MenuBook';
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
          <Link to="/docs">
            <IconButton color="inherit" edge="end"><MenuBookIcon /></IconButton>
          </Link>
        </Toolbar>
      </AppBar>
      <div>
        <Switch>
          <Route exact path="/">
            <Redirect to={"/docs"} />
          </Route>
          <Route path="/docs">
            <Docs />
          </Route>
          <Route path="/list">
            <ListProgram />
          </Route>
          <Route path="/type/:name">
            <TypePage />
          </Route>
          <Route path="/class/:name">
            <Class />
          </Route>
          <Route path="/constrain/:prgmName">
            <Constrain />
          </Route>
          <Route path="/debug/:prgmName">
            <Debug />
          </Route>
          <Route path="/build/:prgmName">
            <Build />
          </Route>
        </Switch>
      </div>
    </Router>
  );
}

export default App;
