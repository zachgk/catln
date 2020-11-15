import './App.css';

import AppBar from '@material-ui/core/AppBar';
import Toolbar from '@material-ui/core/Toolbar';
import Typography from '@material-ui/core/Typography';

import Desugar from './Desugar';

function App() {
  return (
    <div className="App">
      <AppBar position="static">
        <Toolbar>
          <Typography>
            <Typography component="hi" variant="h6" color="inherit" noWrap>Catln WebDocs</Typography>
          </Typography>
        </Toolbar>
      </AppBar>
      <Desugar />
    </div>
  );
}

export default App;
