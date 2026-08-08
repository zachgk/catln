import {useContext} from 'react';

import makeStyles from '@mui/styles/makeStyles';
import Button from '@mui/material/Button';
import {
  Navigate,
  Link,
  useParams
} from 'react-router-dom';

import {TocContext} from './Common/Common';
import DocsPage from './DocsPage';

const useStyles = makeStyles({
  dirButtons: {
    marginTop: '50px',
    width: '100%',
    display: 'flex',
    justifyContent: 'space-evenly'
  }
});

// The page list comes from the sidebar's #sidebar annotation rather than a route of its own, so it
// arrives with the sidebar rather than ahead of the page. A page is therefore shown as soon as it
// is ready, and the buttons that need to know where it sits among the other pages appear when the
// list does.
function Docs() {
  const tocResult = useContext(TocContext);
  const {prgmName} = useParams();

  const pages = tocResult.isLoaded && tocResult.data ? Object.keys(tocResult.data) : [];

  if (!prgmName) {
    if (pages.length === 0) {
      return <div>Loading...</div>;
    }
    return <Navigate to={`/docs/${encodeURIComponent(pages[pages.length - 1])}`} />;
  }

  return <ShowPage pages={pages} prgmName={prgmName} />;
}

function ShowPage(props) {
  const {pages, prgmName} = props;
  const classes = useStyles();

  const pageNum = pages.indexOf(decodeURIComponent(prgmName));

  let showPrev;
  if(pageNum > 0) {
    showPrev = (
      <Link to={`/docs/${encodeURIComponent(pages[pageNum - 1])}`}>
        <Button variant="contained" color="primary">Previous</Button>
      </Link>
    );
  }

  let showNext;
  if(pageNum >= 0 && pageNum < pages.length - 1) {
    showNext = (
      <Link to={`/docs/${encodeURIComponent(pages[pageNum + 1])}`}>
        <Button variant="contained" color="primary">Next</Button>
      </Link>
    );
  }

  return (
    <div>
      <DocsPage prgmName={prgmName} />
      <div className={classes.dirButtons}>
        {showPrev}
        {showNext}
      </div>
    </div>
  );
}

export default Docs;
