import {useContext} from 'react';

import {
  Navigate,
  useParams
} from 'react-router-dom';

import {ConfigContext} from './Common/Common';
import DocsPage from './DocsPage';

// The page a doc server opens at is the page it was started with, which only the server knows, so
// /docs asks it rather than picking one out of a listing of every page.
function Docs() {
  const config = useContext(ConfigContext);
  const {prgmName} = useParams();

  if (prgmName) {
    return <DocsPage prgmName={prgmName} />;
  } else if (!config.isLoaded) {
    return <div>Loading...</div>;
  } else if (!config.data?.rootPage) {
    return <div>This doc server was started with no page to open at.</div>;
  }
  return <Navigate to={`/docs/${encodeURIComponent(config.data.rootPage)}`} />;
}

export default Docs;
