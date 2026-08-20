import {useContext} from 'react';

import {
  Navigate,
  useParams
} from 'react-router-dom';

import {ConfigContext, LoadingProgress} from './Common/Common';
import DocsPage from './DocsPage';

// The page a doc server opens at is the page it was started with, which only the server knows, so
// /docs asks it rather than picking one out of a listing of every page.
function Docs() {
  const config = useContext(ConfigContext);
  const {prgmName} = useParams();

  if (prgmName) {
    return <DocsPage prgmName={prgmName} />;
  } else if (!config.isLoaded) {
    // Reported like any other wait, though what is being waited for is a question the compiler
    // answers without compiling anything. Asking which page to open takes the service's lock, so a
    // reader who arrives while something is being built waits here for as long as that build takes —
    // and this is the first thing they see, before there is a page or a navigation to wait at.
    return <div><div>Loading...</div><LoadingProgress /></div>;
  } else if (!config.data?.rootPage) {
    return <div>This doc server was started with no page to open at.</div>;
  }
  return <Navigate to={`/docs/${encodeURIComponent(config.data.rootPage)}`} />;
}

export default Docs;
