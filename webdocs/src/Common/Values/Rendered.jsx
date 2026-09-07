import React, {useEffect, useRef, useState} from 'react';

import {valList} from '../Common';
import {loadAssets} from './assets';

// A value drawn by the library that owns it.
//
// This is the whole of what the webdocs know about such a display. A library writes an arm of
// /Catln/Doc/dshow returning a /Catln/Doc/Show/Rendered, that show carries the value and the paths
// of the files that draw it, and this fetches those files and hands the value to whichever of them
// can render. So there is nothing here named after any one library's type -- the reason
// Values/Json.jsx is gone and this is what replaced it.
//
// The value is passed to the script as it arrived. A library's display walks its own union and needs
// the constructor name at each step, which is exactly what a spread of the arguments discards.
//
// No update. What is shown is drawn by a script rather than built from an expression this page can
// write back, so there is nothing here to edit -- the same reason an Obsidian task gets none.
function Rendered(props) {
  const value = props["/value"];
  // The paths arrive as a /Data/List, which is a cons chain rather than an array.
  const paths = valList(props["/assets"])
    .map(a => (a && typeof a.contents === 'string' ? a.contents : null))
    .filter(Boolean);
  // The dependency is the paths themselves and not the array holding them, which is rebuilt on every
  // render and would restart the load each time.
  const pathKey = paths.join('\n');

  const [assets, setAssets] = useState({status: 'loading'});
  const root = useRef(null);

  useEffect(() => {
    let live = true;
    loadAssets(pathKey ? pathKey.split('\n') : []).then(
      mods => { if (live) { setAssets({status: 'ready', mods}); } },
      err => { if (live) { setAssets({status: 'failed', err}); } }
    );
    // A component unmounted while its files are still arriving must not set state when they do.
    return () => { live = false; };
  }, [pathKey]);

  // Drawn into a node React keeps empty, so the library owns everything inside it. Handing the same
  // node to dangerouslySetInnerHTML instead would have React rewrite that subtree on every render of
  // the page around it, which for a page that follows its own compilation is every time a file it
  // imports is written to -- and would take the reader's folding with it.
  useEffect(() => {
    if (assets.status !== 'ready' || !root.current) {
      return undefined;
    }
    const renderer = assets.mods.find(m => m && typeof m.render === 'function');
    if (!renderer) {
      return undefined;
    }
    // Whatever the script returns is its teardown, if it returned one.
    const teardown = renderer.render(root.current, value);
    return typeof teardown === 'function' ? teardown : undefined;
  }, [assets, value]);

  if (assets.status === 'failed') {
    return <span>Could not load the display for this value: {String(assets.err)}</span>;
  }
  if (assets.status === 'ready' && !assets.mods.some(m => m && typeof m.render === 'function')) {
    // Said rather than left blank. A library which named only a stylesheet, or whose script exports
    // something else, has a value on the page with nothing drawing it, and a reader shown an empty
    // row would read that as a value holding nothing.
    return <span>None of {paths.join(', ') || 'the files named'} renders this value</span>;
  }

  return <span ref={root} />;
}

export default Rendered;
