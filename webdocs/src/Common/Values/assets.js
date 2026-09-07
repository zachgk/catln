// The files a library ships to draw its own values.
//
// A `/Catln/Doc/Show/Rendered` names them by path under the standard library, and the doc server
// serves them from `/api/asset`. Nothing here knows what any of them are for: a `.js` is asked for
// its exports, a `.css` is laid on the page, and which value they draw is between the library's own
// pages and its own script.
//
// This is its own module rather than part of the component using it so that a test can mock it. The
// import below is of a URL computed at runtime, which is not a specifier `vi.mock` can intercept and
// not one jsdom could resolve.

// One promise per path, for the lifetime of the page. A page holding twenty JSON values names
// `json/json.js` twenty times and must fetch it once -- and must run it once, since a module
// evaluated twice is two copies of whatever state it keeps.
const loaded = new Map();

// Where the doc server serves a library's file from. The path is a query parameter rather than part
// of the address so that the server reads it as one string and not as a route.
function assetUrl(path) {
  return `/api/asset?path=${encodeURIComponent(path)}`;
}

// Load every path, and answer with what each one gave. A stylesheet gives nothing, so the caller
// looks through the answers for the one that can render.
function loadAssets(paths) {
  return Promise.all(paths.map(loadAsset));
}

function loadAsset(path) {
  if (!loaded.has(path)) {
    loaded.set(path, start(path));
  }
  return loaded.get(path);
}

function start(path) {
  if (path.endsWith('.css')) {
    return loadStyle(path);
  }
  return import(/* @vite-ignore */ assetUrl(path));
}

// A stylesheet, laid on the page once.
//
// A stylesheet that will not load resolves rather than rejects: what it was going to do is make the
// display look right, and a display that looks wrong is worth more to a reader than no display at
// all. A script that will not load rejects, because without it there is nothing to show.
function loadStyle(path) {
  return new Promise(resolve => {
    const link = document.createElement('link');
    link.rel = 'stylesheet';
    link.href = assetUrl(path);
    link.addEventListener('load', () => resolve(null));
    link.addEventListener('error', () => resolve(null));
    document.head.appendChild(link);
  });
}

export {assetUrl, loadAssets};
