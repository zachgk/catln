import React, {useCallback, useState} from 'react';

// Markup shown as the page it is, in a frame of its own.
//
// A frame rather than the markup laid into this page, because what is shown here was written
// somewhere else: a compiled result is a whole document with its own styling, and an entry of a feed
// is somebody else's writing. Either laid into this page would take this page's styling and give it
// theirs.
//
// `sandbox` is passed through rather than decided here, because what a frame may do depends on whose
// writing is in it. A compiled result is the reader's own output and is framed with no sandbox at
// all, so that a built site previews as the site it is. Untrusted writing is framed with one -- see
// RssItem, which sanitizes as well.
//
// `fit` asks for the frame to be sized to what is in it once it has loaded. A frame cannot say how
// tall it is until there is something in it and nothing outside it can guess, so a frame which is
// not fitted keeps whatever height it was given -- which is what a compiled result wants, being laid
// out by whatever is showing it.
function Html(props) {
  const {contents, title, sandbox, fit, minHeight} = props;
  const [height, setHeight] = useState(minHeight);

  const measure = useCallback(event => {
    const frame = event.target;
    try {
      const body = frame.contentDocument && frame.contentDocument.body;
      if (body) {
        setHeight(Math.max(body.scrollHeight, minHeight || 0));
      }
    } catch {
      // A frame this page may not read into keeps the height it was given. Nothing has gone wrong:
      // not being able to look inside is what a sandbox without allow-same-origin is for.
    }
  }, [minHeight]);

  if (!fit) {
    return <iframe srcDoc={contents} title={title} sandbox={sandbox} />;
  }

  return (
    <iframe
      srcDoc={contents}
      title={title}
      sandbox={sandbox}
      onLoad={measure}
      style={{border: 0, width: '100%', height: height ? `${height}px` : undefined}}
    />
  );
}

export default Html;
