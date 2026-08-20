import React, {useMemo} from 'react';

import makeStyles from '@mui/styles/makeStyles';
import DOMPurify from 'dompurify';

const useStyles = makeStyles({
  html: {
    // A block, because what a program writes here is block-level markup -- a heading, a table -- and
    // a value is rendered inside the span a statement is written as (RawObjArr, in Syntax.jsx). A
    // span said to be a block is valid where a div written into a span is not.
    display: 'block'
  }
});

// Markup a program wrote, laid into this page.
//
// Laid in rather than framed, which is the whole of the difference between this and Html. That
// component's own note says why it frames: "what is shown here was written somewhere else... Either
// laid into this page would take this page's styling and give it theirs." A program's own display is
// not foreign writing. It is the program saying what one of its own values looks like, so it is
// meant to take this page's styling -- its font, its links, its colours -- where a frame would give
// it none of them, size itself wrongly, and cut the value off from the page it belongs to.
//
// So this is the whole of what the webdocs know about a display a library defines for its own types:
// the library writes an arm of /Catln/Doc/dshow beside the declaration, that arm builds the markup,
// and this renders it. Which is why there is nothing here named after any one library's type.
//
// The markup is not styled here either. A generic renderer overriding the margins of markup it did
// not write is making layout decisions for the library that did.
//
// Sanitized all the same, because "the program's own" is not "trusted". A page is compiled from
// whatever file the doc server was pointed at, an arm builds its markup by joining strings, and a
// string may be anything a file was named -- an Obsidian note is titled by its filename. So the arm
// is responsible for markup that reads correctly, and this is responsible for markup that cannot do
// anything: a display able to run a script is a display able to do whatever the reader can.
function HTMLShow(props) {
  const contents = props["/contents"].contents;
  const classes = useStyles();

  const clean = useMemo(() => DOMPurify.sanitize(contents), [contents]);

  // An arm which built nothing shows nothing, rather than an empty block opening a gap where the
  // program wrote none. Properties does the same with a block that reads as no rows.
  if (!clean) {
    return null;
  }

  return <span className={classes.html} dangerouslySetInnerHTML={{__html: clean}} />;
}

export default HTMLShow;
