import React from 'react';

import makeStyles from '@mui/styles/makeStyles';

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
// So this is one of the two ways a library says how its own types are displayed: the library writes
// an arm of /Catln/Doc/dshow beside the declaration, that arm builds the markup, and this renders
// it. The other is Show/Rendered, where the library ships the script that draws the value. Which is
// why there is nothing here named after any one library's type.
//
// The markup is not styled here either. A generic renderer overriding the margins of markup it did
// not write is making layout decisions for the library that did.
//
// Not sanitized. A library's display is the library's own code, run by a doc server that already
// compiles and evaluates that library's pages -- there is nothing a script in this markup could do
// that an arm of the same library could not do first. What a library joins INTO its markup is a
// different matter and is that arm's to get right: a value read out of a file is not the library's
// writing, and /Catln/Doc/Show/Html says which arms carry that and what they owe it.
function HTMLShow(props) {
  const contents = props["/contents"].contents;
  const classes = useStyles();

  // An arm which built nothing shows nothing, rather than an empty block opening a gap where the
  // program wrote none. Properties does the same with a block that reads as no rows.
  if (!contents) {
    return null;
  }

  return <span className={classes.html} dangerouslySetInnerHTML={{__html: contents}} />;
}

export default HTMLShow;
