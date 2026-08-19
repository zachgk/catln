import React, {useMemo} from 'react';

import DOMPurify from 'dompurify';
import makeStyles from '@mui/styles/makeStyles';
import Chip from '@mui/material/Chip';

import Html from './Html';

// How tall an entry's writing is shown as before it has said how tall it is. An entry is usually one
// image or one paragraph, and a frame given no height at all is shown at whatever a browser picks --
// which crops a comic.
const MIN_HEIGHT = 120;

const useStyles = makeStyles({
  item: {
    display: 'block',
    marginBottom: '1em'
  },
  header: {
    display: 'flex',
    alignItems: 'center',
    flexWrap: 'wrap',
    gap: 8,
    marginBottom: 4
  },
  title: {
    fontWeight: 500
  }
});

// One entry of an RSS feed: what it is called, when it was published, and what it says.
//
// Shown and not edited. An entry is somebody else's writing and there is nothing here to write it
// back to, so it is given no update -- and a value given no update is simply a value that cannot be
// edited.
//
// What an entry says is markup rather than prose. A comic strip's entry is an `img` tag and nothing
// else, so showing it as text would show the tag and no comic. It is shown as the markup it is,
// through the same frame a compiled `.html` result is shown through, with two things standing
// between it and this page: it is sanitized here, and the frame it goes into may not run anything.
//
// The sandbox allows same-origin and nothing else. Without `allow-scripts` there is nothing in the
// frame that can run, so same-origin costs nothing -- and it is what lets the frame be sized to what
// is in it. The pair that is worth avoiding is scripts and same-origin together; neither on its own.
function RssItem(props) {
  const title = props["/title"].contents;
  const link = props["/link"].contents;
  const published = props["/published"].contents;
  const descriptionArg = props["/description"];
  const classes = useStyles();

  const description = useMemo(
    () => DOMPurify.sanitize(descriptionArg.contents),
    [descriptionArg]
  );

  return (
    <span className={classes.item}>
      <span className={classes.header}>
        {link
          ? <a className={classes.title} href={link} target="_blank" rel="noopener noreferrer">{title}</a>
          : <span className={classes.title}>{title}</span>}
        {published ? <Chip label={published} size="small" variant="outlined" /> : null}
      </span>
      {description
        ? <Html contents={description} title={title} sandbox="allow-same-origin" fit minHeight={MIN_HEIGHT} />
        : null}
    </span>
  );
}

export default RssItem;
