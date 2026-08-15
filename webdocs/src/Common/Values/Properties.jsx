import React, {useMemo} from 'react';

import makeStyles from '@mui/styles/makeStyles';

const useStyles = makeStyles({
  table: {
    borderCollapse: 'collapse',
    marginBottom: '1em',
    fontSize: '0.9em'
  },
  key: {
    textAlign: 'left',
    verticalAlign: 'top',
    fontWeight: 'normal',
    opacity: 0.7,
    paddingRight: '1.5em',
    whiteSpace: 'nowrap'
  },
  value: {
    textAlign: 'left',
    whiteSpace: 'pre-wrap'
  }
});

// The properties a note opens with, as the table they read as.
//
// The block arrives as the text it was written as rather than as the properties read out of it,
// because the note is written back from that same text -- so it is read apart here, where reading
// it wrongly costs a row of a table rather than the note itself.
function Properties(props) {
  const text = props["/text"].contents;
  const rows = useMemo(() => properties(text), [text]);
  const classes = useStyles();

  // A block holding nothing that reads as a property is shown as nothing rather than as an empty
  // table, so a note whose properties are all comments does not open with a frame around no rows.
  if (rows.length === 0) {
    return null;
  }

  return (
    <table className={classes.table}>
      <tbody>
        {rows.map(({key, value}) => (
          <tr key={key}>
            <th scope="row" className={classes.key}>{key}</th>
            <td className={classes.value}><PropertyValue value={value} /></td>
          </tr>
        ))}
      </tbody>
    </table>
  );
}

// The rows a block of properties reads as.
//
// A line writing a key opens a row, and a line which does not -- the entries of a list written
// under one -- goes on the row above it. So a property written across several lines is one row
// rather than several rows with no name, and a note is shown the properties it wrote rather than
// the lines it wrote them on.
function properties(text) {
  const rows = [];
  text.split('\n').forEach(line => {
    const key = keyOf(line);
    if (key !== null) {
      rows.push({key, value: line.slice(key.length + 1).trim()});
    } else if (rows.length > 0 && line.trim() !== '') {
      const row = rows[rows.length - 1];
      row.value = row.value === '' ? line.trim() : `${row.value}\n${line.trim()}`;
    }
  });
  return rows;
}

// The key a line writes, if it opens a row at all. A line written with a space or a bullet in front
// of it is written under the key above rather than being one, and a line with no colon writes no
// key at all.
function keyOf(line) {
  if (line !== line.trimStart() || line.startsWith('-')) {
    return null;
  }
  const colon = line.indexOf(':');
  return colon > 0 ? line.slice(0, colon) : null;
}

// A property naming a page is shown as the link it is, so the address a note is written over can be
// opened from the note as readily as from the frame below it. Everything else is shown as it was
// written: a wiki link among the properties names a note, and resolving one needs the vault this
// value does not carry.
function PropertyValue(props) {
  const {value} = props;
  if (value.includes('://') && !value.includes(' ')) {
    return <a href={value} target="_blank" rel="noopener noreferrer">{value}</a>;
  }
  return <span>{value}</span>;
}

export default Properties;
