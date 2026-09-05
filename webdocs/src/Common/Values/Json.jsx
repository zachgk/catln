import React, {useState} from 'react';

import makeStyles from '@mui/styles/makeStyles';
import {SimpleTreeView} from '@mui/x-tree-view/SimpleTreeView';
import {TreeItem} from '@mui/x-tree-view/TreeItem';
import ExpandMoreIcon from '@mui/icons-material/ExpandMore';
import ChevronRightIcon from '@mui/icons-material/ChevronRight';

import {valList} from '../Common';

const useStyles = makeStyles({
  key: {
    fontWeight: 'bold',
    marginRight: '0.5em'
  },
  // The three scalar kinds are coloured apart so that a string holding digits does not read as a
  // number and a string holding `null` does not read as an absence.
  string: {
    color: 'darkgreen'
  },
  number: {
    color: 'darkblue'
  },
  literal: {
    color: 'purple',
    fontStyle: 'italic'
  },
  // What a container holds, shown on the row that folds it, so a reader can decide whether to open
  // it without opening it.
  count: {
    opacity: 0.6,
    marginLeft: '0.5em'
  }
});

// A piece of JSON, as the folding tree of what it holds.
//
// What arrives here is the value itself -- a `/Data/Json` as `stack/json/json.ct` declares one --
// and not a display built from it. That is what the membership in `/Catln/Doc/DShow` means and it
// is why this is handed the whole value rather than the spread of its arguments that every other
// display takes: the component walks a union, so it needs the constructor name at each step, and
// the spread discards it.
//
// A JSON file read by `json/importer.ct` is one of these, so this is what a `.json` page in the doc
// server shows. Nothing here knows that: it renders any `/Data/Json` from wherever it came.
function Json(props) {
  // A member reached on its own is shown as the row it is inside an object -- its key over what is
  // written there -- rather than as a nameless value. Everything else is its own root, with no label
  // above it, because there is no key a document as a whole was written under.
  const member = props.data && props.data.name === "/Data/Json/Member";
  const data = member ? props.data.args["/value"] : props.data;
  const rootLabel = member ? memberKey(props.data, 0) : null;

  // Uncontrolled would lose the reader's folding whenever the page around it re-rendered, which for
  // a page that follows its own compilation is every time a file it imports is written to.
  const [expanded, setExpanded] = useState(defaultExpanded);

  return (
    <SimpleTreeView
      slots={{collapseIcon: ExpandMoreIcon, expandIcon: ChevronRightIcon}}
      expandedItems={expanded}
      onExpandedItemsChange={(event, items) => setExpanded(items)}
    >
      <JsonNode value={data} path="" label={rootLabel} />
    </SimpleTreeView>
  );
}

// The root alone, which is what a document is opened at.
//
// Not everything, and not nothing. A document rendered fully expanded is the thing folding exists to
// avoid, and one rendered fully closed is a single row saying `{5}` — which tells a reader they have
// found the file and nothing about what is in it. Opening the root shows what a file is written at
// its top level: its keys, its scalars as their values, and its containers as the counts they hold.
// Everything below that is a reader's to ask for.
function defaultExpanded() {
  return [""];
}

// One node of the tree: a leaf for a scalar, a foldable item for an array or an object.
//
// `path` is the item's id and is built from where the node sits rather than from what it holds --
// `/tags/0`, `/author/name` -- so it is unique within the document and the same across re-renders.
// Two members written under one key would otherwise be one id, and a tree cannot fold one of them
// without folding the other.
function JsonNode(props) {
  const {value, path, label} = props;
  const classes = useStyles();
  const children = childrenOf(value, path);
  const container = isContainer(value);

  const itemLabel = (
    <span>
      {label === null ? null : <span className={classes.key}>{label}</span>}
      {container
        ? <span className={classes.count}>{summary(value, children.length)}</span>
        : <Scalar value={value} classes={classes} />}
    </span>
  );

  return (
    <TreeItem itemId={path} label={itemLabel}>
      {children.map(child => (
        <JsonNode key={child.path} value={child.value} path={child.path} label={child.label} />
      ))}
    </TreeItem>
  );
}

// A scalar, shown as what it is. An unknown name is shown rather than swallowed: a value that
// reaches here and is not one of the six is a page the compiler built and this did not understand,
// and a reader is better told that than shown an empty row.
function Scalar(props) {
  const {value, classes} = props;
  switch (value && value.name) {
  case "/Data/Json/Str":
    return <span className={classes.string}>"{value.args["/value"].contents}"</span>;
  // The field is a `/Data/Primitive/Number`, which is either arm of it, and the importer writes
  // every number it reads as a float -- JSON has one numeric kind and does not say whether a number
  // written without a point is whole. So what arrives is a `FloatVal` or an `IntVal` and either is
  // read the same way. Rendering needs nothing to undo the float: JSON's numbers and JavaScript's
  // are the same numbers, so a whole one is already written without a `.0`.
  case "/Data/Json/Number":
    return <span className={classes.number}>{String(value.args["/value"].contents)}</span>;
  case "/Data/Json/Bool":
    return <span className={classes.literal}>{value.args["/value"].name === "/Data/Primitive/True" ? "true" : "false"}</span>;
  case "/Data/Json/Null":
    return <span className={classes.literal}>null</span>;
  default:
    return <span>{value && value.name}</span>;
  }
}

// The children of a container, each with the id and the label it is shown under. A scalar has none.
//
// The parent's path is passed in rather than read off the value, because a value does not know where
// it sits: the same `Json/Str(value= "a")` may be the first entry of two different arrays, and two
// nodes sharing an id is a tree that folds one of them when the reader clicks the other.
function childrenOf(value, parentPath = "") {
  if (!value || value.tag !== 'TupleVal') {
    return [];
  }
  switch (value.name) {
  case "/Data/Json/Array":
    return valList(value.args["/entries"]).map((entry, i) => ({
      value: entry,
      path: `${parentPath}/${i}`,
      label: String(i)
    }));
  case "/Data/Json/Object":
    return valList(value.args["/entries"]).map((member, i) => ({
      value: member && member.args ? member.args["/value"] : member,
      path: `${parentPath}/${i}`,
      label: memberKey(member, i)
    }));
  default:
    return [];
  }
}

// A member's key, or its position where what arrived is not a member. A malformed entry is still
// shown, under the one label that is certainly unique.
function memberKey(member, i) {
  const key = member && member.args && member.args["/key"];
  return key && typeof key.contents === 'string' ? key.contents : String(i);
}

// Whether a value is an array or an object, which is asked rather than inferred from whether it has
// children: an empty array and an empty object have none, and are still containers. Deciding by the
// children would show them as scalars of no kind rather than as `[0]` and `{0}`.
function isContainer(value) {
  return Boolean(value) && value.tag === 'TupleVal'
    && (value.name === "/Data/Json/Array" || value.name === "/Data/Json/Object");
}

// What a container holds, in one glance: `{3}` for an object of three members, `[2]` for an array of
// two entries.
function summary(value, count) {
  return value.name === "/Data/Json/Array" ? `[${count}]` : `{${count}}`;
}

export default Json;
