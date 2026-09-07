// A piece of JSON, as the folding tree of what it holds.
//
// This is the display of `/Data/Json`, and it ships from the library declaring that type rather than
// from the doc server showing it. `json.ct`'s arm of `/Catln/Doc/dshow` names this file, the doc
// server fetches it and asks it for `render`, and nothing in the doc server knows what JSON is.
//
// What arrives is the value itself -- a `/Data/Json` as `json.ct` declares one, serialized as the
// compiler's `Val` -- so this walks a union and reads the constructor name at each step.

// The paths a reader has opened, per rendered document rather than per path: the same `/tags/0` in
// two documents on one page is two rows, and folding one must not fold the other. Keyed by the
// element the document was rendered into, which is stable for as long as the document is on screen.
const openPaths = new WeakMap();

// Render `value` into `root`. Called again whenever the page around it re-renders, and returns the
// teardown for when it goes away.
export function render(root, value) {
  // A member reached on its own is shown as the row it is inside an object -- its key over what is
  // written there -- rather than as a nameless value. Everything else is its own root, with no label
  // above it, because there is no key a document as a whole was written under.
  const member = isTuple(value) && value.name === "/Data/Json/Member";
  const data = member ? value.args["/value"] : value;
  const rootLabel = member ? memberKey(value, 0) : null;

  // The root alone, which is what a document is opened at. Not everything, and not nothing: fully
  // expanded is the thing folding exists to avoid, and fully closed is one row saying `{5}`, which
  // tells a reader they have found the file and nothing about what is in it.
  if (!openPaths.has(root)) {
    openPaths.set(root, new Set([""]));
  }
  const open = openPaths.get(root);

  root.className = "catln-json";
  root.replaceChildren(listOf([node(data, "", rootLabel, open)]));

  return () => {
    openPaths.delete(root);
    root.replaceChildren();
  };
}

// One node of the tree: a leaf for a scalar, a foldable row for an array or an object.
//
// `path` is the row's id and is built from where the node sits rather than from what it holds --
// `/tags/0`, `/author/name` -- so it is unique within the document and the same every time it is
// drawn. Two members written under one key would otherwise be one id, and a tree cannot fold one of
// them without folding the other.
function node(value, path, label, open) {
  const item = document.createElement("li");
  const row = document.createElement("div");
  row.className = "catln-json-row";

  const children = childrenOf(value, path);

  if (isContainer(value)) {
    const toggle = document.createElement("button");
    toggle.type = "button";
    toggle.className = "catln-json-toggle";
    row.appendChild(toggle);
    if (label !== null) {
      row.appendChild(keyEl(label));
    }
    // What a container holds, on the row that folds it, so a reader can decide whether to open it
    // without opening it.
    row.appendChild(spanEl("catln-json-count", summary(value, children.length)));
    item.appendChild(row);

    const sub = listOf(children.map(c => node(c.value, c.path, c.label, open)));
    item.appendChild(sub);

    const draw = () => {
      const isOpen = open.has(path);
      sub.hidden = !isOpen;
      toggle.textContent = isOpen ? "▾" : "▸";
      toggle.setAttribute("aria-expanded", String(isOpen));
    };
    // Toggling hides the rows rather than rebuilding them, so everything opened below stays opened
    // and comes back as it was when the row above is opened again.
    toggle.addEventListener("click", () => {
      if (open.has(path)) {
        open.delete(path);
      } else {
        open.add(path);
      }
      draw();
    });
    draw();
  } else {
    // A leaf sits where a toggle would, so scalars and containers line up down the page.
    row.appendChild(spanEl("catln-json-toggle catln-json-leaf", ""));
    if (label !== null) {
      row.appendChild(keyEl(label));
    }
    row.appendChild(scalar(value));
    item.appendChild(row);
  }

  return item;
}

// A scalar, shown as what it is. An unknown name is shown rather than swallowed: a value that
// reaches here and is not one of the six is a page the compiler built and this did not understand,
// and a reader is better told that than shown an empty row.
function scalar(value) {
  switch (isTuple(value) ? value.name : null) {
  case "/Data/Json/Str":
    return spanEl("catln-json-string", '"' + contentsOf(value) + '"');
  // The field is a `/Data/Primitive/Number`, which is either arm of it, and the importer writes
  // every number it reads as a float -- JSON has one numeric kind and does not say whether a number
  // written without a point is whole. So what arrives is a `FloatVal` or an `IntVal` and either is
  // read the same way. Rendering needs nothing to undo the float: JSON's numbers and JavaScript's
  // are the same numbers, so a whole one is already written without a `.0`.
  case "/Data/Json/Number":
    return spanEl("catln-json-number", String(contentsOf(value)));
  case "/Data/Json/Bool":
    return spanEl("catln-json-literal", value.args["/value"].name === "/Data/Primitive/True" ? "true" : "false");
  case "/Data/Json/Null":
    return spanEl("catln-json-literal", "null");
  default:
    return spanEl("catln-json-unknown", value && value.name ? value.name : String(value));
  }
}

// The children of a container, each with the id and the label it is shown under. A scalar has none.
//
// The parent's path is passed in rather than read off the value, because a value does not know where
// it sits: the same `Json/Str(value= "a")` may be the first entry of two different arrays.
function childrenOf(value, parentPath) {
  if (!isTuple(value)) {
    return [];
  }
  switch (value.name) {
  case "/Data/Json/Array":
    return valList(value.args["/entries"]).map((entry, i) => ({
      value: entry,
      path: parentPath + "/" + i,
      label: String(i)
    }));
  case "/Data/Json/Object":
    return valList(value.args["/entries"]).map((m, i) => ({
      value: isTuple(m) ? m.args["/value"] : m,
      path: parentPath + "/" + i,
      label: memberKey(m, i)
    }));
  default:
    return [];
  }
}

// A member's key, or its position where what arrived is not a member. A malformed entry is still
// shown, under the one label that is certainly unique.
function memberKey(m, i) {
  const key = isTuple(m) ? m.args["/key"] : null;
  return key && typeof key.contents === "string" ? key.contents : String(i);
}

// Whether a value is an array or an object, which is asked rather than inferred from whether it has
// children: an empty array and an empty object have none, and are still containers. Deciding by the
// children would show them as scalars of no kind rather than as `[0]` and `{0}`.
function isContainer(value) {
  return isTuple(value) && (value.name === "/Data/Json/Array" || value.name === "/Data/Json/Object");
}

// What a container holds, in one glance: `{3}` for an object of three members, `[2]` for an array of
// two entries.
function summary(value, count) {
  return value.name === "/Data/Json/Array" ? "[" + count + "]" : "{" + count + "}";
}

// The values of an evaluated `/Data/List`, which is a `/Data/List/Cons` chain ending in a
// `/Data/List/Nil`. Written here rather than borrowed from the doc server, because a library's
// display reaches nothing of the page showing it.
//
// A tail which is neither is carried through as an entry rather than ending the walk, so that it is
// shown. A result quietly missing its end reads as a result that ended there.
function valList(val) {
  const items = [];
  let rest = val;
  while (isTuple(rest) && rest.name === "/Data/List/Cons") {
    items.push(rest.args["/head"]);
    rest = rest.args["/tail"];
  }
  if (!(isTuple(rest) && rest.name === "/Data/List/Nil")) {
    items.push(rest);
  }
  return items;
}

function isTuple(val) {
  return Boolean(val) && val.tag === "TupleVal" && Boolean(val.args);
}

function contentsOf(value) {
  const held = value.args["/value"];
  return held ? held.contents : "";
}

function spanEl(className, text) {
  const el = document.createElement("span");
  el.className = className;
  el.textContent = text;
  return el;
}

function keyEl(label) {
  return spanEl("catln-json-key", label);
}

function listOf(items) {
  const ul = document.createElement("ul");
  ul.className = "catln-json-list";
  items.forEach(i => ul.appendChild(i));
  return ul;
}
