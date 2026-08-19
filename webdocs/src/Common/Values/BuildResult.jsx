import React from 'react';

import SyntaxHighlighter from 'react-syntax-highlighter';

import {Value} from '../Value';
import Html from './Html';

// A build result, as `stack/core/result.ct` declares one: the files of the directory it is built
// into, each carrying the path it is built at. It is flat, and the tree is said by the `/` in each
// path, so this shows a list of paths rather than a structure walked into.

// The values of an evaluated `/Data/List`, which is a `/Data/Cons` chain ending in a `/Data/Nil`.
//
// A tail which is neither is carried through as an entry rather than ending the walk, so that it is
// shown. A result quietly missing its end reads as a result that ended there, which is the one
// thing a display of it must not say.
function valList(val) {
  const items = [];
  let rest = val;
  while (rest && rest.tag === 'TupleVal' && rest.name === '/Data/Cons') {
    items.push(rest.args['/head']);
    rest = rest.args['/tail'];
  }
  if (!(rest && rest.tag === 'TupleVal' && rest.name === '/Data/Nil')) {
    items.push(rest);
  }
  return items;
}

// What is written in a file, shown as whatever it is written in.
//
// Which that is comes from the path, because a file carries its writing and not a type for it. An
// `.ll` is compiler output and is highlighted as such; an `.html` is a document and is shown as the
// page it is; anything else is shown as the text it was written as.
function FileContents(props) {
  const {path, contents} = props;
  if (path.endsWith(".ll")) {
    return (
      <SyntaxHighlighter language="llvm">
        {contents}
      </SyntaxHighlighter>
    );
  } else if (path.endsWith(".html")) {
    // No sandbox: a compiled result is the reader's own output, and a built site which needs script
    // to run is a site whose preview must run it.
    return <Html contents={contents} title={path} />;
  } else {
    return (
      <pre>{contents}</pre>
    );
  }
}

// One file of a result: where it is built, over what is written in it.
function BuildFile(props) {
  const path = props['/path'].contents;
  return (
    <div>
      <strong>{path}</strong>
      <FileContents path={path} contents={props['/contents'].contents} />
    </div>
  );
}

// A build result, which is the files of the directory it is built into.
//
// A result of one file is shown as that file's writing alone. A build producing one page is the
// common case, and was the only case a result could express until recently, so heading it with its
// own path would tell a reader of it nothing they did not already know.
function CatlnResult(props) {
  const files = valList(props['/files']);
  if (files.length === 0) {
    return <em>no files</em>;
  }
  if (files.length === 1 && files[0] && files[0].name === '/Catln/BuildFile') {
    const only = files[0].args;
    return <FileContents path={only['/path'].contents} contents={only['/contents'].contents} />;
  }
  return (
    <ul>
      {files.map((file, i) => <li key={i}><Value data={file} /></li>)}
    </ul>
  );
}

export {
  valList,
  BuildFile,
  CatlnResult
};
