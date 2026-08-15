import React, {useContext} from 'react';

import ReactMarkdown, {defaultUrlTransform} from 'react-markdown';
import SyntaxHighlighter from 'react-syntax-highlighter';
import path from 'pathe';

import {KeyWord, PTypeName, tagJoin, PageDirContext} from './Common';
import ListProgram from './Values/ListProgram';
import TypeInfer, {TypeInferPnt} from './Values/TypeInfer';
import Debugger from './Values/Debugger';
import TypePage from './Values/Type';
import Sidebar from './Values/Sidebar';
import Search from './Values/Search';
import Test from './Values/Test';
import Task from './Values/Task';
import TaskList from './Values/TaskList';
import Overlay from './Values/Overlay';
import Properties from './Values/Properties';

// A Value may be given an update function by whatever rendered it. Calling update(src) asks for
// the expression this value was compiled from to be replaced by the Catln source src. What that
// means is the caller's: an AnnotPage recompiles itself with the new annotation, and a docs page
// will rewrite the expression in its source. A value with no update is simply not editable, so
// every display component has to work without one.
function Value(props) {
  let val = props.data;
  const {update} = props;
  switch(val.tag) {
  case "IntVal":
  case "FloatVal":
  case "StrVal":
    return <span>{val.contents}</span>;
  case "TupleVal":
    switch(val.name) {
    case "/Catln/CatlnResult":
      return <CatlnResult data={val}/>;
    case "/Catln/Doc/Show/MD":
      return <Comment comment={val.args["/text"].contents}/>;
    // update is passed outside the spread because the spread is the value's own arg names.
    case "/Catln/Doc/Show/ListProgram":
      return <ListProgram {...val.args} update={update} />;
    case "/Catln/Doc/Show/TypePage":
      return <TypePage {...val.args} />;
    case "/Catln/Doc/Show/TypeInfer":
      return <TypeInfer {...val.args} />;
    case "/Catln/Doc/Show/TypeInferPnt":
      return <TypeInferPnt {...val.args} />;
    case "/Catln/Doc/Show/Debugger":
      return <Debugger {...val.args} />;
    case "/Catln/Doc/Show/Sidebar":
      return <Sidebar {...val.args} />;
    case "/Catln/Doc/Show/Search":
      return <Search {...val.args} />;
    case "/Catln/Doc/Show/Test":
      return <Test {...val.args} />;
    // A task is not a display standing in for something else, it is the value the note declares. So
    // it is the one value here that update edits into the page rather than into an annotation.
    case "/Data/Note/Task":
      return <Task {...val.args} update={update} />;
    // A list of tasks gathered across notes. It gets no update: each of its rows is written back to
    // the note that row came from, which the row carries and the list on this page does not know.
    case "/Data/Note/Show/TaskList":
      return <TaskList {...val.args} />;
    // The page a page is written over, shown as the address it names rather than as a frame of that
    // page. The frame belongs to whatever is laying the page out around it, so a page carrying an
    // overlay is one frame and one link rather than two frames.
    case "/Catln/Doc/Show/Overlay":
      return <Overlay {...val.args} />;
    // The properties a note opens with. No update: they are shown as the note wrote them.
    case "/Data/Note/Show/Properties":
      return <Properties {...val.args} />;
    default:
      let showArgs = "";
      if(Object.keys(val.args).length > 0) {
        showArgs = (
          <span>
            (
            {tagJoin(Object.keys(val.args).map(arg => <span key={arg}>{arg} = <Value key={arg} data={val.args[arg]}/></span>), ", ")}
            )
          </span>
        );
      }

      return <span>{val.name}{showArgs}</span>;
    }
  // A run made for a debugger threads a mock IO handle so its output is captured rather than
  // printed. That is a fact about who ran the program, not about the program, so the value the
  // evaluation tree shows is the one the program sees: its IO.
  case "IOVal":
  case "MockIOVal":
    return <span>IOVal</span>;
  case "NoVal":
    return <span>NoVal</span>;
  default:
    console.error("Unknown val tag", val);
    return <span>Unknown Val tag</span>;
  }
}

function tryValue(val, update) {
  if (!val) {
    return null;
  }
  return Value({data: val, update});
}

function Comment(props) {
  const {comment} = props;
  // The directory this comment's page was imported relative to, which arrives with the page. A
  // comment shown outside a page has none, and its relative links are left as they were written.
  const pageDir = useContext(PageDirContext);
  // let {objNames, classToType} = useContext(ResMaps);
  // objNames = objNames || {};
  // classToType = classToType || {};
  let objNames = {};
  let classToType = {};
  let obj;

  // Replace usages of [TypeName] and [ClassName] with catn:// link reference
  const regex = /\[(\S+)\][^[(]/g;
  const comment2 = comment.replaceAll(regex, (m, name) => {
    if(classToType && name in classToType) {
      return `[${name}](catln://type/${name})${m.slice(-1)}`;
    } else if(objNames && name in objNames) {
      return `[${name}](catln://type/${name})${m.slice(-1)}`;
    } else if(obj && name in obj.objArgs){
      return `[${name}](catln://arg/${name})${m.slice(-1)}`;
    } else if(obj && name in obj.objVars){
      return `[${name}](catln://typevar/${name})${m.slice(-1)}`;
    } else {
      return m;
    }
  });

  const components = {
    a: ({href, children}) => {
      if(href.startsWith("catln://type/")) {
        const typeName = href.substring("catln://type/".length);
        return <PTypeName name={typeName}/>;
      } else if(href.startsWith("catln://arg/")) {
        const argName = href.substring("catln://arg/".length);
        return <KeyWord>{argName}</KeyWord>;
      } else if(href.startsWith("catln://typevar/")) {
          const typeVarName = href.substring("catln://typevar/".length);
          return <KeyWord>{typeVarName}</KeyWord>;
      } else {
        return <a href={href}>{children}</a>;
      }
    }
  };

  function transformLinkUri(href, children, title) {
    if(href.startsWith("catln://")) {
      return href;
    } else if (!isUrl(href) && !path.isAbsolute(href) && pageDir) {
      const p = path.join(pageDir, href);
      return `/docs/${encodeURIComponent(p)}`;
    } else {
      return defaultUrlTransform(href, children, title);
    }
  }

  return <ReactMarkdown children={comment2} urlTransform={transformLinkUri} components={components}/>;
}

function isUrl(str) {
  try {
    new URL(str);
    return true;
  } catch {
    return false;
  }
}

function CatlnResult(props) {
  let data = props.data;
  let fileName = data.args['/name'].contents;
  let fileContents = data.args['/contents'].contents;
  if(fileName.endsWith(".ll")) {
    return (
      <SyntaxHighlighter language="llvm">
        {fileContents}
      </SyntaxHighlighter>
    );
  } else if(fileName.endsWith(".html")) {
    return <iframe srcDoc={fileContents} title={fileName} />;
  } else {
    return (
      <pre>{fileContents}</pre>
    );
  }
}

export {
  Value,
  tryValue
};
