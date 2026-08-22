import React, {useContext} from 'react';

import ReactMarkdown, {defaultUrlTransform} from 'react-markdown';
import path from 'pathe';

import {KeyWord, PTypeName, tagJoin, PageDirContext} from './Common';
import ListProgram from './Values/ListProgram';
import TypeInfer, {TypeInferPnt} from './Values/TypeInfer';
import Debugger from './Values/Debugger';
import TypePage from './Values/Type';
import Sidebar from './Values/Sidebar';
import Search from './Values/Search';
import Test from './Values/Test';
import HTMLShow from './Values/HTMLShow';
import Task from './Values/Task';
import ObsidianTask from './Values/ObsidianTask';
import TaskList from './Values/TaskList';
import TaskQuery from './Values/TaskQuery';
import Calendar from './Values/Calendar';
import Overlay from './Values/Overlay';
import Properties from './Values/Properties';
import {BuildFile, CatlnResult} from './Values/BuildResult';

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
    // A build result and the files in it. The result is the files alone, and each is shown as the
    // path it is built at over what is written in it.
    case "/Catln/CatlnResult":
      return <CatlnResult {...val.args} />;
    case "/Catln/BuildFile":
      return <BuildFile {...val.args} />;
    case "/Catln/Doc/Show/MD":
      return <Comment comment={val.args["/text"].contents}/>;
    // Markup a program wrote, laid into the page rather than framed in one. This is the whole of
    // what the webdocs know about a display a library defines for one of its own types: the library
    // writes an arm of /Catln/Doc/dshow beside the declaration, and what arrives here is the markup
    // that arm built. So a note's page heads itself with the title it is known by without there
    // being anything here that knows what a note is.
    //
    // No update. What is shown is markup rather than the value it was built from, so there is no
    // expression here to write back -- a display meant to be edited is a value of its own, as a task
    // is.
    case "/Catln/Doc/Show/Html":
      return <HTMLShow {...val.args} />;
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
    case "/Data/Note/Task/Simple":
      return <Task {...val.args} update={update} />;
    // The same checkbox in a vault running the Obsidian Tasks plugin, read apart into the fields the
    // plugin wrote into the line. No update: reading a line apart is what the importer does, and
    // writing one back from its parts is not something it can do yet.
    case "/Data/Note/Task/Obsidian":
      return <ObsidianTask {...val.args} />;
    // A list of tasks gathered across notes. It gets no update: each of its rows is written back to
    // the note that row came from, which the row carries and the list on this page does not know.
    case "/Data/Note/Show/TaskList":
      return <TaskList {...val.args} />;
    // What the query a note wrote found. It gets no update for the reason the list above gets none,
    // and for one more: the query is markdown of the note, so it is changed by editing the note.
    // Replacing the annotation with anything that is not the one the query was read into would leave
    // the note unwriteable.
    case "/Data/Note/Show/TaskQuery":
      return <TaskQuery {...val.args} />;
    // Everything a page reaches which sits on a day, on the day it sits on. It gets no update for
    // the reason the two above get none, and it does not need one: an entry of a calendar carries
    // the page it came from and is written back there rather than through whatever is showing it.
    case "/Data/Calendar/Show/Calendar":
      return <Calendar {...val.args} />;
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
      const p = path.join(pageDir, linkPath(href));
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

// The path a link's address names. A file's name may hold a space, which an address cannot, so such
// a name is written escaped and this reads it back. An address which does not decode was never
// escaped -- a page may hold a per cent sign written as itself -- and names the path it already is.
//
// The place within a page an address goes on to name is dropped rather than carried: a page is shown
// as the one run of markdown it is, with no heading to arrive at.
function linkPath(href) {
  const address = href.split("#")[0];
  try {
    return decodeURIComponent(address);
  } catch {
    return address;
  }
}

export {
  Value,
  tryValue
};
