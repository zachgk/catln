import React, {useContext} from 'react';

import ReactMarkdown, {defaultUrlTransform} from 'react-markdown';
import SyntaxHighlighter from 'react-syntax-highlighter';
import {useParams} from 'react-router-dom';
import path from 'pathe';

import {KeyWord, PTypeName, tagJoin, TocContext} from './Common';
import ListProgram from './Values/ListProgram';
import TypeInfer from './Values/TypeInfer';
import Debug from './Values/Debug';
import TypePage from './Values/Type';
import Build from './Values/Build';

function Value(props) {
  let val = props.data;
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
    case "/Catln/Doc/Show/ListProgram":
      return <ListProgram {...val.args} />;
    case "/Catln/Doc/Show/TypePage":
      return <TypePage {...val.args} />;
    case "/Catln/Doc/Show/TypeInfer":
      return <TypeInfer {...val.args} />;
    case "/Catln/Doc/Show/Debug":
      return <Debug {...val.args} />;
    case "/Catln/Doc/Show/BuildPage":
      return <Build {...val.args} />;
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
  case "IOVal":
    return <span>IOVal</span>;
  case "NoVal":
    return <span>NoVal</span>;
  default:
    console.error("Unknown val tag", val);
    return <span>Unknown Val tag</span>;
  }
}

function tryValue(val) {
  if (!val) {
    return null;
  }
  return Value({data: val});
}

function Comment(props) {
  const {comment} = props;
  const params = useParams();
  const tocResult = useContext(TocContext);
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
    } else if (!isUrl(href) && !path.isAbsolute(href) && "prgmName" in params) {
      const p = path.join(tocResult.data[params.prgmName].impDir, href);
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
