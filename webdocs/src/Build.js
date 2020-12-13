import React from 'react';

import SyntaxHighlighter from 'react-syntax-highlighter';

import {useApi, Loading} from './Common';

function Build() {
  let apiResult = useApi("/evalBuild");

  return (
    <Loading status={apiResult}>
    <Main data={apiResult.data} />
    </Loading>
  );
}

function Main(props) {
  let [fileName, fileContents] = props.data;
  switch(fileName) {
  case "out.ll":
    return <BuildLlvm llvm={fileContents}/>;
  case "index.html":
    return <BuildWeb contents={fileContents}/>;
  default:
    console.error("Unknown build result", props);
    return "";
  }
}

function BuildLlvm(props) {
  let {llvm} = props;
  return (
    <SyntaxHighlighter language="llvm">
      {llvm}
    </SyntaxHighlighter>
  );
}

function BuildWeb(props) {
  let {contents} = props;
  return (
    <iframe srcDoc={contents} title="Web display of build" />
  );
}

export default Build;
