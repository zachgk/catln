import React from 'react';

import SyntaxHighlighter from 'react-syntax-highlighter';

import {useApi, Loading} from './Common';

function Llvm() {
  let apiResult = useApi("/llvm");

  return (
    <Loading status={apiResult}>
    <Main data={apiResult.data} />
    </Loading>
  );
}

function Main(props) {
  let llvm = props.data;
  return (
    <SyntaxHighlighter language="llvm">
      {llvm}
    </SyntaxHighlighter>
  );
}

export default Llvm;
