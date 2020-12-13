import React from 'react';

import {useApi, Loading, Val} from './Common';

function Build() {
  let apiResult = useApi("/evalBuild");

  return (
    <Loading status={apiResult}>
    <Main data={apiResult.data} />
    </Loading>
  );
}

function Main(props) {
  return <Val data={props.data}/>;
}

export default Build;
