import React from 'react';

import {useParams} from 'react-router-dom';

import {useApi, Loading, Val} from './Common';

function Build() {
  const { prgmName } = useParams();
  let apiResult = useApi(`/evalBuild?prgmName=${prgmName}`);

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
