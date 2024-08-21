import React from 'react';

import {useParams} from 'react-router-dom';

import {Value} from '../Value';
import {useApi, Loading} from '../Common';

function Build() {
  const { prgmName, fun } = useParams();
  let apiResult = useApi(`/api/evalBuild?prgmName=${prgmName}&function=${fun}`);

  return (
    <Loading status={apiResult}>
    <Main data={apiResult.data} />
    </Loading>
  );
}

function Main(props) {
  return <Value data={props.data}/>;
}

export default Build;
