import React from 'react';

import {useApi, Loading} from '../Common';
import {ShowList} from './ListProgram';

function Class(props) {
  const { name } = props;

  let apiResult = useApi(`/api/type?type=${name}`);

  return (
    <div>
      <h2>{name}</h2>
      <Loading status={apiResult}>
        <ShowList data={apiResult.data}/>
      </Loading>
    </div>
  );
}

export default Class;
