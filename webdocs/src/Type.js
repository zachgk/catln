import React from 'react';

import {useParams} from 'react-router-dom';

import {useApi, Loading} from './Common';
import {ShowList} from './ListProgram';

function TypePage() {
  const { name } = useParams();

  let apiResult = useApi(`/object/${name}`);

  return (
    <div>
      <h2>{name}</h2>
      <Loading status={apiResult}>
        <ShowList data={apiResult.data}/>
      </Loading>
    </div>
  );
}

export default TypePage;
