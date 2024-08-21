import React from 'react';

import {useParams} from 'react-router-dom';

import {useApi, Loading} from '../Common';
import {ShowList} from './ListProgram';

function TypePage() {
  let { name } = useParams();
  name = decodeURIComponent(name);

  let apiResult = useApi(`/api/object/${name}`);

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
