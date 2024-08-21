import React from 'react';

import {useApi, Loading} from '../Common';
import {ShowList} from './ListProgram';

function TypePage(props) {
  const { name } = props;

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
