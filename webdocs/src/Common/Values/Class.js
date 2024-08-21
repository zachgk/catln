import React from 'react';

import {useApi, Loading} from '../Common';
import {ShowList, ClassComments} from './ListProgram';

function Class(props) {
  const { name } = props;

  let apiResult = useApi(`/api/class/${name}`);

  return (
    <div>
      <h2>{name}</h2>
      <Loading status={apiResult}>
        <ClassComments data={apiResult.data} name={name}/>
        <ShowList data={apiResult.data}/>
      </Loading>
    </div>
  );
}

export default Class;
