import React from 'react';

import {useParams} from 'react-router-dom';

import {useApi, Loading} from '../Common';
import {ShowList, ClassComments} from './ListProgram'; 

function Class() {
  let { name } = useParams();
  name = decodeURIComponent(name);

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
