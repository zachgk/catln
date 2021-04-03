import React from 'react';

import {useParams} from 'react-router-dom';

function Class() {
  const { name } = useParams();
  return (
      <h2>{name}</h2>
  );
}

export default Class;
