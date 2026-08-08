import React, {useMemo} from 'react';

import {ShowList} from './ListProgram';

function TypePage(props) {
  // Both fields are required arguments of Show/TypePage, so a compiled value always carries the name
  // it was filtered by and the listing for that name. Nothing is fetched: this is #listProgram
  // narrowed to one type, and the compiler has already done the narrowing.
  const name = props["/name"].contents;
  const prgm = props["/prgm"];
  const listed = useMemo(() => JSON.parse(prgm.contents), [prgm]);

  return (
    <div>
      <h2>{name}</h2>
      <ShowList data={listed}/>
    </div>
  );
}

export default TypePage;
