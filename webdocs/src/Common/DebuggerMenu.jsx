import React, {useContext, useState} from 'react';
import {useNavigate} from 'react-router-dom';

import Menu from '@mui/material/Menu';
import MenuItem from '@mui/material/MenuItem';

import {PrgmNameContext, annotLink} from './Common';

// Offers a debugger for one expression, on right-click of whatever is showing it.
//
// A right-click rather than a button because what this wraps is a result -- a printed value, a test
// case that has already run -- and a result is read far more often than it is debugged. A button
// beside every one of a hundred test cases is a hundred things to look past to read the cases.
//
// Its own file rather than part of Values/Debugger.jsx, which shows a run: that is reached from
// Value.jsx, which also reaches Values/Test.jsx, so a menu living there would make Test -> Debugger
// -> Value -> Test a cycle. This imports nothing but the shared toolkit.
function DebuggerMenu({expr, children}) {
  // Not a prop: Value.jsx spreads only a value's own arguments into the components it dispatches to,
  // so a test's display is never told which page it is on. See PrgmNameContext.
  const prgmName = useContext(PrgmNameContext);
  const navigate = useNavigate();
  const [position, setPosition] = useState(null);

  // An expression this cannot spell -- an annotation that is not a print, a syntax the page did not
  // send a source for -- is left with the browser's own menu, which is honest about there being
  // nothing here to offer. Placed after the hooks, which must run either way.
  if (!expr) {
    return children;
  }

  const open = (event) => {
    event.preventDefault();
    setPosition({top: event.clientY, left: event.clientX});
  };
  const close = () => setPosition(null);
  const debug = () => {
    close();
    navigate(annotLink(`#debugExpr(expr= ${JSON.stringify(expr)})`, prgmName));
  };

  return (
    <span onContextMenu={open}>
      {children}
      <Menu
        open={position !== null}
        onClose={close}
        anchorReference="anchorPosition"
        anchorPosition={position ?? undefined}
      >
        <MenuItem onClick={debug}>Debugger</MenuItem>
      </Menu>
    </span>
  );
}

export default DebuggerMenu;
