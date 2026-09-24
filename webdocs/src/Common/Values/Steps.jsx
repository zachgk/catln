import React, {createContext, useContext} from 'react';

import {useTheme} from '@mui/material/styles';

import {TreeItem} from '@mui/x-tree-view/TreeItem';

import {valList} from '../Common';

// What each row of a step table leads to.
//
// A row says what one step of a call did; what it does not say is HOW, and that is in the evaluation
// the debugger already holds. So the rows come from the library, through /Catln/Doc/dshowCall, and
// what a row opens onto comes from the tree around it — two halves that know nothing about each
// other, joined here by position: the nth entry is what the nth row expands to.
//
// Empty everywhere except inside a debugger, which is the honest default rather than a degenerate
// one. A step table read anywhere else is a table of what a call did, with no run behind it to open.
const CallStepsContext = createContext([]);

// A call shown as what it did, step by step.
//
// This draws a /Catln/Doc/Show/Steps and knows nothing about which call it is of. Which is the point:
// a mapping over a list produces one of these because `stack/core/list-webdocs.ct` says an arm of
// dshowCall over an fmap of a ConsList builds one, not because there is anything here that knows what
// fmap is. A binding over a list reaches this same component, and a filter through the binding it is
// written as. A fold does not: it hands an accumulator from row to row, and draws as FoldSteps.
//
// Rows are tree items rather than table rows, because a row opens onto the evaluation that produced
// it and a <table> cannot hold a TreeItem. The columns are a grid laid on each row's label, which is
// what keeps them lined up down the table without one.
function Steps(props) {
  const steps = valList(props["/steps"]);
  const rowNodes = useContext(CallStepsContext);

  if (!steps.length) {
    return <span className="catln-steps-empty">No steps.</span>;
  }

  return (
    <>
      <StepLine><StepRow className="catln-steps-head" cells={["#", "in", "", "out"]} /></StepLine>
      {steps.map((step, i) => {
        const args = step && step.args ? step.args : {};
        const children = rowNodes[i];
        // The id is the row's place in the table rather than anything read out of the step, so two
        // steps that happen to hold the same values are still two rows that fold on their own.
        return (
          <TreeItem
            key={i}
            itemId={`step-${i}`}
            label={<StepRow cells={[cell(args["/index"]), cell(args["/input"]), "↦", cell(args["/output"])]} />}
          >
            {children}
          </TreeItem>
        );
      })}
    </>
  );
}

// A line of a step table that is not a row: its heading, or a line a table draws above or below its
// rows. Laid where a row's label is laid and in the label's type, so its columns line up with the rows'.
//
// A row is a TreeItem, whose label sits after the item's own padding, its icon container and the gap
// between them, and is set in body1 -- the offset and the type are the x-tree-view TreeItem's, read
// from the same theme. A line written loose beside the rows gets neither: it starts at the rows' left
// edge and in the page's smaller type, and since the columns are sized in ems it is narrower too.
function StepLine(props) {
  const theme = useTheme();
  const style = {
    display: 'block',
    paddingLeft: `calc(${theme.spacing(2)} + 16px)`,
    fontSize: theme.typography.body1.fontSize,
  };
  return <div style={style}>{props.children}</div>;
}

// One row's four columns. The arrow is drawn here and is not in the step: a row points from one end
// to the other by being a row, so what the glyph says is about the shape of the table rather than
// about the call, and a library writing an arm has nothing to decide about it.
function StepRow(props) {
  const {cells, className} = props;
  return (
    <span className={className ? `catln-steps-row ${className}` : "catln-steps-row"} style={rowStyle}>
      <span style={indexStyle}>{cells[0]}</span>
      <span>{cells[1]}</span>
      <span style={arrowStyle}>{cells[2]}</span>
      <span>{cells[3]}</span>
    </span>
  );
}

// A cell holds what the library already rendered, which for a step is a String on every end
// including the index. A step whose field is missing shows nothing rather than the word undefined.
function cell(val) {
  if (val === undefined || val === null) {
    return "";
  }
  return val.contents !== undefined ? String(val.contents) : "";
}

const rowStyle = {
  display: 'inline-grid',
  gridTemplateColumns: 'minmax(1.5em, auto) minmax(4em, 1fr) 1.5em minmax(4em, 1fr)',
  columnGap: '0.75em',
  alignItems: 'baseline',
  fontFamily: 'monospace',
  minWidth: '20em',
};

const indexStyle = {opacity: 0.6, textAlign: 'right'};

const arrowStyle = {opacity: 0.6, textAlign: 'center'};

export default Steps;
export {CallStepsContext, StepLine};
