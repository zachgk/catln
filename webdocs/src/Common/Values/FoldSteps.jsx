import React, {useContext} from 'react';

import {TreeItem} from '@mui/x-tree-view/TreeItem';

import {valList} from '../Common';
import {CallStepsContext, StepLine, StepTable, keyCells} from './Steps';

// A fold shown as what it did, step by step: the accumulator each step was handed, the element it
// combined in, and the accumulator it handed on.
//
// This draws a /Catln/Doc/Show/FoldSteps and, like Steps, knows nothing about which call it is of --
// `stack/core/list-webdocs.ct` says which folds build one. It is a display of its own rather than
// Steps with a column more because what makes a fold a fold is where each output goes next, and that
// is a line drawn between rows rather than anything in one of them.
//
// Rows are always in element order, whichever end the fold started from, since that is the order the
// run's chain of calls is in and so the order each row finds what it opens onto (see chainSteps in
// Debugger.jsx). The direction is what the arrows say: each row's last cell points to where its
// output went. A left fold hands each output down to the accumulator of the row below and the last
// one down into the result; a right fold hands each output up to the row above, and the first row's
// up into the result, which is why its initial accumulator is drawn at the bottom.
//
// A keyed table is a /Catln/Doc/Show/EntryFoldSteps, a fold over the entries of a map, and draws the
// entry's key as a column after the index, as a keyed Steps does.
function FoldSteps(props) {
  const steps = valList(props["/steps"]);
  const fromRight = isTrue(props["/fromRight"]);
  const rowNodes = useContext(CallStepsContext);
  const {keyed} = props;

  const init = <StepLine><FoldRow className="catln-fold-init" keyed={keyed} cells={keyCells(keyed, ["init", "", "", "", "", cell(props["/init"]), fromRight ? upFlow(steps.length) : downFlow(steps.length)], "")} /></StepLine>;
  const result = <StepLine><FoldRow className="catln-fold-result" keyed={keyed} cells={keyCells(keyed, ["", "", "", "", "=", cell(props["/result"]), ""], "")} /></StepLine>;

  const rows = steps.map((step, i) => {
    const args = step && step.args ? step.args : {};
    // A left fold's step hands on to the step after it, or to the result after the last; a right
    // fold's to the step before it, or to the result before the first.
    const flow = fromRight ? upFlow(i) : downFlow(steps.length - 1 - i);
    return (
      <TreeItem
        key={i}
        itemId={`step-${i}`}
        label={<FoldRow keyed={keyed} cells={keyCells(keyed, [cell(args["/index"]), cell(args["/acc"]), "⊕", cell(args["/input"]), "↦", cell(args["/output"]), flow], cell(args["/key"]))} />}
      >
        {rowNodes[i]}
      </TreeItem>
    );
  });

  return (
    <StepTable>
      <StepLine><FoldRow className="catln-steps-head" keyed={keyed} cells={keyCells(keyed, ["#", "acc", "", "v", "", "out", ""], "key")} /></StepLine>
      {fromRight ? result : init}
      {rows}
      {fromRight ? init : result}
    </StepTable>
  );
}

// Where an output goes in a left fold: into the accumulator of the row below, or straight down into
// the result when no row is left below it.
function downFlow(rowsBelow) {
  return rowsBelow > 0 ? "↙" : "↓";
}

// The same for a right fold, upward: into the accumulator of the row above, or straight up into the
// result when no row is left above it.
function upFlow(rowsAbove) {
  return rowsAbove > 0 ? "↖" : "↑";
}

// A Boolean arrives as the constructor it is, with nothing in it.
function isTrue(val) {
  return !!val && val.name === "/Data/Primitive/True";
}

// One row's seven columns, or eight with a key, lined up down the table by the same grid on every row.
function FoldRow(props) {
  const {cells, className, keyed} = props;
  const styles = keyed ? keyedCellStyles : cellStyles;
  return (
    <span className={className ? `catln-steps-row ${className}` : "catln-steps-row"} style={keyed ? keyedRowStyle : rowStyle}>
      {cells.map((c, i) => <span key={i} style={styles[i]}>{c}</span>)}
    </span>
  );
}

// What the library already rendered, as in Steps: a missing field shows nothing.
function cell(val) {
  if (val === undefined || val === null) {
    return "";
  }
  return val.contents !== undefined ? String(val.contents) : "";
}

// The index column is a fixed width rather than sized to what is in it, because each line is a grid of
// its own: the init line's label would otherwise widen its first column and push its cells out of line
// with the rows'. Four characters of this monospace type hold "init" and any index a table shown to a
// reader will reach.
const rowStyle = {
  display: 'inline-grid',
  gridTemplateColumns: '4ch minmax(4em, 1fr) 1.5em minmax(4em, 1fr) 1.5em minmax(4em, 1fr) 1.5em',
  columnGap: '0.5em',
  alignItems: 'baseline',
  fontFamily: 'monospace',
  minWidth: '28em',
};

const indexStyle = {opacity: 0.6, textAlign: 'right'};

const glyphStyle = {opacity: 0.6, textAlign: 'center'};

const flowStyle = {opacity: 0.8, textAlign: 'left'};

// The same grid with the key's column after the index, sized as the other cells are so that it lines
// up down the table.
const keyedRowStyle = {
  ...rowStyle,
  gridTemplateColumns: '4ch minmax(3em, 1fr) minmax(4em, 1fr) 1.5em minmax(4em, 1fr) 1.5em minmax(4em, 1fr) 1.5em',
  minWidth: '32em',
};

// A key is read where the index is only counted, as in Steps.
const keyStyle = {fontWeight: 600};

const cellStyles = [indexStyle, undefined, glyphStyle, undefined, glyphStyle, undefined, flowStyle];

const keyedCellStyles = [indexStyle, keyStyle, undefined, glyphStyle, undefined, glyphStyle, undefined, flowStyle];

export default FoldSteps;
