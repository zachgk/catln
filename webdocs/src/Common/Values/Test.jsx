import React, {useMemo} from 'react';

import Accordion from '@mui/material/Accordion';
import AccordionSummary from '@mui/material/AccordionSummary';
import AccordionDetails from '@mui/material/AccordionDetails';
import ExpandMoreIcon from '@mui/icons-material/ExpandMore';

// What each family of test is called here. The keys are the compiler's TestKind, which says whether
// a test was written in the program or derived from it. A kind that is not one of these is shown
// under its own name rather than hidden, so a family added to the compiler reads as unfamiliar
// rather than as no family at all.
const KINDS = {
  TestSimple: 'test',
  TestProperty: 'property test',
  TestOverlapAgreement: 'overlap agreement',
  TestRefinementAgreement: 'refinement agreement',
  TestPrintGolden: 'print golden',
};

// How one test's run reads. A test the engine generated inputs for says how many cases it got
// through; one written without arguments was run once and has no count.
function caseSummary({trPassed, trCases}) {
  if (trCases === null || trCases === undefined) {
    return trPassed ? 'passed' : 'failed';
  }
  if (!trPassed) {
    return `failed after ${trCases} cases`;
  }
  // A law over a class with no instances has nothing to quantify over and passes without running a
  // case. Saying so is what distinguishes it from a law that was actually checked.
  return trCases === 0 ? 'passed with no cases to check' : `${trCases} cases passed`;
}

// The tests of the definition this #test annotation is written on, as they ran. The run rides on the
// compiled value, so nothing is fetched: the doc server ran these tests to answer for the page and
// the page shows what it was given.
function Test(props) {
  const run = props["/run"];
  const results = useMemo(() => JSON.parse(run.contents), [run]);

  // A #test marks a definition as a test, so a definition with none is a test the run did not
  // reach — worth saying rather than rendering an empty list.
  if (results.length === 0) {
    return <p>Test — no test of this definition was run.</p>;
  }

  const passed = results.filter(r => r.trPassed).length;
  const ok = passed === results.length;
  const summary = results.length === 1 ? caseSummary(results[0]) : `${passed}/${results.length} passed`;

  return (
    <Accordion>
      <AccordionSummary expandIcon={<ExpandMoreIcon />}>
        <span>{ok ? '✓' : '✗'} Test — {summary}</span>
      </AccordionSummary>
      <AccordionDetails>
        <ul>
          {results.map((r, i) => <TestEntry result={r} key={`${r.trName}-${i}`} />)}
        </ul>
      </AccordionDetails>
    </Accordion>
  );
}

// One test of the run. A failure shows what it reported, which for a generated test is the
// counterexample the test engine shrank to.
function TestEntry({result}) {
  return (
    <li>
      <span>
        {result.trPassed ? '✓' : '✗'} {result.trName} ({KINDS[result.trKind] || result.trKind}) — {caseSummary(result)}
      </span>
      {result.trNotes ? <pre>{result.trNotes}</pre> : null}
    </li>
  );
}

export default Test;
