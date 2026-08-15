import React from 'react'
import { render, screen, fireEvent } from '@testing-library/react'
import userEvent from '@testing-library/user-event'
import { ThemeProvider, createTheme } from '@mui/material/styles'
import { MemoryRouter, useLocation } from 'react-router-dom'
import Test from '../Test'
import { PrgmNameContext } from '../../Common'
import testResponse from '../../../test/fixtures/test-response.json'

const theme = createTheme()

// The args of a compiled Show/Test: the run of the annotated definition's tests, as a JSON string.
function test(results = testResponse) {
  return { '/run': { tag: 'StrVal', contents: JSON.stringify(results) } }
}

const PAGE = '/stack/core/algebra.ct'

function LocationDisplay() {
  const location = useLocation()
  return <div data-testid="location">{location.pathname + location.search}</div>
}

// A router because each case offers a debugger, which navigates; a page name because that debugger
// is asked for in the scope of the page the test was written on.
function Providers({ children }) {
  return (
    <MemoryRouter>
      <ThemeProvider theme={theme}>
        <PrgmNameContext.Provider value={PAGE}>{children}</PrgmNameContext.Provider>
      </ThemeProvider>
    </MemoryRouter>
  )
}

const FAILED = [
  {
    trName: '/testAddIdentity',
    trOwner: '/testAddIdentity',
    trOwnerId: '1a0fc28c-0be2-4e0c-a3ae-e8d1824254a7',
    trKind: 'TestProperty',
    trPassed: false,
    trCases: 2,
    trInputs: ['/testAddIdentity(x= 7)', '/testAddIdentity(x= -1000)'],
    trNotes: 'Generated -1000\nFailed assertion',
  },
]

// The definition every case in the fixture is a call of, which the case list is matched against.
const TEST_NAME = testResponse[0].trName

describe('Test', () => {
  beforeEach(() => {
    global.fetch = vi.fn(() => Promise.reject(new Error('Test must not fetch')))
  })

  it('summarises a passing run on one line, without fetching', () => {
    render(<Providers><Test {...test()} /></Providers>)

    expect(screen.getByText(/✓ Test — 100 cases passed/)).toBeInTheDocument()
    expect(global.fetch).not.toHaveBeenCalled()
  })

  it('does not show the individual tests until it is expanded', async () => {
    render(<Providers><Test {...test()} /></Providers>)

    // MUI keeps collapsed detail mounted but hidden, so presence is not the question — visibility is.
    expect(screen.getByText(new RegExp(`${TEST_NAME} \\(property test\\)`))).not.toBeVisible()

    await userEvent.click(screen.getByRole('button'))
    expect(await screen.findByText(new RegExp(`${TEST_NAME} \\(property test\\)`))).toBeVisible()
  })

  it('lists every case that ran with the input it ran on', async () => {
    render(<Providers><Test {...test()} /></Providers>)
    await userEvent.click(screen.getByRole('button'))

    // One entry per case the engine reported, not merely the count of them.
    const inputs = testResponse[0].trInputs
    const cases = await screen.findAllByText(new RegExp(`^${TEST_NAME}\\[`))
    expect(cases).toHaveLength(inputs.length)
    expect(cases[0]).toHaveTextContent(inputs[0])
    // The two types this law was checked against, which is what a count alone cannot say.
    expect(cases.filter(c => c.textContent.includes('/Data/String')).length).toBeGreaterThan(0)
    expect(cases.filter(c => c.textContent.includes('/Data/Primitive/Integer')).length).toBeGreaterThan(0)
  })

  it('says how far a failing property got and what it found', async () => {
    render(<Providers><Test {...test(FAILED)} /></Providers>)

    expect(screen.getByText(/✗ Test — failed after 2 cases/)).toBeInTheDocument()
    await userEvent.click(screen.getByRole('button'))
    // The counterexample the engine shrank to, which is the whole reason to open this.
    expect(await screen.findByText(/Generated -1000/)).toBeVisible()
    // The case it first failed on is the last one it ran, and is marked as such. It is a different
    // value from the shrunk counterexample above.
    expect(screen.getByText(/failed here/)).toBeVisible()
    expect(screen.getByText(/x= 7/)).toBeVisible()
  })

  it('lists no cases for a test the engine generated nothing for', async () => {
    const simple = [{...testResponse[0], trKind: 'TestSimple', trCases: null, trInputs: []}]
    render(<Providers><Test {...test(simple)} /></Providers>)
    await userEvent.click(screen.getByRole('button'))

    expect(screen.queryAllByRole('listitem')).toHaveLength(1)
  })

  it('counts the tests when the definition has more than one', () => {
    const many = [
      { ...testResponse[0], trName: '/f', trPassed: true, trCases: 100 },
      { ...testResponse[0], trName: '/f :: overlap-agreement', trKind: 'TestOverlapAgreement', trPassed: false, trCases: 4, trInputs: ['/f(x= 1)', '/f(x= 2)', '/f(x= 3)', '/f(x= 4)'], trNotes: 'disagreed' },
    ]
    render(<Providers><Test {...test(many)} /></Providers>)

    expect(screen.getByText(/✗ Test — 1\/2 passed/)).toBeInTheDocument()
  })

  it('says a law checked against no instances ran no cases, rather than calling it checked', () => {
    const vacuous = [{ ...testResponse[0], trPassed: true, trCases: 0, trInputs: [] }]
    render(<Providers><Test {...test(vacuous)} /></Providers>)

    expect(screen.getByText(/✓ Test — passed with no cases to check/)).toBeInTheDocument()
  })

  it('says so when the run held no test of this definition', () => {
    render(<Providers><Test {...test([])} /></Providers>)

    expect(screen.getByText(/no test of this definition was run/)).toBeInTheDocument()
  })

  // A case that has already run is the one thing a reader can ask to see run again. The engine
  // generated it under a seed that is not kept, so the text of the call it ran is the only handle
  // on that case there is -- which is what the debugger is given.
  describe('debugging one case', () => {
    async function caseMenu(results, index) {
      render(
        <Providers>
          <Test {...test(results)} />
          <LocationDisplay />
        </Providers>
      )
      await userEvent.click(screen.getByRole('button'))
      const cases = screen.getAllByText(new RegExp(`^${results[0].trName.replace(/[/]/g, '\\/')}[[(]`))
      fireEvent.contextMenu(cases[index])
      return results[0].trInputs[index]
    }

    it('offers a debugger of the case a passing test ran on', async () => {
      const input = await caseMenu(testResponse, 0)

      fireEvent.click(screen.getByText('Debugger'))
      expect(decodeURIComponent(screen.getByTestId('location').textContent))
        .toBe(`/annot?annot=#debugExpr(expr= ${JSON.stringify(input)})&prgmName=${PAGE}`)
    })

    // Not only the case a failure stopped on: what a passing law was actually checked against is
    // exactly what is worth looking into when it passed for a reason you did not expect.
    it('offers one for every case, not only the first', async () => {
      const input = await caseMenu(testResponse, 3)

      fireEvent.click(screen.getByText('Debugger'))
      expect(decodeURIComponent(screen.getByTestId('location').textContent))
        .toBe(`/annot?annot=#debugExpr(expr= ${JSON.stringify(input)})&prgmName=${PAGE}`)
    })

    it('offers one for the case a failing test stopped on', async () => {
      const input = await caseMenu(FAILED, FAILED[0].trInputs.length - 1)

      fireEvent.click(screen.getByText('Debugger'))
      expect(decodeURIComponent(screen.getByTestId('location').textContent))
        .toBe(`/annot?annot=#debugExpr(expr= ${JSON.stringify(input)})&prgmName=${PAGE}`)
    })
  })
})
