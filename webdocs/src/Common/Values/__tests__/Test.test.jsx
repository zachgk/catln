import React from 'react'
import { render, screen } from '@testing-library/react'
import userEvent from '@testing-library/user-event'
import { ThemeProvider, createTheme } from '@mui/material/styles'
import Test from '../Test'
import testResponse from '../../../test/fixtures/test-response.json'

const theme = createTheme()

// The args of a compiled Show/Test: the run of the annotated definition's tests, as a JSON string.
function test(results = testResponse) {
  return { '/run': { tag: 'StrVal', contents: JSON.stringify(results) } }
}

function Providers({ children }) {
  return <ThemeProvider theme={theme}>{children}</ThemeProvider>
}

const FAILED = [
  {
    trName: '/testAddIdentity',
    trOwner: '/testAddIdentity',
    trOwnerId: '1a0fc28c-0be2-4e0c-a3ae-e8d1824254a7',
    trKind: 'TestProperty',
    trPassed: false,
    trCases: 1,
    trNotes: 'Generated -1000\nFailed assertion',
  },
]

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
    expect(screen.getByText(/\/testAddIdentity/)).not.toBeVisible()

    await userEvent.click(screen.getByRole('button'))
    expect(await screen.findByText(/\/testAddIdentity/)).toBeVisible()
  })

  it('says how far a failing property got and what it found', async () => {
    render(<Providers><Test {...test(FAILED)} /></Providers>)

    expect(screen.getByText(/✗ Test — failed after 1 cases/)).toBeInTheDocument()
    await userEvent.click(screen.getByRole('button'))
    // The counterexample the engine shrank to, which is the whole reason to open this.
    expect(await screen.findByText(/Generated -1000/)).toBeVisible()
  })

  it('counts the tests when the definition has more than one', () => {
    const many = [
      { ...testResponse[0], trName: '/f', trPassed: true, trCases: 100 },
      { ...testResponse[0], trName: '/f :: overlap-agreement', trKind: 'TestOverlapAgreement', trPassed: false, trCases: 4, trNotes: 'disagreed' },
    ]
    render(<Providers><Test {...test(many)} /></Providers>)

    expect(screen.getByText(/✗ Test — 1\/2 passed/)).toBeInTheDocument()
  })

  it('says a law checked against no instances ran no cases, rather than calling it checked', () => {
    const vacuous = [{ ...testResponse[0], trPassed: true, trCases: 0 }]
    render(<Providers><Test {...test(vacuous)} /></Providers>)

    expect(screen.getByText(/✓ Test — passed with no cases to check/)).toBeInTheDocument()
  })

  it('says so when the run held no test of this definition', () => {
    render(<Providers><Test {...test([])} /></Providers>)

    expect(screen.getByText(/no test of this definition was run/)).toBeInTheDocument()
  })
})
