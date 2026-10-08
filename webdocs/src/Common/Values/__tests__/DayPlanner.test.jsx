import React from 'react'
import { render, screen, waitFor, within } from '@testing-library/react'
import userEvent from '@testing-library/user-event'
import { ThemeProvider, createTheme } from '@mui/material/styles'
import { MemoryRouter } from 'react-router-dom'
import { ConfigContext, ReloadContext } from '../../Common'
import DayPlanner from '../DayPlanner'

const theme = createTheme()

const NOTE = '/vault/Daily/2026-01-03.md'

// The day the answers below were worked out on, which is the day the compiler was given.
const TODAY = '2026-01-03'

// One entry of what was found: the value itself, the day it sits on, the time of day it was given if
// it was given one, and where it was written.
function entry(id, contents, ciDay, ciTime = null) {
  return {
    ciExpr: id,
    ciPage: NOTE,
    ciDay,
    ciTime,
    ciValue: {
      tag: 'TupleVal',
      name: '/Data/Note/Task/Simple',
      args: {
        '/contents': { tag: 'StrVal', contents },
        '/state': { tag: 'TupleVal', name: '/Data/Primitive/False', args: {} },
      },
    },
  }
}

// A compiled /Data/Calendar/Show/DayPlanner, which carries the calendar's own answer.
function planned(crError, ...items) {
  return { '/result': { tag: 'StrVal', contents: JSON.stringify({ crError, crToday: TODAY, crItems: items }) } }
}

function Providers({ children, readonly = false, reload = () => {} }) {
  return (
    <ThemeProvider theme={theme}>
      <ConfigContext.Provider value={{ isLoaded: true, data: { readonly } }}>
        <ReloadContext.Provider value={reload}>
          <MemoryRouter>{children}</MemoryRouter>
        </ReloadContext.Provider>
      </ConfigContext.Provider>
    </ThemeProvider>
  )
}

function saved() {
  return Promise.resolve({ status: 200, json: () => Promise.resolve({ tag: 'Success', contents: [null, []] }) })
}

describe('DayPlanner', () => {
  beforeEach(() => {
    global.fetch = vi.fn(saved)
  })

  afterEach(() => {
    vi.useRealTimers()
  })

  // A day's blocks are drawn on its timeline, and what sits on the day with no time of day is drawn
  // across the top of it as unscheduled rather than left off.
  it('lays the day out as its blocks and its unscheduled entries', () => {
    const { container } = render(<Providers><DayPlanner {...planned(
      null,
      entry('id-1', 'Write', TODAY, { ctStart: '09:00', ctEnd: '10:30' }),
      entry('id-2', 'Read', TODAY),
    )} /></Providers>)

    const timeline = container.querySelector('.rbc-time-content')
    expect(within(timeline).getByDisplayValue('Write')).toBeInTheDocument()
    expect(within(container.querySelector('.rbc-allday-cell')).getByDisplayValue('Read')).toBeInTheDocument()
  })

  // The day it opens on is the day the answer was worked out on, and not the day a clock says.
  it('opens on the day the answer was worked out on and not on the clock', () => {
    vi.useFakeTimers()
    vi.setSystemTime(new Date('2030-06-15T12:00:00Z'))

    render(<Providers><DayPlanner {...planned(null, entry('id-1', 'Write', TODAY, { ctStart: '09:00', ctEnd: '10:30' }))} /></Providers>)

    expect(screen.getByText(/Jan 03/)).toBeInTheDocument()
    expect(screen.getByDisplayValue('Write')).toBeInTheDocument()
  })

  // Another day's blocks are not this day's.
  it('shows only the blocks of the day it is turned to', async () => {
    render(<Providers><DayPlanner {...planned(
      null,
      entry('id-1', 'Write', TODAY, { ctStart: '09:00', ctEnd: '10:30' }),
      entry('id-2', 'Tomorrow', '2026-01-04', { ctStart: '09:00', ctEnd: '10:00' }),
    )} /></Providers>)

    expect(screen.queryByDisplayValue('Tomorrow')).not.toBeInTheDocument()

    await userEvent.click(screen.getByRole('button', { name: 'Next' }))

    expect(screen.getByDisplayValue('Tomorrow')).toBeInTheDocument()
    expect(screen.queryByDisplayValue('Write')).not.toBeInTheDocument()
    // Turned over rather than asked for again.
    expect(global.fetch).not.toHaveBeenCalled()
  })

  // The answer carries a day and not a time, so there is no now to draw a line at.
  it('draws no line at a now the answer does not carry', () => {
    const { container } = render(<Providers><DayPlanner {...planned(null, entry('id-1', 'Write', TODAY, { ctStart: '09:00', ctEnd: '10:30' }))} /></Providers>)

    const indicator = container.querySelector('.rbc-current-time-indicator')
    if (indicator) {
      expect(window.getComputedStyle(indicator).display).toBe('none')
    }
  })

  it('carries the entries which sit on no day rather than dropping them', () => {
    render(<Providers><DayPlanner {...planned(null, entry('id-3', 'Someday', null))} /></Providers>)

    expect(within(screen.getByTestId('calendar-undated')).getByDisplayValue('Someday')).toBeInTheDocument()
  })

  it('says why nothing was found, where the planner was written', () => {
    render(<Providers><DayPlanner {...planned('This evaluation is not of one page.')} /></Providers>)

    expect(screen.getByTestId('planner-error')).toHaveTextContent('not of one page')
  })

  // A block is written back to the note it came from, as an entry of the calendar is.
  it('writes a checked box back to the page the block came from', async () => {
    const reload = vi.fn()
    render(<Providers reload={reload}><DayPlanner {...planned(null, entry('id-1', 'Write', TODAY, { ctStart: '09:00', ctEnd: '10:30' }))} /></Providers>)

    await userEvent.click(screen.getByRole('checkbox'))

    await waitFor(() => expect(global.fetch).toHaveBeenCalled())
    const [url] = global.fetch.mock.calls[0]
    expect(url).toContain(encodeURIComponent(NOTE))
    expect(url).toContain('id-1')
    await waitFor(() => expect(reload).toHaveBeenCalled())
  })
})
