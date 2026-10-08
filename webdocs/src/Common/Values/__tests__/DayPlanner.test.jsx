import React from 'react'
import { fireEvent, render, screen, waitFor, within } from '@testing-library/react'
import userEvent from '@testing-library/user-event'
import { ThemeProvider, createTheme } from '@mui/material/styles'
import { MemoryRouter } from 'react-router-dom'
import { ConfigContext, ReloadContext } from '../../Common'
import DayPlanner, { openingTime } from '../DayPlanner'

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

  // A day far from the one the planner opened on is reached by naming it, rather than by turning over
  // a day at a time to get there.
  it('turns to a day named in the field for it', () => {
    render(<Providers><DayPlanner {...planned(
      null,
      entry('id-1', 'Write', TODAY, { ctStart: '09:00', ctEnd: '10:30' }),
      entry('id-2', 'Later', '2026-03-10', { ctStart: '14:00', ctEnd: '15:00' }),
    )} /></Providers>)

    expect(screen.getByTestId('go-to-day')).toHaveValue(TODAY)
    fireEvent.change(screen.getByTestId('go-to-day'), { target: { value: '2026-03-10' } })

    expect(screen.getByText(/Mar 10/)).toBeInTheDocument()
    expect(screen.getByDisplayValue('Later')).toBeInTheDocument()
    expect(screen.queryByDisplayValue('Write')).not.toBeInTheDocument()
    expect(screen.getByTestId('go-to-day')).toHaveValue('2026-03-10')
  })

  // A browser reports a date field changed at every keystroke, with the year typed so far. Those are
  // not days to turn to, and turning to them would write them back under the reader's fingers.
  it('waits for a whole year before turning to a typed day', () => {
    render(<Providers><DayPlanner {...planned(null, entry('id-2', 'Later', '2026-03-10', { ctStart: '14:00', ctEnd: '15:00' }))} /></Providers>)
    const field = screen.getByTestId('go-to-day')

    for (const typed of ['0002-03-10', '0020-03-10', '0202-03-10']) {
      fireEvent.change(field, { target: { value: typed } })
      expect(screen.getByText(/Jan 03/)).toBeInTheDocument()
      expect(field).toHaveValue(typed)
    }
    fireEvent.change(field, { target: { value: '2026-03-10' } })

    expect(screen.getByText(/Mar 10/)).toBeInTheDocument()
    expect(screen.getByDisplayValue('Later')).toBeInTheDocument()
  })

  // The field follows the toolbar too, so it always names the day that is showing.
  it('names the day the toolbar turned to', async () => {
    render(<Providers><DayPlanner {...planned(null, entry('id-1', 'Write', TODAY, { ctStart: '09:00', ctEnd: '10:30' }))} /></Providers>)

    await userEvent.click(screen.getByRole('button', { name: 'Next' }))

    expect(screen.getByTestId('go-to-day')).toHaveValue('2026-01-04')
  })

  // A block of the day holds its entry on one line, which a block a week wide has room for too.
  it('draws a block of a week on one line', async () => {
    const { container } = render(<Providers><DayPlanner {...planned(null, entry('id-1', 'Write', TODAY, { ctStart: '09:00', ctEnd: '09:45' }))} /></Providers>)

    await userEvent.click(screen.getByRole('button', { name: 'Week' }))

    const block = container.querySelector('.rbc-time-content .rbc-event')
    expect(within(block).getByDisplayValue('Write')).toBeInTheDocument()
  })

  // A day opens at its first block, and a week at the first block of any of its days, so that no
  // block of what is showing starts out scrolled above it. A day with no block opens in the morning.
  it('opens at the earliest block of what is showing', () => {
    const at = (day, hours, minutes) => { const d = new Date(2026, 0, day); d.setHours(hours, minutes); return d }
    const events = [
      { allDay: false, start: at(3, 9, 0) },
      { allDay: false, start: at(4, 8, 0) },
      { allDay: true, start: at(3, 0, 0) },
    ]
    const opened = (date, view) => { const t = openingTime(events, date, view); return [t.getHours(), t.getMinutes()] }

    expect(opened(new Date(2026, 0, 3), 'day')).toEqual([9, 0])
    expect(opened(new Date(2026, 0, 3), 'week')).toEqual([8, 0])
    expect(opened(new Date(2026, 0, 5), 'day')).toEqual([6, 0])
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
