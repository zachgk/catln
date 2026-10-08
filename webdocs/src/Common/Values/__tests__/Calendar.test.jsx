import React from 'react'
import { fireEvent, render, screen, waitFor, within } from '@testing-library/react'
import userEvent from '@testing-library/user-event'
import { ThemeProvider, createTheme } from '@mui/material/styles'
import { MemoryRouter } from 'react-router-dom'
import { ConfigContext, ReloadContext } from '../../Common'
import Calendar from '../Calendar'

const theme = createTheme()

const NOTE = '/vault/Tasks.md'

// The day the answers below were worked out on, which is the day the compiler was given and not any
// day a clock says.
const TODAY = '2026-01-03'

// One entry of what a calendar found: the value itself, the day it sits on, and where it was written.
function entry(id, contents, ciDay, page = NOTE) {
  return {
    ciExpr: id,
    ciPage: page,
    ciDay,
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

// A compiled /Data/Calendar/Show/Calendar, which carries what was found, why nothing was, and the
// day the question was asked on, all in the one result.
function calendared(crError, ...items) {
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

// Turns the calendar over to one of the four ways of reading it, which is a button of its own toolbar.
async function showAs(view) {
  await userEvent.click(screen.getByRole('button', { name: view }))
}

describe('Calendar', () => {
  beforeEach(() => {
    global.fetch = vi.fn(saved)
  })

  afterEach(() => {
    vi.useRealTimers()
  })

  it('lays an entry on the day it sits on', () => {
    render(<Providers><Calendar {...calendared(null, entry('id-1', 'Ship it', '2026-01-04'))} /></Providers>)

    expect(screen.getByDisplayValue('Ship it')).toBeInTheDocument()
  })

  // One answer read four ways. Turning the calendar over asks the compiler nothing, so every way of
  // reading it shows the same entries.
  it.each(['Day', 'Week', 'Month', 'Agenda'])('shows the one answer read as a %s', async view => {
    render(<Providers><Calendar {...calendared(null, entry('id-1', 'Answer it', TODAY))} /></Providers>)

    await showAs(view)

    expect(screen.getByDisplayValue('Answer it')).toBeInTheDocument()
    // Read four ways and parsed once: the answer is turned over rather than asked for again.
    expect(global.fetch).not.toHaveBeenCalled()
  })

  // The day a calendar marks is the day its answer was worked out on. A browser's own clock is a
  // different clock in a different place, and reading it here would mark a day the answer was not
  // about -- and would make what this test sees depend on the day it runs.
  it('opens on the day the answer was worked out on and not on the clock', () => {
    vi.useFakeTimers()
    vi.setSystemTime(new Date('2030-06-15T12:00:00Z'))

    const { container } = render(
      <Providers><Calendar {...calendared(null, entry('id-1', 'Ship it', '2026-01-04'))} /></Providers>)

    expect(screen.getByText('January 2026')).toBeInTheDocument()
    // And the day it marks is that day rather than one of June 2030, which is what the clock says.
    expect(container.querySelectorAll('.rbc-today').length).toBeGreaterThan(0)
  })

  // A month far from the one the calendar opened on is reached by naming a day of it.
  it('turns to a day named in the field for it', () => {
    render(<Providers><Calendar {...calendared(null, entry('id-1', 'Later', '2026-09-14'))} /></Providers>)

    fireEvent.change(screen.getByTestId('go-to-day'), { target: { value: '2026-09-14' } })

    expect(screen.getByText('September 2026')).toBeInTheDocument()
    expect(screen.getByDisplayValue('Later')).toBeInTheDocument()
  })

  // An entry given a time of day is drawn over that time rather than across the whole day.
  it('draws an entry with a time of day over that time', async () => {
    const timed = { ...entry('id-1', 'Write', TODAY), ciTime: { ctStart: '09:00', ctEnd: '10:30' } }
    const { container } = render(<Providers><Calendar {...calendared(null, timed, entry('id-2', 'Read', TODAY))} /></Providers>)

    await showAs('Day')

    const grid = container.querySelector('.rbc-time-content')
    expect(within(grid).getByDisplayValue('Write')).toBeInTheDocument()
    expect(within(grid).queryByDisplayValue('Read')).not.toBeInTheDocument()
    expect(within(container.querySelector('.rbc-allday-cell')).getByDisplayValue('Read')).toBeInTheDocument()
  })

  // The gather found these, so a calendar which quietly left them off would be showing less than it
  // was handed.
  it('shows the entries which sit on no day, apart from the days', () => {
    render(<Providers><Calendar {...calendared(
      null,
      entry('id-1', 'Ship it', '2026-01-04'),
      entry('id-2', 'Book lunch', null),
    )} /></Providers>)

    const undated = screen.getByTestId('calendar-undated')
    expect(within(undated).getByDisplayValue('Book lunch')).toBeInTheDocument()
    expect(within(undated).queryByDisplayValue('Ship it')).not.toBeInTheDocument()
  })

  it('names the page an entry which sits on no day was written in', () => {
    render(<Providers><Calendar {...calendared(null, entry('id-2', 'Book lunch', null))} /></Providers>)

    expect(screen.getByRole('link', { name: 'Tasks.md' })).toHaveAttribute(
      'href', `/docs/${encodeURIComponent(NOTE)}`)
  })

  // An entry is drawn as whatever draws a value of its kind, so a calendar shows a kind of thing it
  // has never heard of. Nothing here matches on what a task is.
  it('draws an entry of a kind it knows nothing about as that kind is drawn', () => {
    const meeting = {
      ciExpr: 'id-9',
      ciPage: '/vault/Diary.md',
      ciDay: '2026-01-05',
      ciValue: { tag: 'TupleVal', name: '/Diary/Meeting', args: { '/name': { tag: 'StrVal', contents: 'Stand up' } } },
    }
    render(<Providers><Calendar {...calendared(null, meeting)} /></Providers>)

    expect(screen.getByText('Stand up')).toBeInTheDocument()
  })

  // A calendar which cannot be answered is an ordinary answer and not a page which failed.
  it('says why nothing was found, where the calendar was written', () => {
    render(<Providers><Calendar {...calendared('This evaluation is not of one page.')} /></Providers>)

    expect(screen.getByTestId('calendar-error')).toHaveTextContent('not of one page')
  })

  // An entry is written back to the page it came from, which is not the page showing the calendar.
  it('writes a checked box back to the page the entry came from', async () => {
    const reload = vi.fn()
    render(<Providers reload={reload}><Calendar {...calendared(null, entry('id-1', 'Ship it', '2026-01-04'))} /></Providers>)

    await userEvent.click(screen.getByRole('checkbox'))

    await waitFor(() => expect(global.fetch).toHaveBeenCalled())
    const [url] = global.fetch.mock.calls[0]
    expect(url).toContain(encodeURIComponent(NOTE))
    expect(url).toContain('id-1')
    await waitFor(() => expect(reload).toHaveBeenCalled())
  })

  it('does not offer to change an entry when the doc server is read only', () => {
    render(<Providers readonly><Calendar {...calendared(null, entry('id-1', 'Ship it', '2026-01-04'))} /></Providers>)

    expect(screen.getByRole('checkbox')).toBeDisabled()
  })
})
