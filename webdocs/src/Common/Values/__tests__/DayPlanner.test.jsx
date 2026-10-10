import React from 'react'
import { fireEvent, render, screen, waitFor, within } from '@testing-library/react'
import userEvent from '@testing-library/user-event'
import { ThemeProvider, createTheme } from '@mui/material/styles'
import { MemoryRouter } from 'react-router-dom'
import { ConfigContext, ReloadContext } from '../../Common'
import DayPlanner, { createOps, editOps, openingTime, placedValue } from '../DayPlanner'

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
    // Where the pages can be written the planner is selectable, and react-big-calendar asks what is
    // under the pointer on every click, which jsdom has no layout to answer.
    document.elementFromPoint = () => null
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
    expect(within(screen.getByTestId('planner-unscheduled')).getByDisplayValue('Read')).toBeInTheDocument()
    expect(within(timeline).queryByDisplayValue('Read')).not.toBeInTheDocument()
  })

  // The strip holds what sits on the days drawn and nothing of any other day, and each of its
  // entries is dragged the way anything outside the calendar is, so that the calendar takes it up.
  it('draws the entries of the day with no time in a strip of their own, to be dragged', () => {
    render(<Providers><DayPlanner {...planned(
      null,
      entry('id-2', 'Read', TODAY),
      entry('id-4', 'Later', '2026-01-04'),
    )} /></Providers>)

    const strip = screen.getByTestId('planner-unscheduled')
    expect(within(strip).getByDisplayValue('Read')).toBeInTheDocument()
    expect(within(strip).queryByDisplayValue('Later')).not.toBeInTheDocument()
    expect(within(strip).getByTestId('planner-unscheduled-entry')).toHaveAttribute('draggable', 'true')
  })

  // Read only, an entry of the strip is not dragged, and an empty strip is not drawn at all.
  it('neither drags the strip nor draws an empty one where nothing can be written', () => {
    const { unmount } = render(<Providers readonly><DayPlanner {...planned(null, entry('id-2', 'Read', TODAY))} /></Providers>)
    expect(screen.getByTestId('planner-unscheduled-entry')).toHaveAttribute('draggable', 'false')
    unmount()

    render(<Providers readonly><DayPlanner {...planned(null, entry('id-1', 'Write', TODAY, { ctStart: '09:00', ctEnd: '10:30' }))} /></Providers>)
    expect(screen.queryByTestId('planner-unscheduled')).not.toBeInTheDocument()
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

  // A block taken hold of and let go of over the strip has its time taken off, which is written back
  // as its value with no time on the day it was on.
  it('takes the time off a block let go of over the strip', async () => {
    const { container } = render(<Providers><DayPlanner {...planned(
      null,
      entry('id-1', 'Write', TODAY, { ctStart: '09:00', ctEnd: '10:30' }),
    )} /></Providers>)

    fireEvent.mouseDown(container.querySelector('.rbc-time-content .rbc-event'))
    fireEvent.mouseUp(screen.getByTestId('planner-unscheduled'))

    await waitFor(() => expect(global.fetch).toHaveBeenCalled())
    const [url, request] = global.fetch.mock.calls[0]
    expect(url).toBe('/api/write-exprs')
    const [op] = JSON.parse(request.body)
    expect(op).toMatchObject({ tag: 'Replace', metaId: 'id-1' })
    expect(op.src).toContain('time= /Data/Maybe/Nothing')
    expect(op.src).toContain('day= 3')
  })

  // A block taken hold of and let go of anywhere else is not taken off its time by the strip.
  it('leaves a block let go of anywhere but the strip to the calendar', async () => {
    const { container } = render(<Providers><DayPlanner {...planned(
      null,
      entry('id-1', 'Write', TODAY, { ctStart: '09:00', ctEnd: '10:30' }),
    )} /></Providers>)

    const block = container.querySelector('.rbc-time-content .rbc-event')
    fireEvent.mouseDown(block)
    fireEvent.mouseUp(block)

    expect(global.fetch).not.toHaveBeenCalled()
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
  // Where pages can be written, the plugin's edit modes are offered; where they cannot, nothing is.
  it('offers the edit modes only where the pages can be written', () => {
    const { unmount } = render(<Providers><DayPlanner {...planned(null, entry('id-1', 'Write', TODAY, { ctStart: '09:00', ctEnd: '10:30' }))} /></Providers>)
    expect(screen.getByRole('button', { name: 'Push neighbors' })).toBeInTheDocument()
    unmount()

    render(<Providers readonly><DayPlanner {...planned(null, entry('id-1', 'Write', TODAY, { ctStart: '09:00', ctEnd: '10:30' }))} /></Providers>)
    expect(screen.queryByRole('button', { name: 'Push neighbors' })).not.toBeInTheDocument()
  })

  it('says where nothing shown is from a note to create a block in', () => {
    const fromNoNote = { ...entry('id-1', 'Write', TODAY, { ctStart: '09:00', ctEnd: '10:30' }), ciValue: { tag: 'TupleVal', name: '/Other/Thing', args: {} } }
    render(<Providers><DayPlanner {...planned(null, fromNoNote)} /></Providers>)
    expect(screen.getByTestId('planner-no-create')).toBeInTheDocument()
  })
})

// The events the planner draws the entries above as, built the way it builds them.
const local = (hours, minutes = 0, day = 3) => { const d = new Date(2026, 0, day); d.setHours(hours, minutes, 0, 0); return d }
function block(id, contents, from, to, name = '/Data/Note/Task/Simple') {
  const item = entry(id, contents, TODAY, { ctStart: '00:00', ctEnd: '00:00' })
  item.ciValue.name = name
  return { id, allDay: false, start: local(...from), end: local(...to), item }
}

describe('editing a day planner', () => {
  // A moved block is written back whole with its time and day changed, which the importer writes
  // into its line.
  it('writes a moved block back as its value with its new time and day', () => {
    const write = block('id-1', 'Write', [9], [10])
    const { ops } = editOps([write], { event: write, start: local(11), end: local(12) }, 'move')
    expect(ops).toEqual([{ tag: 'Replace', page: NOTE, metaId: 'id-1', src: expect.stringContaining('time= /Data/Maybe/Some(value= /Data/Time/TimeRange(start= /Data/Time/TimeOfDay(hour= 11, minute= 0), end= /Data/Time/TimeOfDay(hour= 12, minute= 0)))') }])
    expect(ops[0].src).toContain('plannedDay= /Data/Maybe/Some(value= /Data/Time/Date(year= 2026, month= 1, day= 3))')
    expect(ops[0].src).toContain('contents= "Write"')
  })

  // An entry dragged out of the strip onto the day is given the plugin's default length.
  it('gives an entry dragged onto the day the default length', () => {
    const read = { ...block('id-2', 'Read', [0], [0]), allDay: true }
    const { changes } = editOps([read], { event: read, start: local(14), end: local(14) }, 'move')
    expect(changes[0].end).toEqual(local(14, 30))
  })

  // A block put across the top of a day has its time taken off, which an event cannot have.
  it('takes the time off a task put across the top of a day, and refuses to for an event', () => {
    const write = block('id-1', 'Write', [9], [10])
    const { ops } = editOps([write], { event: write, start: local(0, 0, 4), end: local(0, 0, 4), isAllDay: true }, 'move')
    expect(ops[0].src).toContain('time= /Data/Maybe/Nothing')
    expect(ops[0].src).toContain('day= 4')

    const lunch = block('id-3', 'Lunch', [12], [13], '/Data/Note/Event')
    expect(editOps([lunch], { event: lunch, start: local(0), end: local(0), isAllDay: true }, 'move').error).toMatch(/event/)
  })

  // Pushing writes every block it pushed, in the one batch.
  it('writes back the blocks a push moved', () => {
    const a = block('id-a', 'A', [9], [10])
    const b = block('id-b', 'B', [10], [11])
    const { ops } = editOps([a, b], { event: a, start: local(9, 30), end: local(10, 30) }, 'push')
    expect(ops.map(o => o.metaId)).toEqual(['id-a', 'id-b'])
    expect(ops[1].src).toContain('start= /Data/Time/TimeOfDay(hour= 10, minute= 30)')
  })

  // A copy is a new block written after the original, which is left where it was.
  it('writes a copy after the original', () => {
    const write = block('id-1', 'Write', [9], [10])
    const { ops } = editOps([write], { event: write, start: local(15), end: local(16) }, 'copy')
    expect(ops).toEqual([expect.objectContaining({ tag: 'InsertAfter', metaId: 'id-1' })])
  })

  // An event's time is its span and not a span it may not have.
  it('writes an event with its span rather than a maybe of one', () => {
    const lunch = block('id-3', 'Lunch', [12], [13], '/Data/Note/Event')
    expect(placedValue(lunch.item.ciValue, { ...lunch, start: local(12, 30), end: local(13, 30) }).args['/time'].name).toBe('/Data/Time/TimeRange')
  })

  // A created block is a task or a bullet over the span it was drawn on, on its day.
  it('creates a task or a bullet over the span drawn', () => {
    const [task] = createOps(NOTE, { start: local(8), end: local(8, 30) }, 'Stretch', 'task')
    expect(task.tag).toBe('Create')
    expect(task.page).toBe(NOTE)
    expect(task.src.startsWith('/Data/Note/Task/Simple(contents= "Stretch", state= /Data/Primitive/False, time= /Data/Maybe/Some')).toBe(true)
    const [bullet] = createOps(NOTE, { start: local(8), end: local(8, 30) }, 'Stretch', 'bullet')
    expect(bullet.src.startsWith('/Data/Note/Event(contents= "Stretch", time= /Data/Time/TimeRange')).toBe(true)
  })
})
