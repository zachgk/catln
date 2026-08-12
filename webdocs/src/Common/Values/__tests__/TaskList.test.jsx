import React from 'react'
import { render, screen, waitFor } from '@testing-library/react'
import userEvent from '@testing-library/user-event'
import { ThemeProvider, createTheme } from '@mui/material/styles'
import { MemoryRouter } from 'react-router-dom'
import { ConfigContext, ReloadContext } from '../../Common'
import TaskList from '../TaskList'

const theme = createTheme()

const NOTE = '/vault/Tasks.md'
const OTHER_NOTE = '/vault/Notes/Later.md'

// One entry of a compiled /Data/Note/Show/TaskList: the task's own value, beside the page it was
// written in and the expression it was declared by. Those last two are what a list across notes
// carries and a note's own page does not, since the page displaying the list is not the note.
function entry(id, contents, done, page = NOTE) {
  return {
    ptExpr: id,
    ptPage: page,
    ptTask: {
      tag: 'TupleVal',
      name: '/Data/Note/Task',
      args: {
        '/contents': { tag: 'StrVal', contents },
        '/state': { tag: 'TupleVal', name: done ? '/Data/Primitive/True' : '/Data/Primitive/False', args: {} },
      },
    },
  }
}

function taskList(...entries) {
  return { '/tasks': { tag: 'StrVal', contents: JSON.stringify(entries) } }
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

describe('TaskList', () => {
  beforeEach(() => {
    global.fetch = vi.fn(saved)
  })

  it('shows every task it is given, as the task it is', () => {
    render(<Providers><TaskList {...taskList(
      entry('id-1', 'Read it', false),
      entry('id-2', 'Write this note', true),
    )} /></Providers>)

    const boxes = screen.getAllByRole('checkbox')
    expect(boxes).toHaveLength(2)
    expect(boxes[0]).not.toBeChecked()
    expect(boxes[1]).toBeChecked()
    expect(screen.getAllByRole('textbox').map(t => t.value)).toEqual(['Read it', 'Write this note'])
  })

  it('shows the tasks in the order they are given', () => {
    render(<Providers><TaskList {...taskList(
      entry('id-1', 'First', false),
      entry('id-2', 'Second', false),
      entry('id-3', 'Third', false),
    )} /></Providers>)

    expect(screen.getAllByRole('textbox').map(t => t.value)).toEqual(['First', 'Second', 'Third'])
  })

  // The whole point of the list: a task gathered from another note is written back to that note,
  // not to whatever page is displaying the list.
  it('writes a checked task back to the note it was read from', async () => {
    render(<Providers><TaskList {...taskList(
      entry('id-1', 'Read it', false),
      entry('id-2', 'Plan it', false, OTHER_NOTE),
    )} /></Providers>)

    await userEvent.click(screen.getAllByRole('checkbox')[1])

    await waitFor(() => expect(global.fetch).toHaveBeenCalled())
    const [url, options] = global.fetch.mock.calls[0]
    expect(url).toBe(`/api/write-expr?prgmName=${encodeURIComponent(OTHER_NOTE)}&metaId=id-2`)
    expect(options.method).toBe('POST')
    expect(options.body).toBe('/Data/Note/Task(contents= "Plan it", state= /Data/Primitive/True)')
  })

  it('reads the display again once the note has been written', async () => {
    const reload = vi.fn()
    render(<Providers reload={reload}><TaskList {...taskList(entry('id-1', 'Read it', false))} /></Providers>)

    await userEvent.click(screen.getByRole('checkbox'))

    await waitFor(() => expect(reload).toHaveBeenCalled())
  })

  // A save that failed leaves the notes as they were, so re-reading would show the same list and
  // say nothing about why. The failure is reported instead.
  it('reports a save that failed and does not read the display again', async () => {
    const reload = vi.fn()
    global.fetch = vi.fn(() => Promise.resolve({
      status: 200,
      json: () => Promise.resolve({ tag: 'ResFail', contents: [{ msg: 'Saving is disabled' }] }),
    }))
    render(<Providers reload={reload}><TaskList {...taskList(entry('id-1', 'Read it', false))} /></Providers>)

    await userEvent.click(screen.getByRole('checkbox'))

    expect(await screen.findByTestId('task-list-error')).toHaveTextContent('Saving is disabled')
    expect(reload).not.toHaveBeenCalled()
  })

  // A list of tasks nothing can be written from is still a list of tasks to read.
  it('is read-only against a readonly doc server', () => {
    render(<Providers readonly={true}><TaskList {...taskList(entry('id-1', 'Read it', false))} /></Providers>)

    expect(screen.getByRole('checkbox')).toBeDisabled()
    expect(screen.getByRole('textbox')).toHaveAttribute('readonly')
  })

  // Notes with no checkboxes are a list with nothing in it, not a page that failed to arrive.
  it('says so when there are no tasks', () => {
    render(<Providers><TaskList {...taskList()} /></Providers>)

    expect(screen.getByText('No tasks.')).toBeInTheDocument()
    expect(screen.queryByRole('checkbox')).not.toBeInTheDocument()
  })

  it('links each task to the note it was written in', () => {
    render(<Providers><TaskList {...taskList(entry('id-1', 'Plan it', false, OTHER_NOTE))} /></Providers>)

    expect(screen.getByRole('link', { name: 'Later.md' }))
      .toHaveAttribute('href', `/docs/${encodeURIComponent(OTHER_NOTE)}`)
  })
})
