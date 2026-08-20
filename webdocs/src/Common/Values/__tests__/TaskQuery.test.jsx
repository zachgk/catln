import React from 'react'
import { render, screen, waitFor } from '@testing-library/react'
import userEvent from '@testing-library/user-event'
import { ThemeProvider, createTheme } from '@mui/material/styles'
import { MemoryRouter } from 'react-router-dom'
import { ConfigContext, ReloadContext } from '../../Common'
import TaskQuery from '../TaskQuery'

const theme = createTheme()

const NOTE = '/vault/Tasks.md'

// One task of what a query found, which is the same shape a list of every task carries: the task's
// own value, beside the page it was written in and the expression it was declared by.
function entry(id, contents, done, page = NOTE) {
  return {
    ptExpr: id,
    ptPage: page,
    ptTask: {
      tag: 'TupleVal',
      name: '/Data/Note/Task/Simple',
      args: {
        '/contents': { tag: 'StrVal', contents },
        '/state': { tag: 'TupleVal', name: done ? '/Data/Primitive/True' : '/Data/Primitive/False', args: {} },
      },
    },
  }
}

// A compiled /Data/Note/Show/TaskQuery, which carries what the query found and why nothing was found
// in the one result.
function answered(qrError, ...entries) {
  return { '/result': { tag: 'StrVal', contents: JSON.stringify({ qrError, qrTasks: entries }) } }
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

describe('TaskQuery', () => {
  beforeEach(() => {
    global.fetch = vi.fn(saved)
  })

  it('shows the tasks the query found, as the tasks they are', () => {
    render(<Providers><TaskQuery {...answered(
      null,
      entry('id-1', 'Read it', false),
      entry('id-2', 'Write this note', true),
    )} /></Providers>)

    expect(screen.getByDisplayValue('Read it')).toBeInTheDocument()
    expect(screen.getByDisplayValue('Write this note')).toBeInTheDocument()
  })

  it('names the note each task it found was written in', () => {
    render(<Providers><TaskQuery {...answered(null, entry('id-1', 'Read it', false))} /></Providers>)

    expect(screen.getByRole('link', { name: 'Tasks.md' })).toHaveAttribute(
      'href', `/docs/${encodeURIComponent(NOTE)}`)
  })

  // A query a reader typed which nobody can read is an ordinary answer rather than a page which
  // failed: the message belongs where the reader wrote the query.
  it('says which line of the query was not understood, where the query was written', () => {
    render(<Providers><TaskQuery {...answered('do not understand the query instruction: soonish')} /></Providers>)

    expect(screen.getByTestId('task-query-error')).toHaveTextContent('soonish')
  })

  it('says so when nothing the query asked for is there', () => {
    render(<Providers><TaskQuery {...answered(null)} /></Providers>)

    expect(screen.getByText('No tasks match this query.')).toBeInTheDocument()
  })

  // A task found by a query is written back to the note it came from, which is not the note showing
  // the query. So the write names the page and expression the task carries.
  it('writes a checked box back to the note the task came from', async () => {
    const reload = vi.fn()
    render(<Providers reload={reload}><TaskQuery {...answered(null, entry('id-1', 'Read it', false))} /></Providers>)

    await userEvent.click(screen.getByRole('checkbox'))

    await waitFor(() => expect(global.fetch).toHaveBeenCalled())
    const [url] = global.fetch.mock.calls[0]
    expect(url).toContain(encodeURIComponent(NOTE))
    expect(url).toContain('id-1')
    await waitFor(() => expect(reload).toHaveBeenCalled())
  })
})
