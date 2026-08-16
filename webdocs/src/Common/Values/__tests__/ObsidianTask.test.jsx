import React from 'react'
import { render, screen } from '@testing-library/react'
import { ThemeProvider, createTheme } from '@mui/material/styles'
import ObsidianTask from '../ObsidianTask'

const theme = createTheme()

// A /Data/Maybe as it arrives: the Some arm carrying a value, or the Nothing arm carrying none.
// These are the first fields of this shape the webdocs have had to read, so what a case is written
// against is the compiled shape itself rather than a convenience for it.
function some(value) {
  return { tag: 'TupleVal', name: '/Data/Some', args: { '/value': { tag: 'StrVal', contents: value } } }
}

const NOTHING = { tag: 'TupleVal', name: '/Data/Nothing', args: {} }

function str(contents) {
  return { tag: 'StrVal', contents }
}

function member(name) {
  return { tag: 'TupleVal', name, args: {} }
}

// The args of a compiled /Data/Note/Task/Obsidian: the line as it was written, the status the vault
// gave the box, the few fields worth naming, and the rest as the JSON they travel in.
function task({rest = {}, ...fields} = {}) {
  return {
    '/contents': str('Ship it'),
    '/symbol': str(' '),
    '/statusType': member('/Data/Note/StatusType/Todo'),
    '/description': str('Ship it'),
    '/priority': member('/Data/Note/Priority/None'),
    '/scheduled': NOTHING,
    '/due': NOTHING,
    '/recurrence': NOTHING,
    ...fields,
    '/rest': str(JSON.stringify({ statusName: 'Todo', ...rest })),
  }
}

function Providers({ children }) {
  return <ThemeProvider theme={theme}>{children}</ThemeProvider>
}

describe('ObsidianTask', () => {
  it('shows what the task says and leaves the box unchecked', () => {
    render(<Providers><ObsidianTask {...task()} /></Providers>)

    expect(screen.getByRole('checkbox')).not.toBeChecked()
    expect(screen.getByText('Ship it')).toBeInTheDocument()
  })

  // The box is read from the status the vault named and never from the character in it, since which
  // character means done is the vault's to say.
  it('a box the vault calls done is checked', () => {
    render(<Providers><ObsidianTask {...task({
      '/symbol': str('x'),
      '/statusType': member('/Data/Note/StatusType/Done'),
      rest: { statusName: 'Done' },
    })} /></Providers>)

    expect(screen.getByRole('checkbox')).toBeChecked()
  })

  // A status the vault named itself is shown by that name, which is the only thing saying what a
  // box holding something other than a space or an x means.
  it('shows the name the vault gave the status', () => {
    render(<Providers><ObsidianTask {...task({
      '/symbol': str('/'),
      '/statusType': member('/Data/Note/StatusType/InProgress'),
      rest: { statusName: 'In Progress' },
    })} /></Providers>)

    expect(screen.getByText('In Progress')).toBeInTheDocument()
    expect(screen.getByRole('checkbox')).not.toBeChecked()
  })

  it('shows a date carried as a field of its own', () => {
    render(<Providers><ObsidianTask {...task({'/due': some('2026-01-04')})} /></Providers>)

    expect(screen.getByText('due: 2026-01-04')).toBeInTheDocument()
  })

  // A field of the value and a field out of the JSON are shown the same way, so which of them is
  // which is not something a reader of the page can see.
  it('shows a date carried in the rest just the same', () => {
    render(<Providers><ObsidianTask {...task({rest: {created: '2026-01-01'}})} /></Providers>)

    expect(screen.getByText('created: 2026-01-01')).toBeInTheDocument()
  })

  // The date a task was finished or dropped is one field, and the status says which of the two it is.
  it('names the status date for what the status is', () => {
    render(<Providers><ObsidianTask {...task({
      '/statusType': member('/Data/Note/StatusType/Done'),
      rest: { statusName: 'Done', statusDate: '2026-01-05' },
    })} /></Providers>)

    expect(screen.getByText('done: 2026-01-05')).toBeInTheDocument()
  })

  // Every field is shown only where the task carries one, so a plain checkbox stays a plain
  // checkbox rather than a row of empty labels.
  it('shows nothing for a field the task does not carry', () => {
    render(<Providers><ObsidianTask {...task()} /></Providers>)

    expect(screen.queryByText(/^due:/)).not.toBeInTheDocument()
    expect(screen.queryByText(/^priority:/)).not.toBeInTheDocument()
    expect(screen.queryByText(/^waits on:/)).not.toBeInTheDocument()
  })

  it('shows the priority, the recurrence and the tags', () => {
    render(<Providers><ObsidianTask {...task({
      '/priority': member('/Data/Note/Priority/High'),
      '/recurrence': some('every week'),
      rest: { statusName: 'Todo', tags: ['#work', '#urgent'] },
    })} /></Providers>)

    expect(screen.getByText('priority: high')).toBeInTheDocument()
    expect(screen.getByText('repeats: every week')).toBeInTheDocument()
    expect(screen.getByText('#work')).toBeInTheDocument()
    expect(screen.getByText('#urgent')).toBeInTheDocument()
  })

  it('shows an id and the ids the task waits on', () => {
    render(<Providers><ObsidianTask {...task({
      rest: { statusName: 'Todo', id: 'dcf64c', dependsOn: ['dcf64c', '0h17ye'] },
    })} /></Providers>)

    expect(screen.getByText('id: dcf64c')).toBeInTheDocument()
    expect(screen.getByText('waits on: dcf64c, 0h17ye')).toBeInTheDocument()
  })

  // Reading a line apart is what the importer does; writing one back from its parts is not, so
  // there is nothing to edit with and the box says so by being disabled.
  it('cannot be edited', () => {
    render(<Providers><ObsidianTask {...task()} /></Providers>)

    expect(screen.getByRole('checkbox')).toBeDisabled()
  })
})
