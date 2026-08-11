import React from 'react'
import { render, screen } from '@testing-library/react'
import userEvent from '@testing-library/user-event'
import { ThemeProvider, createTheme } from '@mui/material/styles'
import Task from '../Task'

const theme = createTheme()

// The args of a compiled /Data/Note/Task: what the checkbox says, and whether it is checked. A task
// is the value the note declares rather than a display standing in for one, so these are the fields
// of the type itself.
function task(contents = 'Read it', done = false) {
  return {
    '/contents': { tag: 'StrVal', contents },
    '/state': { tag: 'TupleVal', name: done ? '/Data/Primitive/True' : '/Data/Primitive/False', args: {} },
  }
}

function Providers({ children }) {
  return <ThemeProvider theme={theme}>{children}</ThemeProvider>
}

describe('Task', () => {
  it('shows what the checkbox says and whether it is checked', () => {
    render(<Providers><Task {...task('Write this note', true)} update={vi.fn()} /></Providers>)

    expect(screen.getByRole('checkbox')).toBeChecked()
    expect(screen.getByRole('textbox')).toHaveValue('Write this note')
  })

  it('shows an unchecked box unchecked', () => {
    render(<Providers><Task {...task()} update={vi.fn()} /></Providers>)

    expect(screen.getByRole('checkbox')).not.toBeChecked()
  })

  it('checking the box declares the task with the other state', async () => {
    const update = vi.fn()
    render(<Providers><Task {...task('Read it', false)} update={update} /></Providers>)

    await userEvent.click(screen.getByRole('checkbox'))

    expect(update).toHaveBeenCalledWith('/Data/Note/Task(contents= "Read it", state= /Data/Primitive/True)')
  })

  it('unchecking the box declares the task with the other state', async () => {
    const update = vi.fn()
    render(<Providers><Task {...task('Read it', true)} update={update} /></Providers>)

    await userEvent.click(screen.getByRole('checkbox'))

    expect(update).toHaveBeenCalledWith('/Data/Note/Task(contents= "Read it", state= /Data/Primitive/False)')
  })

  // Typing is not the edit — finishing with the box is. Otherwise every keystroke would write the
  // note back and recompile the page it is in.
  it('does not write the contents while they are being typed', async () => {
    const update = vi.fn()
    render(<Providers><Task {...task()} update={update} /></Providers>)

    await userEvent.type(screen.getByRole('textbox'), ' again')

    expect(screen.getByRole('textbox')).toHaveValue('Read it again')
    expect(update).not.toHaveBeenCalled()
  })

  it('writes the contents once they are finished with', async () => {
    const update = vi.fn()
    render(<Providers><Task {...task('Read it', true)} update={update} /></Providers>)

    await userEvent.type(screen.getByRole('textbox'), ' again')
    await userEvent.tab()

    expect(update).toHaveBeenCalledTimes(1)
    expect(update).toHaveBeenCalledWith('/Data/Note/Task(contents= "Read it again", state= /Data/Primitive/True)')
  })

  it('writes the contents when the edit is entered', async () => {
    const update = vi.fn()
    render(<Providers><Task {...task()} update={update} /></Providers>)

    await userEvent.type(screen.getByRole('textbox'), ' again{Enter}')

    expect(update).toHaveBeenCalledWith('/Data/Note/Task(contents= "Read it again", state= /Data/Primitive/False)')
  })

  it('does not write contents that were not changed', async () => {
    const update = vi.fn()
    render(<Providers><Task {...task()} update={update} /></Providers>)

    await userEvent.click(screen.getByRole('textbox'))
    await userEvent.tab()

    expect(update).not.toHaveBeenCalled()
  })

  // A task shown where nothing can be written is still a task to read.
  it('is read-only without an update', async () => {
    render(<Providers><Task {...task('Read it', true)} /></Providers>)

    expect(screen.getByRole('checkbox')).toBeDisabled()
    expect(screen.getByRole('textbox')).toHaveAttribute('readonly')
    expect(screen.getByRole('textbox')).toHaveValue('Read it')
  })

  // Disabling the controls is how a task says it cannot be edited; it is not what stops the edit.
  // A change event still reaches a disabled checkbox, and without an update there is nothing to
  // call — so the handler has to answer for that itself.
  it('does nothing when a change reaches it without an update', () => {
    render(<Providers><Task {...task()} /></Providers>)

    expect(() => screen.getByRole('checkbox').click()).not.toThrow()
    expect(screen.getByRole('checkbox')).not.toBeChecked()
  })

  // The contents the compiler answered with are what the task says, so a page read again replaces
  // whatever an edit typed its way to.
  it('shows the contents it is given again when the page is read again', () => {
    const { rerender } = render(<Providers><Task {...task('Read it')} update={vi.fn()} /></Providers>)

    rerender(<Providers><Task {...task('Read it twice')} update={vi.fn()} /></Providers>)

    expect(screen.getByRole('textbox')).toHaveValue('Read it twice')
  })

  it('writes contents holding a quote as a string the compiler can read back', async () => {
    const update = vi.fn()
    render(<Providers><Task {...task('Read')} update={update} /></Providers>)

    await userEvent.type(screen.getByRole('textbox'), ' "it"{Enter}')

    expect(update).toHaveBeenCalledWith('/Data/Note/Task(contents= "Read \\"it\\"", state= /Data/Primitive/False)')
  })
})
