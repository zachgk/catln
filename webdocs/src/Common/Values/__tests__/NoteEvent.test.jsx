import React from 'react'
import { render, screen } from '@testing-library/react'
import NoteEvent from '../NoteEvent'

function event(contents) {
  return {
    '/contents': { tag: 'StrVal', contents },
    '/time': { tag: 'TupleVal', name: '/Data/Time/TimeRange', args: {} },
    '/plannedDay': { tag: 'TupleVal', name: '/Data/Maybe/Nothing', args: {} },
  }
}

describe('NoteEvent', () => {
  // A bullet is shown as the line it was written as, with no box: there is nothing to check off.
  it('shows the line it was written as and no box', () => {
    render(<NoteEvent {...event('12:00 - 13:00 Lunch')} />)

    expect(screen.getByText('12:00 - 13:00 Lunch')).toBeInTheDocument()
    expect(screen.queryByRole('checkbox')).not.toBeInTheDocument()
  })

  // Dense, the line is cut short where it does not fit and is read whole from its tooltip.
  it('carries the whole line in its tooltip where it is drawn dense', () => {
    const { container } = render(<NoteEvent {...event('12:00 - 13:00 Lunch with the team')} dense />)

    expect(container.firstChild).toHaveAttribute('title', '12:00 - 13:00 Lunch with the team')
  })
})
