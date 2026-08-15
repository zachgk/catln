import React from 'react'
import { render, screen } from '@testing-library/react'
import { ThemeProvider, createTheme } from '@mui/material/styles'
import Properties from '../Properties'

const theme = createTheme()

// The args of a compiled Show/Properties: the block a note opens with, as the text it was written
// as rather than as the properties read out of it.
function properties(text) {
  return { '/text': { tag: 'StrVal', contents: text } }
}

function Providers({ children }) {
  return <ThemeProvider theme={theme}>{children}</ThemeProvider>
}

describe('Properties', () => {
  beforeEach(() => {
    global.fetch = vi.fn(() => Promise.reject(new Error('Properties must not fetch')))
  })

  it('shows a row per property, without fetching', () => {
    render(<Providers><Properties {...properties('url: https://example.com\ntitle: A page')} /></Providers>)

    expect(screen.getByRole('rowheader', { name: 'url' })).toBeInTheDocument()
    expect(screen.getByRole('rowheader', { name: 'title' })).toBeInTheDocument()
    expect(screen.getByText('A page')).toBeInTheDocument()
    expect(global.fetch).not.toHaveBeenCalled()
  })

  it('shows a property naming a page as the link it is', () => {
    render(<Providers><Properties {...properties('url: https://example.com')} /></Providers>)

    const link = screen.getByRole('link', { name: 'https://example.com' })
    expect(link).toHaveAttribute('href', 'https://example.com')
    expect(link).toHaveAttribute('target', '_blank')
  })

  // A property written across several lines is one property, so its entries belong to the row its
  // key opened rather than to rows of their own with no name.
  it('writes the entries of a list on the row of their key', () => {
    render(<Providers><Properties {...properties('tags:\n  - reading\n  - web\nurl: https://example.com')} /></Providers>)

    expect(screen.getAllByRole('row')).toHaveLength(2)
    // Matched without the normalizer that would collapse the newline the entries are kept apart by.
    expect(screen.getByText('- reading\n- web', { normalizer: s => s })).toBeInTheDocument()
  })

  it('shows nothing at all when nothing in the block reads as a property', () => {
    const { container } = render(<Providers><Properties {...properties('# just a comment')} /></Providers>)

    expect(container).toBeEmptyDOMElement()
  })

  it('shows nothing at all for an empty block', () => {
    const { container } = render(<Providers><Properties {...properties('')} /></Providers>)

    expect(container).toBeEmptyDOMElement()
  })
})
