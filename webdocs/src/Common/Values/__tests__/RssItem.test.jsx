import React from 'react'
import { render, screen } from '@testing-library/react'
import { ThemeProvider, createTheme } from '@mui/material/styles'
import RssItem from '../RssItem'

const theme = createTheme()

function str(contents) {
  return { tag: 'StrVal', contents }
}

// The args of a compiled /Data/RSS/Item: what the entry is called, where it is, what the feed calls
// it, when it was published, and what it says. What it says is markup, because that is what a feed
// writes -- a comic strip's entry is an img tag and nothing else.
function item(fields = {}) {
  return {
    '/title': str('Accretionary Arc'),
    '/link': str('https://xkcd.com/3285/'),
    '/guid': str('https://xkcd.com/3285/'),
    '/published': str('Fri, 14 Aug 2026 04:00:00 -0000'),
    '/description': str('<img src="https://imgs.xkcd.com/comics/accretionary_arc.png" alt="A comic" />'),
    ...fields,
  }
}

function Providers({ children }) {
  return <ThemeProvider theme={theme}>{children}</ThemeProvider>
}

describe('RssItem', () => {
  it('shows the title as a link to the entry', () => {
    render(<Providers><RssItem {...item()} /></Providers>)

    const link = screen.getByRole('link', { name: 'Accretionary Arc' })
    expect(link).toHaveAttribute('href', 'https://xkcd.com/3285/')
  })

  // An entry may be published with no address of its own. Then there is nothing to link to, and the
  // title is still the title.
  it('shows the title as text where the entry has no address', () => {
    render(<Providers><RssItem {...item({ '/link': str('') })} /></Providers>)

    expect(screen.queryByRole('link')).toBeNull()
    expect(screen.getByText('Accretionary Arc')).toBeInTheDocument()
  })

  it('shows the date the feed wrote', () => {
    render(<Providers><RssItem {...item()} /></Providers>)

    expect(screen.getByText('Fri, 14 Aug 2026 04:00:00 -0000')).toBeInTheDocument()
  })

  // A feed which dates nothing shows no date, rather than an empty label where a date would be.
  it('shows no date where the feed wrote none', () => {
    const { container } = render(<Providers><RssItem {...item({ '/published': str('') })} /></Providers>)

    expect(container.querySelectorAll('.MuiChip-root')).toHaveLength(0)
  })

  it('shows what the entry says as the markup it is', () => {
    const { container } = render(<Providers><RssItem {...item()} /></Providers>)

    const frame = container.querySelector('iframe')
    expect(frame).not.toBeNull()
    expect(frame.getAttribute('srcdoc')).toContain('accretionary_arc.png')
  })

  // An entry is somebody else's writing, so what it says is sanitized before it is shown and the
  // frame it goes into may not run anything. Either alone would do; both is what is here.
  it('drops script an entry carries, and frames what is left', () => {
    const carrying = item({
      '/description': str('<p>Hello</p><script>window.top.location="http://elsewhere"</script>'),
    })
    const { container } = render(<Providers><RssItem {...carrying} /></Providers>)

    const srcdoc = container.querySelector('iframe').getAttribute('srcdoc')
    expect(srcdoc).toContain('<p>Hello</p>')
    expect(srcdoc).not.toContain('<script')
  })

  it('drops an event handler written on a tag', () => {
    const carrying = item({
      '/description': str('<img src="x.png" onerror="window.top.location=1" />'),
    })
    const { container } = render(<Providers><RssItem {...carrying} /></Providers>)

    const srcdoc = container.querySelector('iframe').getAttribute('srcdoc')
    expect(srcdoc).toContain('x.png')
    expect(srcdoc).not.toContain('onerror')
  })

  it('frames the entry sandboxed, and without leave to run anything', () => {
    const { container } = render(<Providers><RssItem {...item()} /></Providers>)

    const sandbox = container.querySelector('iframe').getAttribute('sandbox')
    expect(sandbox).toBe('allow-same-origin')
    expect(sandbox).not.toContain('allow-scripts')
  })

  // An entry which says nothing is its title and its date, rather than an empty frame beneath them.
  it('frames nothing where the entry says nothing', () => {
    const { container } = render(<Providers><RssItem {...item({ '/description': str('') })} /></Providers>)

    expect(container.querySelector('iframe')).toBeNull()
    expect(screen.getByRole('link', { name: 'Accretionary Arc' })).toBeInTheDocument()
  })
})
