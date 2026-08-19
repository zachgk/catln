import React from 'react'
import { render, screen } from '@testing-library/react'
import { ThemeProvider, createTheme } from '@mui/material/styles'
import HTMLShow from '../HTMLShow'

const theme = createTheme()

// The args of a compiled /Catln/Doc/Show/Html: the markup an arm of /Catln/Doc/dshow built, and
// nothing else. What type it was built for is the arm's business and not this component's.
function html(contents) {
  return { '/contents': { tag: 'StrVal', contents } }
}

function Providers({ children }) {
  return <ThemeProvider theme={theme}>{children}</ThemeProvider>
}

describe('HTMLShow', () => {
  it('lays the markup in as the elements it is', () => {
    render(<Providers><HTMLShow {...html('<h1>Tasks</h1>')} /></Providers>)

    expect(screen.getByRole('heading', { level: 1, name: 'Tasks' })).toBeInTheDocument()
  })

  // Laid into the page rather than framed in one, which is the whole of the difference between this
  // and Html: a program's own display is meant to take this page's styling, where a frame would give
  // it none of it. This is the case that fails if anyone reroutes this through Html.
  it('lays the markup into the page rather than framing it', () => {
    const { container } = render(<Providers><HTMLShow {...html('<h1>Tasks</h1>')} /></Providers>)

    expect(container.querySelector('iframe')).toBeNull()
  })

  it('drops a script the markup carries', () => {
    const { container } = render(
      <Providers><HTMLShow {...html('<h1>Tasks</h1><script>window.stolen = 1</script>')} /></Providers>
    )

    expect(screen.getByRole('heading', { level: 1, name: 'Tasks' })).toBeInTheDocument()
    expect(container.querySelector('script')).toBeNull()
    expect(container.innerHTML).not.toContain('<script')
  })

  it('drops a handler written on a tag, keeping the tag', () => {
    const { container } = render(
      <Providers><HTMLShow {...html('<img src="x.png" onerror="window.stolen = 1" />')} /></Providers>
    )

    const img = container.querySelector('img')
    expect(img).not.toBeNull()
    expect(img.getAttribute('src')).toBe('x.png')
    expect(img.getAttribute('onerror')).toBeNull()
  })

  it('drops a link that runs rather than goes somewhere', () => {
    const { container } = render(
      <Providers><HTMLShow {...html('<a href="javascript:window.stolen = 1">go</a>')} /></Providers>
    )

    expect(container.querySelector('a').getAttribute('href')).toBeNull()
  })

  // The accepted behaviour, asserted on purpose: an arm joining a string into markup is an arm whose
  // string is read as markup. A note titled with a tag is a heading holding that tag rather than a
  // heading of that name, which is the arm's to get right and not this component's to undo.
  it('reads what the arm joined in as the markup it is', () => {
    render(<Providers><HTMLShow {...html('<h1><b>Tasks</b></h1>')} /></Providers>)

    const heading = screen.getByRole('heading', { level: 1 })
    expect(heading.querySelector('b')).not.toBeNull()
    expect(heading).toHaveTextContent('Tasks')
  })

  // An arm which built nothing shows nothing, rather than an empty block opening a gap where the
  // program wrote none.
  it('shows nothing where the arm built nothing', () => {
    const { container } = render(<Providers><HTMLShow {...html('')} /></Providers>)

    expect(container.firstChild).toBeNull()
  })
})
