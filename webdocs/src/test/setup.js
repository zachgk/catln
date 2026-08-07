import '@testing-library/jest-dom'

// monaco-vim deep-imports monaco-editor, which does not resolve outside a bundler
// and would pull the whole editor into jsdom. Stub it for every suite.
vi.mock('monaco-vim', () => ({
  initVimMode: vi.fn(() => ({ dispose: vi.fn() })),
  VimMode: { Vim: { defineEx: vi.fn() } },
}))

// Default fetch mock - tests override as needed
global.fetch = vi.fn(() =>
  Promise.resolve({
    status: 200,
    json: () => Promise.resolve({ tag: 'Success', contents: [null, []] }),
  })
)

afterEach(() => {
  vi.restoreAllMocks()
})
