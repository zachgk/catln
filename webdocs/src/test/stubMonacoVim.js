// monaco-vim deep-imports monaco-editor, which does not resolve outside a bundler
// and would pull the whole editor into jsdom. Stub it for every suite.
//
// Apart from setup.js because the integration suite needs this and nothing else of it: setup.js
// also mocks fetch, and the integration suite's requests go to a real compiler.
vi.mock('monaco-vim', () => ({
  initVimMode: vi.fn(() => ({ dispose: vi.fn() })),
  VimMode: { Vim: { defineEx: vi.fn() } },
}))
