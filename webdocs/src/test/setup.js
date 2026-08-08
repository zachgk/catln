import '@testing-library/jest-dom'

// monaco-vim deep-imports monaco-editor, which does not resolve outside a bundler
// and would pull the whole editor into jsdom. Stub it for every suite.
vi.mock('monaco-vim', () => ({
  initVimMode: vi.fn(() => ({ dispose: vi.fn() })),
  VimMode: { Vim: { defineEx: vi.fn() } },
}))

// jsdom under this node leaves window.localStorage as a bare object with none of the Storage
// methods on it, so anything remembering a preference throws rather than reading back what it
// stored. Give every suite a working one, fresh per test so one test's writes do not reach another.
beforeEach(() => {
  let stored = new Map()
  Object.defineProperty(window, 'localStorage', {
    configurable: true,
    value: {
      getItem: key => (stored.has(key) ? stored.get(key) : null),
      setItem: (key, value) => stored.set(key, String(value)),
      removeItem: key => stored.delete(key),
      clear: () => stored.clear(),
      key: i => Array.from(stored.keys())[i] ?? null,
      get length() { return stored.size },
    },
  })
})

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
