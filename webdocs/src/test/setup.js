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

// jsdom has no EventSource, which is what a display that follows its page opens. Stubbed rather than
// polyfilled: what the suites need is to answer one, so each stub records itself and hands over the
// frames a test pushes into it. Fresh per test, since a source left open by one suite would be
// answered by the next.
class FakeEventSource {
  constructor(url) {
    this.url = url
    this.readyState = 1
    this.onmessage = null
    this.onerror = null
    openSources.push(this)
    // What the compiler is working on is not a question anything asks once, so there is no answer to
    // take a first frame from and the cases about it push their own. Left alone here as well because
    // a real EventSource does not go through fetch at all: taking the first frame that way is this
    // stub's doing, and a suite counting the requests a display made would count one it never made.
    if (url.includes('/api/progress')) {
      return
    }
    // The first frame is the answer a request asked once would have got, so it is taken from
    // whatever the suite is answering requests with. A suite that mocks one URL therefore answers
    // both the display that asks about it once and the display that follows it, and the cases about
    // everything other than following need to know nothing about any of this.
    Promise.resolve()
      .then(() => global.fetch(url))
      .then(res => {
        if (res.status !== 200) {
          throw new Error(res.statusText)
        }
        return res.json()
      })
      .then(res => this.frame(res), () => this.fail())
  }
  // A frame from the server: what one answer to the question this source asked looks like.
  //
  // A closed source hands over nothing, as a real one does not. That is what makes a display which
  // follows its page immune to answers arriving out of order: closing is what a display does when it
  // moves to another page, so the answer to the page it left cannot land on the page it went to.
  frame(res) { if (this.readyState === 1) this.onmessage?.({data: JSON.stringify(res)}) }
  // The doc server going away. The browser opens the connection again by itself, so this is only
  // ever a report; a later frame is what takes it back.
  fail() { if (this.readyState === 1) this.onerror?.({}) }
  close() { this.readyState = 2 }
}

let openSources = []

// The sources a test opened, and the one it opened for a given URL.
global.eventSources = () => openSources
global.eventSourceFor = url => openSources.filter(s => s.url.includes(url)).at(-1)

beforeEach(() => {
  openSources = []
  global.EventSource = FakeEventSource
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
