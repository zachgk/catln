import '@testing-library/jest-dom'

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
