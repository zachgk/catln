/**
 * Integration tests that run against the real Catln Haskell backend.
 *
 * Prerequisites:
 *   Start the backend API server before running these tests:
 *     stack exec catln doc test/integration/id.ct -- --api
 *   Or from stack repl:
 *     :l test/Spec
 *     testd
 *
 * Run with:
 *   npm run test:integration
 */

const API_BASE = 'http://localhost:31204'

async function fetchApi(path) {
  const res = await fetch(`${API_BASE}${path}`)
  expect(res.status).toBe(200)
  return res.json()
}

describe('Backend API contract', () => {
  it('/api/toc returns Success with page entries', async () => {
    const data = await fetchApi('/api/toc')
    expect(data.tag).toBe('Success')
    expect(data.contents).toHaveLength(2)

    const [toc, notes] = data.contents
    expect(Array.isArray(toc)).toBe(true)
    expect(Array.isArray(notes)).toBe(true)

    // TOC should have at least one page entry (each is a [path, importData] tuple)
    expect(toc.length).toBeGreaterThan(0)
    expect(toc[0]).toHaveLength(2)
    expect(typeof toc[0][0]).toBe('string')
  })

  it('/api/desugar returns Success with program data', async () => {
    const data = await fetchApi('/api/desugar')
    expect(data.tag).toBe('Success')
    expect(data.contents).toHaveLength(2)

    const [prgm] = data.contents
    expect(prgm).toHaveProperty('prgmObjMap')
    expect(typeof prgm.prgmObjMap).toBe('object')
  })

  it('/api/typecheck returns Success with typed program', async () => {
    const data = await fetchApi('/api/typecheck')
    expect(data.tag).toBe('Success')
    expect(data.contents).toHaveLength(2)

    const [prgm] = data.contents
    expect(prgm).toHaveProperty('prgmObjMap')
    expect(prgm).toHaveProperty('prgmCG')
  })

  it('/api/page returns Success for a known page', async () => {
    // First get the TOC to find a valid page name
    // TOC is an array of [pagePath, importData] tuples
    const tocData = await fetchApi('/api/toc')
    const toc = tocData.contents[0]
    expect(Array.isArray(toc)).toBe(true)
    expect(toc.length).toBeGreaterThan(0)

    const pageName = encodeURIComponent(toc[0][0])
    const data = await fetchApi(`/api/page?prgmName=${pageName}`)
    expect(data.tag).toBe('Success')
    expect(data.contents).toHaveLength(2)

    const [pageContent] = data.contents
    // Page content is [imports, statements]
    expect(Array.isArray(pageContent)).toBe(true)
    expect(pageContent).toHaveLength(2)
  })

  it('/api/treebuild returns Success', async () => {
    const data = await fetchApi('/api/treebuild')
    expect(data.tag).toBe('Success')
    expect(data.contents).toHaveLength(2)
  })

  it('CRes response structure is consistent', async () => {
    // Verify all endpoints follow the same {tag, contents} pattern
    const endpoints = ['/api/toc', '/api/desugar', '/api/typecheck']

    for (const endpoint of endpoints) {
      const data = await fetchApi(endpoint)
      expect(data).toHaveProperty('tag')
      expect(data).toHaveProperty('contents')
      expect(['Success', 'ResFail']).toContain(data.tag)
    }
  })
})
