/**
 * Integration tests that run against the real Catln Haskell backend.
 *
 * Prerequisites:
 *   Start the backend API servers before running these tests:
 *     stack exec catln doc test/Integration/code/id.ct -- --api --cached
 *     stack exec catln doc test/Integration/code/id.ct -- --cached --readonly
 *   The second serves the same API on the serve port (8080) and is only there to
 *   check that --readonly refuses writes.
 *   Or from stack repl:
 *     :l test/Spec
 *     testd
 *
 * Run with:
 *   npm run test:integration
 */

const API_BASE = 'http://localhost:31204'
const READONLY_API_BASE = 'http://localhost:8080'

async function fetchApi(path, base = API_BASE) {
  const res = await fetch(`${base}${path}`)
  expect(res.status).toBe(200)
  return res.json()
}

// Returns the raw response: unlike the read endpoints, callers here assert on status.
function writeApi(prgmName, source, base = API_BASE) {
  return fetch(`${base}/api/write?prgmName=${encodeURIComponent(prgmName)}`, {
    method: 'POST',
    headers: {'Content-Type': 'text/plain'},
    body: source,
  })
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

  it('/api/page returns Success with prgm data and source text', async () => {
    // First get the TOC to find a valid page name
    const tocData = await fetchApi('/api/toc')
    const toc = tocData.contents[0]
    expect(Array.isArray(toc)).toBe(true)
    expect(toc.length).toBeGreaterThan(0)

    const pageName = encodeURIComponent(toc[0][0])
    const data = await fetchApi(`/api/page?prgmName=${pageName}`)
    expect(data.tag).toBe('Success')
    expect(data.contents).toHaveLength(2)

    const [pageContent] = data.contents
    // pageContent is [prgmData, maybeSource]
    expect(Array.isArray(pageContent)).toBe(true)
    expect(pageContent).toHaveLength(2)

    // prgmData is [imports, statements]
    const [prgmData, maybeSource] = pageContent
    expect(Array.isArray(prgmData)).toBe(true)
    expect(prgmData).toHaveLength(2)

    // maybeSource is a string or null
    expect(maybeSource === null || typeof maybeSource === 'string').toBe(true)
  })

  it('/api/treebuild returns Success', async () => {
    const data = await fetchApi('/api/treebuild')
    expect(data.tag).toBe('Success')
    expect(data.contents).toHaveLength(2)
  })

  it('/api/config reports a writable server', async () => {
    const data = await fetchApi('/api/config')
    expect(data.tag).toBe('Success')
    expect(data.contents[0]).toEqual({readonly: false})
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

  // Write tests come last: a successful write calls ctssRead, which invalidates every
  // page (Meta's derived Eq includes a per-parse random UUID, so the unchanged-file
  // check in buildSSF never matches), making the next page load pay a full rebuild.
  describe('writes', () => {
    async function idPageName(base = API_BASE) {
      const toc = (await fetchApi('/api/toc', base)).contents[0]
      const name = toc.map(t => t[0]).find(n => n.endsWith('/test/Integration/code/id.ct'))
      expect(name).toBeDefined()
      return name
    }

    it('refuses an existing file that is not a served page', async () => {
      // ho.ct is a real file, but not reachable from the id.ct doc root
      const outside = (await idPageName()).replace('/id.ct', '/ho.ct')
      const res = await writeApi(outside, 'CORRUPTED')
      expect(res.status).toBe(200)
      expect((await res.json()).tag).toBe('ResFail')
    })

    it('refuses a path that does not exist', async () => {
      const res = await writeApi('/does/not/exist.ct', 'main = 1\n')
      expect(res.status).not.toBe(200)
    })

    it('--readonly reports readonly config and refuses writes', async () => {
      const config = await fetchApi('/api/config', READONLY_API_BASE)
      expect(config.tag).toBe('Success')
      expect(config.contents[0]).toEqual({readonly: true})

      const pageName = await idPageName(READONLY_API_BASE)
      const res = await writeApi(pageName, 'main = 999\n', READONLY_API_BASE)
      expect(res.status).toBe(200)
      const body = await res.json()
      expect(body.tag).toBe('ResFail')
      expect(JSON.stringify(body.contents)).toContain('readonly')

      // and the file is untouched
      const after = (await fetchApi(`/api/page?prgmName=${encodeURIComponent(pageName)}`, READONLY_API_BASE))
        .contents[0][1]
      expect(after).not.toContain('main = 999')
    })

    // Generous timeout: the trailing read rebuilds every invalidated page.
    it('round-trips a page source', async () => {
      const pageName = await idPageName()

      const original = (await fetchApi(`/api/page?prgmName=${encodeURIComponent(pageName)}`))
        .contents[0][1]
      expect(typeof original).toBe('string')

      // Write the identical bytes back: exercises the whole write path without
      // actually modifying the checked-in file.
      const written = await writeApi(pageName, original)
      expect(written.status).toBe(200)
      expect((await written.json()).tag).toBe('Success')

      const after = (await fetchApi(`/api/page?prgmName=${encodeURIComponent(pageName)}`))
        .contents[0][1]
      expect(after).toBe(original)
    }, 180000)
  })
})
