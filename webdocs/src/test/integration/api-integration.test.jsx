/**
 * Integration tests that run against the real Catln Haskell backend.
 *
 * Prerequisites:
 *   Start the backend API servers before running these tests:
 *     stack exec catln doc test/Integration/code/id.ct test/Integration/code/obsidian.ct -- --api
 *     stack exec catln doc test/Integration/code/id.ct -- --readonly
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
  // The pages of the doc server are listed by an annotation rather than a route of their own, and
  // unscoped, which is the only way to be asking about every page rather than about one.
  it('/api/annot lists every page for an unscoped #sidebar', async () => {
    const pages = await sidebarPages()

    expect(pages.length).toBeGreaterThan(0)
    for (const page of pages) {
      expect(page.tag).toBe('SidebarFile')
      expect(typeof page.sbPath).toBe('string')
      // Every file is named by the path it is asked for, under the directory it sits in.
      expect(page.sbPath.endsWith(`/${page.sbName}`)).toBe(true)
    }
    // The doc root and the stdlib it imports are both loaded pages, so both are listed.
    expect(pages.map(p => p.sbPath).some(n => n.endsWith('/test/Integration/code/id.ct'))).toBe(true)
    expect(pages.map(p => p.sbPath).some(n => n.endsWith('/stack/core/main.ct'))).toBe(true)
  })

  // What the sidebar lists does not depend on the page it is compiled in. That is what lets the
  // webdocs name a page and so keep a navigation from costing a compilation of everything it names:
  // named with one page it still lists them all, including the ones that page does not import.
  it('#sidebar lists every page when it is compiled within one', async () => {
    const unscoped = (await sidebarPages()).map(p => p.sbPath)
    const scoped = (await sidebarPages(API_BASE, await idPageName())).map(p => p.sbPath)

    expect(scoped).toEqual(unscoped)
    // id.ct imports nothing of the Obsidian vault the server was also given, so a listing of only
    // what it reaches would have none of the vault's notes in it.
    expect(scoped.some(p => p.includes('/artifacts/catln-test/'))).toBe(true)
  })

  it('#sidebar leads a directory with the page it is entered through', async () => {
    const val = await annotValue('#sidebar')
    const tree = JSON.parse(val.args['/pages'].contents)

    // A directory of pages is itself a page (importing it imports its main.ct), so it appears as
    // the directory node holding those pages rather than as a file beside them.
    const core = (function find(nodes) {
      for (const node of nodes) {
        if (node.tag !== 'SidebarDir') continue
        if (node.sbPath.endsWith('/stack/core')) return node
        const found = find(node.sbChildren)
        if (found) return found
      }
    })(tree)
    expect(core).toBeDefined()
    expect(core.sbChildren[0].sbName).toBe('main.ct')
  })

  // The search box gets its pages from an annotation of its own rather than from the sidebar's
  // tree. They are two answers to the same question, so they are checked against each other here:
  // the duplication is deliberate for the moment, and a divergence should be a reported fact.
  it('/api/annot lists every page for an unscoped #search', async () => {
    const val = await annotValue('#search')
    expect(val.name).toBe('/Catln/Doc/Show/Search')

    const pages = JSON.parse(val.args['/pages'].contents)
    expect(pages.length).toBeGreaterThan(0)
    expect(pages.every(p => typeof p === 'string')).toBe(true)
    // Sorted, which is the order a box offers them in.
    expect(pages).toEqual([...pages].sort())
    // The doc root and the stdlib it imports are both loaded pages, so both can be searched for.
    expect(pages.some(p => p.endsWith('/test/Integration/code/id.ct'))).toBe(true)
    expect(pages.some(p => p.endsWith('/stack/core/main.ct'))).toBe(true)

    // Every page the sidebar shows as a file is one the box can open. Containment rather than
    // equality: a directory of pages is a page in its own right, so it is in the list while the
    // sidebar shows it as the directory node holding those pages.
    expect(pages).toEqual(expect.arrayContaining((await sidebarPages()).map(p => p.sbPath)))
  })

  // The listProgram eval primitive is only reachable through the doc pipeline, so these are the
  // only automated checks that a mode actually selects a different compiler stage.
  async function listProgram(modeName) {
    const annot = `#listProgram(mode= ${modeName})`
    const data = await fetchApi(`/api/annot?annot=${encodeURIComponent(annot)}`)
    expect(data.tag).toBe('Success')

    // The response is a one-statement raw program; the compiled value rides on the annotation
    // expression's meta as getMetaDat[2].
    const [, statements] = data.contents[0]
    const [statement] = statements[0]
    expect(statement.tag).toBe('RawAnnot')

    const val = statement.contents.contents[0].getMetaDat[2]
    expect(val.tag).toBe('TupleVal')
    expect(val.name).toBe('/Catln/Doc/Show/ListProgram')
    // The mode travels with the value, which is what the page's stage selector reads.
    expect(val.args['/mode'].name).toBe(modeName)

    expect(val.args['/prgm'].tag).toBe('StrVal')
    const prgm = JSON.parse(val.args['/prgm'].contents)
    expect(prgm).toHaveProperty('prgmObjMap')
    expect(prgm).toHaveProperty('prgmCG')
    return prgm
  }

  // Every {tag: ...} appearing anywhere under the object map.
  function tagsIn(prgm) {
    const tags = new Set()
    const walk = (o) => {
      if (Array.isArray(o)) return o.forEach(walk)
      if (o && typeof o === 'object') {
        if (typeof o.tag === 'string') tags.add(o.tag)
        Object.values(o).forEach(walk)
      }
    }
    walk(prgm.prgmObjMap)
    return tags
  }

  // Whether any meta carries stage metadata. ParseMetaDat is (), which encodes as [].
  function hasMetaDat(prgm) {
    let found = false
    const walk = (o) => {
      if (found) return
      if (Array.isArray(o)) return o.forEach(walk)
      if (o && typeof o === 'object') {
        if ('getMetaID' in o && o.getMetaDat !== null && !(Array.isArray(o.getMetaDat) && o.getMetaDat.length === 0)) {
          found = true
          return
        }
        Object.values(o).forEach(walk)
      }
    }
    walk(prgm.prgmObjMap)
    return found
  }

  it('/api/annot lists the typecheck stage for the Typecheck mode', async () => {
    const prgm = await listProgram('/Catln/Doc/Typecheck')

    // Typechecked expressions are still Exprs, but their metas now carry a reaches tree.
    expect(tagsIn(prgm).has('TupleApply')).toBe(true)
    expect(tagsIn(prgm).has('TCalls')).toBe(false)
    expect(hasMetaDat(prgm)).toBe(true)
  })

  it('/api/annot lists the desugar stage for the Desugar mode', async () => {
    const prgm = await listProgram('/Catln/Doc/Desugar')

    // Same expression shape as the typecheck stage, but nothing has been inferred onto the metas.
    expect(tagsIn(prgm).has('TupleApply')).toBe(true)
    expect(tagsIn(prgm).has('TCalls')).toBe(false)
    expect(hasMetaDat(prgm)).toBe(false)
  })

  // TreeBuild is the expensive mode and needs more than the 15s default. It is the only request
  // that forces evalBuildAll, and it carries the built stdlib as an escaped JSON string. Measured
  // on a quiet dev machine against the id.ct doc root: 37MB, 9.0s on the first call and 6.1s once
  // the build is cached, against 2.6s for Typecheck. This budget is that plus headroom.
  const TREEBUILD_TIMEOUT = 60000

  it('/api/annot lists the tree build stage for the TreeBuild mode', async () => {
    const prgm = await listProgram('/Catln/Doc/TreeBuild')

    // Tree building replaces Expr with TExpr and attaches the call trees.
    const tags = tagsIn(prgm)
    expect(tags.has('TTupleApply')).toBe(true)
    expect(tags.has('TCalls')).toBe(true)
  }, TREEBUILD_TIMEOUT)

  // #type is #listProgram narrowed to one name. Like listProgram its primitive is only reachable
  // through the doc pipeline, so this is the only automated check that the filtering happens in the
  // compiler rather than in the page.
  it('/api/annot filters the program to one name for #type', async () => {
    const name = '/Catln/Doc/DShow'
    const annot = `#type(name= "${name}")`
    const data = await fetchApi(`/api/annot?annot=${encodeURIComponent(annot)}`)
    expect(data.tag).toBe('Success')

    const [, statements] = data.contents[0]
    const [statement] = statements[0]
    expect(statement.tag).toBe('RawAnnot')

    const val = statement.contents.contents[0].getMetaDat[2]
    expect(val.tag).toBe('TupleVal')
    expect(val.name).toBe('/Catln/Doc/Show/TypePage')
    // The name travels with the value, which is what the page titles itself from.
    expect(val.args['/name'].contents).toBe(name)

    expect(val.args['/prgm'].tag).toBe('StrVal')
    const filtered = JSON.parse(val.args['/prgm'].contents)
    expect(filtered).toHaveProperty('prgmObjMap')
    expect(filtered).toHaveProperty('prgmCG')

    // The point of the filter: strictly less than the whole program the same page would list.
    const whole = await listProgram('/Catln/Doc/Typecheck')
    const count = (p) => JSON.stringify(p.prgmObjMap).length
    expect(count(filtered)).toBeGreaterThan(0)
    expect(count(filtered)).toBeLessThan(count(whole))
  })

  // #typeInfer takes no arguments: like every annotation it reports on the program it is written
  // in. What names a page is the API call, so these also cover /api/annot's prgmName scoping.
  //
  // An annotation that cannot be evaluated does not fail the request. It is reported as an error
  // note and left out of the response, so a page whose other annotations worked is still shown
  // them. What came back is therefore a value that may be absent beside the notes saying why.
  async function annotResult(annot, prgmName, base = API_BASE) {
    const scope = prgmName ? `&prgmName=${encodeURIComponent(prgmName)}` : ''
    const data = await fetchApi(`/api/annot?annot=${encodeURIComponent(annot)}${scope}`, base)
    if (data.tag !== 'Success') return {data, value: null, notes: data.contents}

    const [prgm, notes] = data.contents
    const [, statements] = prgm
    const [statement] = statements[0]
    expect(statement.tag).toBe('RawAnnot')
    return {data, value: statement.contents.contents[0].getMetaDat[2], notes}
  }

  async function annotValue(annot, prgmName, base = API_BASE) {
    const {data, value} = await annotResult(annot, prgmName, base)
    return data.tag === 'Success' ? value : data
  }

  // The pages the sidebar lists, flattened. #sidebar is the only listing of them, so this is also
  // how the tests that need a page name find one. Takes a page to be compiled in, since what it
  // lists is the same either way.
  async function sidebarPages(base = API_BASE, prgmName = undefined) {
    const val = await annotValue('#sidebar', prgmName, base)
    expect(val.name).toBe('/Catln/Doc/Show/Sidebar')
    const flatten = (nodes) => nodes.flatMap(n => n.tag === 'SidebarDir' ? flatten(n.sbChildren) : [n])
    return flatten(JSON.parse(val.args['/pages'].contents))
  }

  async function idPageName(base = API_BASE) {
    const pages = await sidebarPages(base)
    const name = pages.map(p => p.sbPath).find(n => n.endsWith('/test/Integration/code/id.ct'))
    expect(name).toBeDefined()
    return name
  }

  it('/api/annot scopes #typeInfer to the page it names', async () => {
    const page = await idPageName()
    const val = await annotValue('#typeInfer', page)
    expect(val.tag).toBe('TupleVal')
    expect(val.name).toBe('/Catln/Doc/Show/TypeInfer')

    const [prgm, notes] = JSON.parse(val.args['/constrain'].contents)
    const [objMap] = prgm
    expect(Array.isArray(notes)).toBe(true)

    // The scoping is the point: this is id.ct's own inference, not every loaded page's joined.
    // id.ct defines id, so that is present while the stdlib it imports is not.
    expect(Object.keys(objMap)).toContain('/id')
    expect(Object.keys(objMap).length).toBeLessThan(20)
  })

  it('/api/annot reports #typeInfer with no page to be scoped to as an error note', async () => {
    // Points are numbered per typecheck run, so there is no joined inference to fall back to, and a
    // silent substitution would be the wrong answer. Saying so is not the same as refusing the
    // request: the annotation is left out and the reason returned beside what did arrive.
    const {data, value, notes} = await annotResult('#typeInfer')
    expect(data.tag).toBe('Success')
    expect(value).toBeNull()
    expect(notes.some(n => n.tp === 'CNoteError')).toBe(true)
    expect(JSON.stringify(notes)).toContain('not scoped to a single page')
  })

  it('/api/annot narrows #typeInferPnt to one point of that page', async () => {
    const page = await idPageName()
    const whole = await annotValue('#typeInfer', page)
    const [prgm] = JSON.parse(whole.args['/constrain'].contents)

    // Every meta carries its point as getMetaDat[0]; take one the page actually inferred.
    const pnts = new Set()
    const walk = (o) => {
      if (Array.isArray(o)) return o.forEach(walk)
      if (o && typeof o === 'object') {
        if (Array.isArray(o.getMetaDat) && typeof o.getMetaDat[0] === 'number') pnts.add(o.getMetaDat[0])
        Object.values(o).forEach(walk)
      }
    }
    walk(prgm[0])
    expect(pnts.size).toBeGreaterThan(0)
    const pnt = [...pnts].sort((a, b) => a - b)[0]

    const val = await annotValue(`#typeInferPnt(pnt= ${pnt})`, page)
    expect(val.name).toBe('/Catln/Doc/Show/TypeInferPnt')
    // The point travels with the value, which is what the point page titles itself from.
    expect(val.args['/pnt'].contents).toBe(pnt)

    const trace = JSON.parse(val.args['/trace'].contents)
    expect(trace).toHaveProperty('tcEpochs')
    expect(trace).toHaveProperty('tcCons')
    // Filtered to this point: its initial scheme is the only one kept.
    expect(Object.keys(trace.tcInitial)).toEqual([String(pnt)])
  })

  it('/api/annot runs a function of the page #debugger names', async () => {
    const page = await idPageName()
    const val = await annotValue('#debugger(fun= "/main")', page)
    expect(val.name).toBe('/Catln/Doc/Show/Debugger')
    // The function travels with the value, which is what the page titles itself from.
    expect(val.args['/fun'].contents).toBe('/main')

    // id.ct is `main{io -> IO}= io.exit(val= id(x= 0))`, so it really is run: it exits 0 and its
    // evaluation tree has the arrows it entered. Printing nothing, it captures no output -- which is
    // the same buffer a printing program's output would land in instead of this server's stdout.
    const run = JSON.parse(val.args['/run'].contents)
    expect(run.drExit).toBe(0)
    expect(run.drOutput).toBe('')
    expect(run.drResult.erTreebug.length).toBeGreaterThan(0)
  })

  it('/api/annot reports #debugger with no page to be scoped to as an error note', async () => {
    // A run needs one program with one entry point; every loaded page joined has as many mains as
    // it has pages, so there is nothing to fall back to.
    const {data, value, notes} = await annotResult('#debugger(fun= "/main")')
    expect(data.tag).toBe('Success')
    expect(value).toBeNull()
    expect(notes.some(n => n.tp === 'CNoteError')).toBe(true)
    expect(JSON.stringify(notes)).toContain('not scoped to a single page')
  })

  // The tasks of every note the doc server holds, gathered into one list. Unscoped, which is the
  // only way to be asking about every note rather than about one, and why the server is started with
  // the Obsidian vault beside id.ct.
  describe('#taskList', () => {
    // The note the vault fixture writes its tasks in.
    const NOTE = '/test/Integration/code/artifacts/catln-test/Tasks.md'
    const FIRST = 'Read [[Welcome]]'

    async function tasks() {
      const val = await annotValue('#taskList')
      expect(val.tag).toBe('TupleVal')
      expect(val.name).toBe('/Data/Note/Show/TaskList')
      return JSON.parse(val.args['/tasks'].contents)
    }

    function contentsOf(task) {
      return task.ptTask.args['/contents'].contents
    }

    function isDone(task) {
      return task.ptTask.args['/state'].name === '/Data/Primitive/True'
    }

    function taskSource(contents, done) {
      return `/Data/Note/Task(contents= ${JSON.stringify(contents)}, state= /Data/Primitive/${done ? 'True' : 'False'})`
    }

    // Written by the page and expression the task carries, which for a list gathered across notes is
    // the only thing naming where the edit goes.
    function writeTask(task, source) {
      return fetch(`${API_BASE}/api/write-expr?prgmName=${encodeURIComponent(task.ptPage)}&metaId=${encodeURIComponent(task.ptExpr)}`, {
        method: 'POST',
        headers: {'Content-Type': 'text/plain'},
        body: source,
      })
    }

    it('lists the tasks of every note as the values they display as', async () => {
      const found = await tasks()
      expect(found.length).toBeGreaterThan(0)

      for (const task of found) {
        // Each entry is the task's own value, so a task in the list is shown by whatever shows a
        // task rather than by the list re-rendering its fields.
        expect(task.ptTask.tag).toBe('TupleVal')
        expect(task.ptTask.name).toBe('/Data/Note/Task')
        expect(typeof contentsOf(task)).toBe('string')
        expect(['/Data/Primitive/True', '/Data/Primitive/False']).toContain(task.ptTask.args['/state'].name)
        // Beside where it was written, which is what editing it in a list across notes needs.
        expect(task.ptPage.endsWith('.md')).toBe(true)
        expect(task.ptExpr).toMatch(/^[0-9a-f-]{36}$/)
      }

      // The vault's tasks in the order the note writes them, nested ones included and the one inside
      // a code fence left out: a checkbox in a fence is writing about a task rather than making one.
      const fromNote = found.filter(t => t.ptPage.endsWith(NOTE))
      expect(fromNote.map(contentsOf)).toEqual([
        FIRST,
        'Write this note',
        'Plan the rest',
        'A checkbox indented deeper is a task of its own',
      ])
      expect(fromNote.map(isDone)).toEqual([false, true, false, false])
    })

    // The whole point of gathering them: a task is written back to the note it was read from, which
    // is not the page whatever displays the list is on.
    it('writes a task of the list back to the note it was read from', async () => {
      const before = (await tasks()).find(t => contentsOf(t) === FIRST)
      expect(before).toBeDefined()
      expect(isDone(before)).toBe(false)

      try {
        const res = await writeTask(before, taskSource(FIRST, true))
        expect(res.status).toBe(200)
        expect((await res.json()).tag).toBe('Success')

        // The note says so in markdown, which is the reversion the parser that read it wrote.
        const page = await fetchApi(`/api/page?prgmName=${encodeURIComponent(before.ptPage)}`)
        expect(page.contents[0].pgSource).toContain(`- [x] ${FIRST}`)

        // And so does the list, which reads the notes again rather than remembering the write.
        expect(isDone((await tasks()).find(t => contentsOf(t) === FIRST))).toBe(true)
      } finally {
        const now = (await tasks()).find(t => contentsOf(t) === FIRST)
        if (now) await writeTask(now, taskSource(FIRST, false))
      }
    })
  })

  it('/api/page returns Success with prgm data and source text', async () => {
    const pageName = encodeURIComponent(await idPageName())
    const data = await fetchApi(`/api/page?prgmName=${pageName}`)
    expect(data.tag).toBe('Success')
    expect(data.contents).toHaveLength(2)

    const [page] = data.contents
    // pgPrgm is [imports, statements]
    expect(Array.isArray(page.pgPrgm)).toBe(true)
    expect(page.pgPrgm).toHaveLength(2)

    // pgSource is a string or null
    expect(page.pgSource === null || typeof page.pgSource === 'string').toBe(true)

    // The directory the page was imported relative to, which its relative doc links resolve against
    expect(await idPageName()).toBe(`${page.pgDir}/id.ct`)
  })

  // The pages either side of one are the server's answer, and the sidebar is the server's other
  // answer about the same order: a page's neighbours are the pages around it in the tree.
  //
  // Only the page the server opens at is warmed, and each page evaluates its own annotations on
  // first load, so the three pages read here pay that cost rather than the default 15s budget.
  it('/api/page names the pages either side of one, and the ends have none', async () => {
    const order = (await sidebarPages()).map(p => p.sbPath)
    expect(order.length).toBeGreaterThan(2)

    const pageAt = async (name) =>
      (await fetchApi(`/api/page?prgmName=${encodeURIComponent(name)}`)).contents[0]

    const middle = await pageAt(order[1])
    expect(middle.pgPrev).toBe(order[0])
    expect(middle.pgNext).toBe(order[2])

    const first = await pageAt(order[0])
    expect(first.pgPrev).toBeNull()
    expect(first.pgNext).toBe(order[1])

    const last = await pageAt(order[order.length - 1])
    expect(last.pgPrev).toBe(order[order.length - 2])
    expect(last.pgNext).toBeNull()
  }, 60000)

  it('/api/config reports a writable server and the page it opens at', async () => {
    const data = await fetchApi('/api/config')
    expect(data.tag).toBe('Success')
    // The doc server was started with id.ct, so that is the page /docs lands on.
    expect(data.contents[0]).toEqual({readonly: false, rootPage: await idPageName()})
  })

  it('CRes response structure is consistent', async () => {
    // Verify all endpoints follow the same {tag, contents} pattern
    const endpoints = ['/api/config', '/api/annot?annot=%23sidebar']

    for (const endpoint of endpoints) {
      const data = await fetchApi(endpoint)
      expect(data).toHaveProperty('tag')
      expect(data).toHaveProperty('contents')
      expect(['Success', 'ResFail']).toContain(data.tag)
    }
  })

  describe('writes', () => {
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
      expect(config.contents[0].readonly).toBe(true)

      const pageName = await idPageName(READONLY_API_BASE)
      const res = await writeApi(pageName, 'main = 999\n', READONLY_API_BASE)
      expect(res.status).toBe(200)
      const body = await res.json()
      expect(body.tag).toBe('ResFail')
      expect(JSON.stringify(body.contents)).toContain('readonly')

      // and the file is untouched
      const after = (await fetchApi(`/api/page?prgmName=${encodeURIComponent(pageName)}`, READONLY_API_BASE))
        .contents[0].pgSource
      expect(after).not.toContain('main = 999')
    })

    // Replacing one expression of a page and saving the page back as the file it was read from.
    // What the reversion writes is the parser's business, so what the endpoint answers for is which
    // page, which expression, and whether that syntax can be written back at all.
    describe('/api/write-expr', () => {
      // The id of the first expression the page declares, which is one this page really holds. The
      // page's own comment is a statement of it too, so the declarations are picked out rather than
      // the first statement taken.
      async function anExprId(base = API_BASE) {
        const pageName = await idPageName(base)
        const page = (await fetchApi(`/api/page?prgmName=${encodeURIComponent(pageName)}`, base)).contents[0]
        const [, statements] = page.pgPrgm
        const [statement] = statements
          .map(([statement]) => statement)
          .filter(statement => statement.tag === 'RawDeclStatement')
        expect(statement).toBeDefined()
        return [pageName, statement.contents.roaObj.contents[0].getMetaID]
      }

      function writeExpr(prgmName, metaId, source, base = API_BASE) {
        return fetch(`${base}/api/write-expr?prgmName=${encodeURIComponent(prgmName)}&metaId=${encodeURIComponent(metaId)}`, {
          method: 'POST',
          headers: {'Content-Type': 'text/plain'},
          body: source,
        })
      }

      // A .ct page has no reversion yet, so saving one is refused rather than half-done. This is
      // what a syntax without one answers, and it is the answer every syntax but a note gives.
      it('refuses a page whose syntax cannot be written back', async () => {
        const [pageName, metaId] = await anExprId()

        const res = await writeExpr(pageName, metaId, 'main')
        expect(res.status).toBe(200)
        const body = await res.json()
        expect(body.tag).toBe('ResFail')
        expect(JSON.stringify(body.contents)).toContain('can not be written back')

        // The page is left as it was, since the reversion failed before anything was written.
        const after = (await fetchApi(`/api/page?prgmName=${encodeURIComponent(pageName)}`)).contents[0].pgSource
        expect(after).toContain('id')
      })

      it('refuses an expression the page does not hold', async () => {
        const pageName = await idPageName()

        const res = await writeExpr(pageName, '00000000-0000-0000-0000-000000000001', 'main')
        expect(res.status).toBe(200)
        const body = await res.json()
        expect(body.tag).toBe('ResFail')
        expect(JSON.stringify(body.contents)).toContain('holds no expression')
      })

      it('refuses an id that is not an id', async () => {
        const pageName = await idPageName()

        const res = await writeExpr(pageName, 'not-an-id', 'main')
        expect(res.status).toBe(200)
        expect(JSON.stringify((await res.json()).contents)).toContain('is not an id')
      })

      it('refuses source it cannot parse', async () => {
        const [pageName, metaId] = await anExprId()

        const res = await writeExpr(pageName, metaId, '((((')
        expect(res.status).toBe(200)
        expect((await res.json()).tag).toBe('ResFail')
      })

      it('--readonly refuses an expression write', async () => {
        const [pageName, metaId] = await anExprId(READONLY_API_BASE)

        const res = await writeExpr(pageName, metaId, 'main', READONLY_API_BASE)
        expect(res.status).toBe(200)
        const body = await res.json()
        expect(body.tag).toBe('ResFail')
        expect(JSON.stringify(body.contents)).toContain('readonly')
      })
    })

    it('round-trips a page source', async () => {
      const pageName = await idPageName()

      const original = (await fetchApi(`/api/page?prgmName=${encodeURIComponent(pageName)}`))
        .contents[0].pgSource
      expect(typeof original).toBe('string')

      // Write the identical bytes back: exercises the whole write path without
      // actually modifying the checked-in file.
      const written = await writeApi(pageName, original)
      expect(written.status).toBe(200)
      expect((await written.json()).tag).toBe('Success')

      // A write re-reads every file the doc root reaches, but a file that has not changed now
      // reads back as an equal program, so no page is invalidated and this is an ordinary cached
      // page load. Measured on a quiet dev machine: 0.2s for the read, 0.1s for the write and 0.3s
      // for the read after it, against the ~170s that read cost when a write rebuilt the stdlib.
      // Comfortably inside the default budget, so this test needs none of its own.
      const after = (await fetchApi(`/api/page?prgmName=${encodeURIComponent(pageName)}`))
        .contents[0].pgSource
      expect(after).toBe(original)
    })
  })
})
