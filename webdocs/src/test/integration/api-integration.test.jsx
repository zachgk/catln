/**
 * Integration tests that run against the real Catln Haskell backend.
 *
 * Prerequisites:
 *   Start the backend API servers before running these tests:
 *     stack exec catln doc test/Integration/code/id.ct test/Integration/code/obsidian.ct test/Integration/code/obsidianTasks.ct test/Integration/code/debug.ct -- --api
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

  // The search box gets its pages from a search of its own rather than from the sidebar's tree. They
  // are two answers to the same question, so they are checked against each other here: the
  // duplication is deliberate for the moment, and a divergence should be a reported fact.
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

  // What /api/call answers with is the value itself rather than a program carrying it, since a call
  // names what to evaluate instead of being a statement compiled to hold it.
  function callApiUrl(fun, args, prgmName) {
    const scope = prgmName ? `&prgmName=${encodeURIComponent(prgmName)}` : ''
    return `/api/call?fun=${encodeURIComponent(fun)}&args=${encodeURIComponent(JSON.stringify(args))}${scope}`
  }

  async function searchPages(query, prgmName) {
    const data = await fetchApi(callApiUrl('/Catln/Doc/searchWebdocs', {query}, prgmName))
    expect(data.tag).toBe('Success')
    const [val] = data.contents
    expect(val.name).toBe('/Catln/Doc/Show/Search')
    return JSON.parse(val.args['/pages'].contents)
  }

  // The search runs where the pages are. What crosses is what matched rather than every page there
  // is, which is the whole of why a box asks the compiler rather than filtering a list it was given.
  it('/api/call searches the pages for what it is given', async () => {
    const matched = await searchPages('webdoc')

    expect(matched.length).toBeGreaterThan(0)
    expect(matched.every(p => p.toLowerCase().includes('webdoc'))).toBe(true)
    expect(matched.some(p => p.endsWith('/stack/core/webdoc.ct'))).toBe(true)
    // Narrower than the whole list, which is the point of asking.
    expect(matched.length).toBeLessThan((await searchPages('')).length)
  })

  // The empty query is every page, which is what a box nobody has typed into covers -- and is what
  // the #search annotation is, so the two cannot come apart.
  it('/api/call answers the empty query with what #search does', async () => {
    const val = await annotValue('#search')
    expect(await searchPages('')).toEqual(JSON.parse(val.args['/pages'].contents))
  })

  it('/api/call matches without regard to case', async () => {
    expect(await searchPages('WEBDOC')).toEqual(await searchPages('webdoc'))
  })

  async function docPages(query, prgmName) {
    const data = await fetchApi(callApiUrl('/Catln/Doc/docPages', {query}, prgmName))
    expect(data.tag).toBe('Success')
    const [val] = data.contents
    expect(val.name).toBe('/Catln/Doc/Show/DocPages')
    return JSON.parse(val.args['/pages'].contents)
  }

  // The annotations that are pages of their own are read off the program rather than listed in the
  // webdocs, so what a box offers is what stack/core/webdoc.ct marks. This is that list, and the rule
  // deciding it: an annotation is offered once per combination of argument values the compiler can
  // name, and not at all where an argument names something only the reader knows.
  it('/api/call lists the annotations marked as pages of their own', async () => {
    const pages = await docPages('')
    const annots = pages.map(p => p.dpAnnot)

    // Taking a sealed class, so one page per member of it, written absolutely so that running it
    // does not depend on what the page it is run against happens to name.
    expect(annots).toEqual([
      '#listProgram(mode= /Catln/Doc/Desugar)',
      '#listProgram(mode= /Catln/Doc/TreeBuild)',
      '#listProgram(mode= /Catln/Doc/Typecheck)',
    ])
    // Offered under the name the member is picked by rather than the one it is run under.
    expect(pages.find(p => p.dpAnnot === '#listProgram(mode= /Catln/Doc/Typecheck)').dpLabel)
      .toBe('#listProgram(mode= Typecheck)')

    // Sorted, which is the order a box offers them in. Asserted apart from the list above so that a
    // list which grows is still held to being offered in an order.
    expect(annots).toEqual([...annots].sort())
  })

  // What is not offered, and the two different reasons: unmarked, and marked but with nothing to
  // offer. The list above says the same by being exhaustive; these say which rule left each out, so
  // that marking an annotation and getting nothing is told apart from not marking it.
  it('/api/call offers no annotation that is not a page, and none it cannot name a value for', async () => {
    const annots = (await docPages('')).map(p => p.dpAnnot)

    // Unmarked: a display of whatever it is written on, or of the doc server's own furniture, or of
    // one program rather than of the pages there are.
    for (const unmarked of ['#print', '#md', '#test', '#sidebar', '#search', '#typeInfer']) {
      expect(annots.some(a => a.startsWith(unmarked))).toBe(false)
    }

    // Would still be offered nothing if marked, since a String names something only the reader
    // knows: there is no list of type names or function names to pick from.
    for (const unnameable of ['#type(', '#debugger', '#debugExpr', '#typeInferPnt', '#overlay']) {
      expect(annots.some(a => a.startsWith(unnameable))).toBe(false)
    }
  })

  // Narrowed by the compiler, the same way and by the same rule the pages are, so a box narrowing
  // both sections at once cannot answer one query two ways.
  it('/api/call narrows the annotation pages by what was typed', async () => {
    // Narrowed by the argument's value, not only by the annotation's name, which is what makes the
    // three stages of a #listProgram three pages to a reader rather than one repeated.
    const matched = await docPages('typecheck')
    expect(matched.map(p => p.dpAnnot)).toEqual(['#listProgram(mode= /Catln/Doc/Typecheck)'])
    expect(matched.length).toBeLessThan((await docPages('')).length)

    expect(await docPages('TYPECHECK')).toEqual(matched)
    expect(await docPages('nosuchpage')).toEqual([])
  })

  // Naming a page is what keeps a call from compiling every page there is. It does not narrow what a
  // search finds: the pages there are do not depend on the page a search is called in.
  it('/api/call searches the same pages whichever page it is called in', async () => {
    const rootPage = (await fetchApi('/api/config')).contents[0].rootPage
    expect(await searchPages('core', rootPage)).toEqual(await searchPages('core'))
  })

  // A call that cannot be built is reported rather than raised: an error escaping a request is a
  // failed connection, which a browser answers by asking again, so a bad call would be made forever.
  it('/api/call reports a function it does not hold rather than failing', async () => {
    const data = await fetchApi(callApiUrl('nosuchfunction', {}))
    expect(data.tag).toBe('ResFail')
    expect(data.contents[0].msg).toMatch(/no single function named nosuchfunction/)
  })

  it('/api/call reports a call it cannot show rather than failing', async () => {
    const data = await fetchApi(callApiUrl('/Catln/Doc/searchWebdocs', {notAnArgument: 'x'}))
    expect(data.tag).toBe('ResFail')
    expect(data.contents[0].msg).toMatch(/does not reach \/Catln\/Doc\/DShow/)
  })

  it('/api/call reports arguments it cannot read rather than failing', async () => {
    const res = await fetch(`${API_BASE}/api/call?fun=%2FCatln%2FDoc%2FsearchWebdocs&args=notjson`)
    expect(res.status).toBe(200)
    const data = await res.json()
    expect(data.tag).toBe('ResFail')
    expect(data.contents[0].msg).toMatch(/not JSON/)
  })

  // What a reader types is sent as a value rather than written into Catln source to be parsed back,
  // so a query holding what would be syntax finds nothing rather than failing to compile.
  it('/api/call takes a query that would not parse as Catln', async () => {
    expect(await searchPages('") + #print(')).toEqual([])
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
    return pageNameEnding('/test/Integration/code/id.ct', base)
  }

  async function pageNameEnding(suffix, base = API_BASE) {
    const pages = await sidebarPages(base)
    const name = pages.map(p => p.sbPath).find(n => n.endsWith(suffix))
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
    // id.ct writes no #debug, so the run recorded none. An empty list rather than a missing one:
    // the page shows the records of every run, and one that recorded nothing says so.
    expect(run.drResult.erDebugs).toEqual([])
  })

  // A run's #debug records, which is what the annotation exists for: nothing reads them within the
  // program, so a run is the only thing that can be asked for them.
  it('/api/annot reports the #debug records of the run it makes', async () => {
    const page = await pageNameEnding('/test/Integration/code/debug.ct')
    const val = await annotValue('#debugger(fun= "/main")', page)
    const run = JSON.parse(val.args['/run'].contents)
    expect(run.drExit).toBe(0)

    // debug.ct writes `#debug(d= x)` then `#debug(d= x + 1)` on `inc`, which main calls with -1.
    // They come back in the order they were written, which is what makes the list a trace of the
    // run rather than a set of the values it happened to record.
    expect(run.drResult.erDebugs.map(d => d.edValue)).toEqual([
      {tag: 'IntVal', contents: -1},
      {tag: 'IntVal', contents: 0},
    ])
    expect(run.drResult.erDebugs.map(d => d.edPos[0].line)).toEqual([11, 12])
    expect(run.drResult.erDebugs.every(d => d.edPos[0].name.endsWith('/debug.ct'))).toBe(true)

    // Each names the call it was recorded in, which is a node of the tree beside it -- a definition
    // called twice records twice, and a record has to say which of the two it came from.
    const nodes = new Set()
    const collect = ([, , , children, id]) => { nodes.add(id); children.forEach(collect) }
    run.drResult.erTreebug.forEach(collect)
    expect(run.drResult.erDebugs.every(d => nodes.has(d.edNode))).toBe(true)
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

  // #debugExpr runs an expression rather than a named entry point, which is what lets a reader debug
  // something the program does not contain: the computation a #print shows the result of, or the one
  // generated call a test case ran on.
  describe('#debugExpr', () => {
    it('compiles and runs an expression in the scope of the page', async () => {
      const page = await idPageName()
      const val = await annotValue('#debugExpr(expr= "id(x= 3)")', page)
      expect(val.name).toBe('/Catln/Doc/Show/Debugger')
      // What was run travels with the value, as a function name does for #debugger.
      expect(val.args['/fun'].contents).toBe('id(x= 3)')

      // `id` is a definition of id.ct, so the expression means what it means on that page, and
      // really is run: it evaluates to 3 through an arrow the tree records. An expression is given
      // no /io handle, so it has no exit code and nothing to have printed.
      const run = JSON.parse(val.args['/run'].contents)
      expect(run.drExit).toBeNull()
      expect(run.drValue).toEqual({tag: 'IntVal', contents: 3})
      expect(run.drResult.erTreebug.length).toBeGreaterThan(0)
    })

    it('runs a bare literal, which names no definition at all', async () => {
      const page = await idPageName()
      const val = await annotValue('#debugExpr(expr= "\\"hi\\"")', page)
      const run = JSON.parse(val.args['/run'].contents)
      expect(run.drValue).toEqual({tag: 'StrVal', contents: 'hi'})
    })

    it('reports an expression it cannot read as an error note rather than failing the request', async () => {
      // A generated test input is rendered from a value, and not every value renders as something
      // that can be read back. The reader is told which expression could not be read.
      const page = await idPageName()
      const {data, value, notes} = await annotResult('#debugExpr(expr= "id(x=")', page)
      expect(data.tag).toBe('Success')
      expect(value).toBeNull()
      expect(notes.some(n => n.tp === 'CNoteError')).toBe(true)
    })

    it('reports #debugExpr with no page to be scoped to as an error note', async () => {
      // An expression means what the page it is written in makes it mean, and the join of every
      // page is not a page anything was written in.
      const {data, value, notes} = await annotResult('#debugExpr(expr= "id(x= 3)")')
      expect(data.tag).toBe('Success')
      expect(value).toBeNull()
      expect(JSON.stringify(notes)).toContain('not scoped to a single page')
    })
  })

  // The tasks of every note the doc server holds, gathered into one list. Unscoped, which is the
  // only way to be asking about every note rather than about one, and why the server is started with
  // the Obsidian vault beside id.ct.
  describe('#taskList', () => {
    // The note the vault fixture writes its tasks in, and the note of the vault beside it which
    // runs the Obsidian Tasks plugin. The two are read as the two members of /Data/Note/Task, so a
    // list holding both is what shows that the gather is of a class rather than of one type.
    const NOTE = '/test/Integration/code/artifacts/catln-test/Tasks.md'
    const FIRST = 'Read [[Welcome]]'
    const TASKS_NOTE = '/test/Integration/code/artifacts/catln-tasks/Tasks.md'

    const SIMPLE = '/Data/Note/Task/Simple'
    const OBSIDIAN = '/Data/Note/Task/Obsidian'

    function isSimple(task) {
      return task.ptTask.name === SIMPLE
    }

    // Everything a task said that is not a field of its own, which it carries as JSON.
    function restOf(task) {
      return JSON.parse(task.ptTask.args['/rest'].contents)
    }

    // A /Data/Maybe as the value it holds, or null where it holds none.
    function some(value) {
      return value.name === '/Data/Some' ? value.args['/value'].contents : null
    }

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
      return `/Data/Note/Task/Simple(contents= ${JSON.stringify(contents)}, state= /Data/Primitive/${done ? 'True' : 'False'})`
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
        // Either reading of a checkbox, since which one a note gets is its vault's to decide.
        expect([SIMPLE, OBSIDIAN]).toContain(task.ptTask.name)
        expect(typeof contentsOf(task)).toBe('string')
        // Beside where it was written, which is what editing it in a list across notes needs.
        expect(task.ptPage.endsWith('.md')).toBe(true)
        expect(task.ptExpr).toMatch(/^[0-9a-f-]{36}$/)
      }

      // Whether the box is checked is a field of the plain reading alone. The other says it with a
      // status, because which character in a box means done is its vault's to name.
      for (const task of found.filter(isSimple)) {
        expect(['/Data/Primitive/True', '/Data/Primitive/False']).toContain(task.ptTask.args['/state'].name)
      }

      // Both readings are in the one list, which is the whole of what gathering a class's members
      // rather than one type buys.
      expect(found.some(isSimple)).toBe(true)
      expect(found.some(t => !isSimple(t))).toBe(true)

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

    // What the other reading carries, once the whole pipeline has run rather than the line parser
    // alone: the vault's settings read off disk, a checkbox read by them, the value compiled, and
    // the list gathered out of the compiled page.
    it('reads the tasks of a vault running the plugin as the plugin writes them', async () => {
      const found = await tasks()
      const fromNote = found.filter(t => t.ptPage.endsWith(TASKS_NOTE))
      expect(fromNote.length).toBeGreaterThan(0)
      for (const task of fromNote) {
        expect(task.ptTask.name).toBe(OBSIDIAN)
      }

      // In the order the note writes them, which is what gathering both readings in one pass keeps.
      expect(fromNote.map(t => t.ptTask.args['/description'].contents)).toEqual([
        'Read the note',
        'Write this note',
        'Plan the rest',
        'Drop the old plan',
        'Ship it #work',
        'Water the plants',
        'Do this first',
        'Do this after',
        'Answer the question',
        'Book \u{1F4C5} lunch',
      ])

      // A status the vault named itself, which markdown knows nothing about.
      const begun = fromNote.find(t => t.ptTask.args['/description'].contents === 'Plan the rest')
      expect(begun.ptTask.args['/symbol'].contents).toBe('/')
      expect(begun.ptTask.args['/statusType'].name).toBe('/Data/Note/StatusType/InProgress')
      expect(restOf(begun).statusName).toBe('In Progress')

      // The fields the plugin wrote into the line, whether they are fields of the value or ride in
      // the JSON beside them.
      const ship = fromNote.find(t => t.ptTask.args['/description'].contents === 'Ship it #work')
      expect(contentsOf(ship)).toBe('Ship it #work \u{23EB} \u{1F4C5} 2026-01-04')
      expect(some(ship.ptTask.args['/due'])).toBe('2026-01-04')
      expect(ship.ptTask.args['/priority'].name).toBe('/Data/Note/Priority/High')
      expect(restOf(ship).tags).toEqual(['#work'])

      const waits = fromNote.find(t => t.ptTask.args['/description'].contents === 'Do this after')
      expect(restOf(waits).dependsOn).toEqual(['dcf64c', '0h17ye'])
      expect(restOf(waits).created).toBe('2026-01-01')

      // The date a task was finished, which is one field for done and cancelled alike.
      const written = fromNote.find(t => t.ptTask.args['/description'].contents === 'Write this note')
      expect(written.ptTask.args['/statusType'].name).toBe('/Data/Note/StatusType/Done')
      expect(restOf(written).statusDate).toBe('2026-01-05')

      // A mark with nothing it names after it is writing, which is why a line is read from its end.
      const lunch = fromNote.find(t => t.ptTask.args['/description'].contents.startsWith('Book'))
      expect(some(lunch.ptTask.args['/due'])).toBeNull()
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

  // A note which opens with properties naming a page on the web is written over that page. The
  // properties are read into a display of their own, and the address they name into an overlay
  // beside them -- neither of which the note itself wrote.
  describe('a note written over a page', () => {
    const NOTE = '/test/Integration/code/artifacts/catln-test/Overlay.md'
    const URL = 'https://catln.dev'
    const TASK = 'Finish reading the page these notes are written over'

    async function notePage() {
      const found = (await sidebarPages()).map(p => p.sbPath).find(p => p.endsWith(NOTE))
      expect(found).toBeDefined()
      return found
    }

    // The metas of every statement of the page, at any depth: a note's own statements are declared
    // inside the page it is read as.
    function statementMetas(statementTrees, found = []) {
      for (const [statement, subStatements] of statementTrees) {
        const expr = statement.tag === 'RawDeclStatement' ? statement.contents.roaObj
                   : statement.tag === 'RawAnnot' ? statement.contents
                   : null
        const meta = expr?.contents?.[0]
        if (meta?.getMetaDat) found.push(meta)
        statementMetas(subStatements, found)
      }
      return found
    }

    // The metas of the declarations alone, which is where a note's tasks are.
    function declMetas(statementTrees, found = []) {
      for (const [statement, subStatements] of statementTrees) {
        if (statement.tag === 'RawDeclStatement') {
          const meta = statement.contents.roaObj?.contents?.[0]
          if (meta?.getMetaDat) found.push(meta)
        }
        declMetas(subStatements, found)
      }
      return found
    }

    function pageValues(statementTrees) {
      return statementMetas(statementTrees).map(m => m.getMetaDat[2]).filter(Boolean)
    }

    async function valuesOfNote() {
      const page = (await fetchApi(`/api/page?prgmName=${encodeURIComponent(await notePage())}`)).contents[0]
      return {values: pageValues(page.pgPrgm[1]), source: page.pgSource}
    }

    it('reads its properties and the page it is written over out of the one block', async () => {
      const {values} = await valuesOfNote()

      const properties = values.find(v => v.name === '/Data/Note/Show/Properties')
      expect(properties).toBeDefined()
      // Carried as the block was written rather than as the properties read out of it, because the
      // note is written back from that same text.
      expect(properties.args['/text'].contents).toContain(`url: ${URL}`)
      expect(properties.args['/text'].contents).toContain('- reading')

      const overlay = values.find(v => v.name === '/Catln/Doc/Show/Overlay')
      expect(overlay).toBeDefined()
      expect(overlay.args['/url'].contents).toBe(URL)
    })

    // The overlay is a statement the note never wrote, and a statement the reversion does not know
    // is a note that cannot be saved at all. So this is the proof that naming a page to be written
    // over does not cost the note its saving -- and that the properties survive the save.
    it('is still written back with its properties when one of its tasks is checked', async () => {
      const page = await notePage()

      // The expression the note's task was declared by, which is what an edit of it names.
      const taskMetaId = async () => {
        const statements = (await fetchApi(`/api/page?prgmName=${encodeURIComponent(page)}`)).contents[0].pgPrgm[1]
        const found = declMetas(statements).find(m => m.getMetaDat[2]?.args?.['/contents']?.contents === TASK)
        expect(found).toBeDefined()
        return found.getMetaID
      }

      const check = async done => fetch(
        `${API_BASE}/api/write-expr?prgmName=${encodeURIComponent(page)}&metaId=${encodeURIComponent(await taskMetaId())}`, {
          method: 'POST',
          headers: {'Content-Type': 'text/plain'},
          body: `/Data/Note/Task/Simple(contents= ${JSON.stringify(TASK)}, state= /Data/Primitive/${done ? 'True' : 'False'})`,
        })

      try {
        const res = await check(true)
        expect(res.status).toBe(200)
        expect((await res.json()).tag).toBe('Success')

        const {source} = await valuesOfNote()
        // Written back as the note it was: opening with its properties, and now saying the box is
        // checked.
        expect(source.startsWith('---\n')).toBe(true)
        expect(source).toContain(`url: ${URL}`)
        expect(source).toContain(`- [x] ${TASK}`)
      } finally {
        await check(false)
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
