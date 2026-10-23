import { mergeConfig } from 'vite'
import { defineConfig } from 'vitest/config'
import viteConfig from './vite.config'

export default mergeConfig(viteConfig, defineConfig({
  test: {
    environment: 'jsdom',
    globals: true,
    // The monaco-vim stub the unit suites have, and none of the rest of their setup: that mocks
    // fetch, and these requests go to a real compiler. An integration test reaches monaco-vim
    // through the components it imports, and it does not resolve outside a bundler.
    setupFiles: ['./src/test/stubMonacoVim.js'],
    css: true,
    include: ['src/test/integration/**/*.test.{js,jsx,ts,tsx}'],
    // These run against a real compiler rather than a stub, so a case is as slow as compiling
    // whatever the doc server holds. What it holds grows: `buildSCCPipeline` offers every loaded
    // page as an external dependency of every other (see the comment on `ctssTestPages` in
    // `src/CtService.hs`), so serving one more program makes every request slower, not just the
    // requests about that program. A case which writes a file and reads it back pays that twice.
    // Raised from 15s when the vault running the Obsidian Tasks plugin was added beside the one
    // that does not; three of the write-and-read-back cases were then finishing just over 15s.
    testTimeout: 30000,
  },
}))
