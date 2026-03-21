import { mergeConfig } from 'vite'
import { defineConfig } from 'vitest/config'
import viteConfig from './vite.config'

export default mergeConfig(viteConfig, defineConfig({
  test: {
    environment: 'jsdom',
    globals: true,
    css: true,
    include: ['src/test/integration/**/*.test.{js,jsx,ts,tsx}'],
    testTimeout: 15000,
  },
}))
