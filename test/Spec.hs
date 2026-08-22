{-# LANGUAGE BlockArguments #-}
module Main where
import           Integration.Integ
import           Semantics.TypesTests      (typeTests)
import           Stack.StackTests          (stackTests)
import           Syntax.CalendarTests      (calendarTests)
import           Syntax.FileDirTests       (fileDirTests)
import           Syntax.FileWatchTests     (fileWatchTests)
import           Syntax.LiveReloadTests    (liveReloadTests)
import           Syntax.ObsidianCacheTests (obsidianCacheTests)
import           Syntax.ObsidianQueryTests (obsidianQueryTests)
import           Syntax.ObsidianTaskTests  (obsidianTaskTests)
import           Syntax.ObsidianTests      (obsidianTests)
import           Syntax.RereadTests        (rereadTests)
import           Syntax.RssCacheTests      (rssCacheTests)
import           Syntax.SyntaxTests        (syntaxTests)
import           Test.Tasty
import           Typecheck.TypeCheckTests  (typecheckTests)

main :: IO ()
main = do
  integrationTests' <- integrationTests
  syntaxTests' <- syntaxTests
  stackTests' <- stackTests
  let catlnTests = testGroup "CatlnTests" [integrationTests', typeTests, typecheckTests, syntaxTests', stackTests', obsidianTests, obsidianTaskTests, obsidianQueryTests, obsidianCacheTests, rssCacheTests, rereadTests, fileDirTests, fileWatchTests, liveReloadTests, calendarTests]
  defaultMain catlnTests
