{-# LANGUAGE BlockArguments #-}
module Main where
import           Eval.CommonTests            (evalCommonTests)
import           Eval.PrintGoldenTests       (printGoldenTests)
import           Eval.QuantificationTests    (quantificationTests)
import           Eval.RefinementProofTests   (refinementProofTests)
import           Eval.RefinementRuntimeTests (refinementRuntimeTests)
import           Eval.SystemTests            (systemTests)
import           Integration.Integ
import           Semantics.InFlightTests     (inFlightTests)
import           Semantics.TypesTests        (typeTests)
import           Stack.StackTests            (stackTests)
import           Syntax.CalendarTests        (calendarTests)
import           Syntax.DirListingTests      (dirListingTests)
import           Syntax.FileDirTests         (fileDirTests)
import           Syntax.FileWatchTests       (fileWatchTests)
import           Syntax.HsBuiltinsTests      (hsBuiltinsTests)
import           Syntax.ImporterCallTests    (importerCallTests)
import           Syntax.LiveReloadTests      (liveReloadTests)
import           Syntax.NativeTests          (nativeTests)
import           Syntax.ObsidianCacheTests   (obsidianCacheTests)
import           Syntax.ObsidianPlannerTests (obsidianPlannerTests)
import           Syntax.ObsidianQueryTests   (obsidianQueryTests)
import           Syntax.ObsidianTaskTests    (obsidianTaskTests)
import           Syntax.ObsidianTests        (obsidianTests)
import           Syntax.RereadTests          (rereadTests)
import           Syntax.RssCacheTests        (rssCacheTests)
import           Syntax.SyntaxTests          (syntaxTests)
import           Test.Tasty
import           Typecheck.PartitionTests    (partitionTests)
import           Typecheck.TypeCheckTests    (typecheckTests)

main :: IO ()
main = do
  integrationTests' <- integrationTests
  syntaxTests' <- syntaxTests
  stackTests' <- stackTests
  typecheckTests' <- typecheckTests
  let catlnTests = testGroup "CatlnTests" [integrationTests', typeTests, inFlightTests, typecheckTests', partitionTests, syntaxTests', stackTests', obsidianTests, obsidianTaskTests, obsidianPlannerTests, obsidianQueryTests, obsidianCacheTests, rssCacheTests, rereadTests, fileDirTests, fileWatchTests, liveReloadTests, calendarTests, hsBuiltinsTests, evalCommonTests, printGoldenTests, refinementProofTests, refinementRuntimeTests, quantificationTests, systemTests, nativeTests, importerCallTests, dirListingTests]
  defaultMain catlnTests
