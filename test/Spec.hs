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
import           Syntax.DirListingTests      (dirListingTests)
import           Syntax.FileDirTests         (fileDirTests)
import           Syntax.FileWatchTests       (fileWatchTests)
import           Syntax.HsBuiltinsTests      (hsBuiltinsTests)
import           Syntax.ImporterCallTests    (importerCallTests)
import           Syntax.LiveReloadTests      (liveReloadTests)
import           Syntax.NativeTests          (nativeTests)
import           Syntax.ObsidianCacheTests   (obsidianCacheTests)
import           Syntax.ObsidianPlannerTests (obsidianPlannerTests)
import           Syntax.ObsidianTests        (obsidianTests)
import           Syntax.RereadTests          (rereadTests)
import           Syntax.RssCacheTests        (rssCacheTests)
import           Syntax.SyntaxTests          (syntaxTests)
import           Test.Tasty
import           TestService                 (libraryService, seededService)
import           Typecheck.PartitionTests    (partitionTests)
import           Typecheck.TypeCheckTests    (typecheckTests)

main :: IO ()
main = do
  -- Built before anything else, so every test's service starts from it rather than building it.
  lib <- libraryService
  let mk = seededService lib
  integrationTests' <- integrationTests mk
  syntaxTests' <- syntaxTests
  stackTests' <- stackTests lib
  typecheckTests' <- typecheckTests mk
  let catlnTests = testGroup "CatlnTests" [integrationTests', typeTests, inFlightTests, typecheckTests', partitionTests, syntaxTests', stackTests', obsidianTests, obsidianPlannerTests, obsidianCacheTests, rssCacheTests, rereadTests, fileDirTests, fileWatchTests, liveReloadTests, hsBuiltinsTests, evalCommonTests, printGoldenTests mk, refinementProofTests, refinementRuntimeTests mk, quantificationTests, systemTests mk, nativeTests, importerCallTests mk, dirListingTests]
  defaultMain catlnTests
