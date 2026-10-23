--------------------------------------------------------------------
-- |
-- Module    :  Typecheck.PartitionTests
-- Copyright :  (c) Zach Kimberg 2026
-- License   :  MIT
-- Maintainer:  zachary@kimberg.com
-- Stability :  experimental
-- Portability: non-portable
--
-- Tests for the per-function-SCC solve grouping ('TypeCheck.Partition').
--------------------------------------------------------------------
module Typecheck.PartitionTests where

import qualified Data.HashMap.Strict as H
import qualified Data.HashSet        as S
import           Data.List           (sortOn)
import           Data.Maybe
import           Hedgehog
import           Semantics.Prgm
import           Semantics.Types
import           Test.Tasty
import qualified Test.Tasty.Hedgehog as THG
import           Test.Tasty.HUnit
import           Testing.Generation  (genPrgms)
import           TypeCheck.Partition
import           Utils               (fst3)

-- | A minimal function item @name(args...)= body-referencing-refs@.
mkFuncOa :: String -> [String] -> ObjArr Expr ()
mkFuncOa name refs = ObjArr (Just objE) FunctionObj Nothing [] (Just (Just bodyE, emptyMetaN))
  where
    objE = Value emptyMetaN (mkName name)
    bodyE = foldl addRef (Value emptyMetaN (mkName "base")) refs
    -- Reference each name as an applied argument value: base(a= ref)
    addRef be r = TupleApply emptyMetaN (emptyMetaN, be) (EAppArg (mkIObjArr' r))
    mkIObjArr' r = ObjArr (Just (Value emptyMetaN (mkName "a"))) ArgObj Nothing [] (Just (Just (Value emptyMetaN (mkName r)), emptyMetaN))

-- | A data item @name@ (no arrow).
mkDataOa :: String -> ObjArr Expr ()
mkDataOa name = ObjArr (Just (Value emptyMetaN (mkName name))) TypeObj Nothing [] Nothing

-- | A declaration whose RESULT TYPE spells a name (no expression reference).
mkDeclOa :: String -> String -> ObjArr Expr ()
mkDeclOa name resultTypeName = ObjArr (Just (Value emptyMetaN (mkName name))) FunctionObj Nothing [] (Just (Nothing, emptyMetaT (relTypeVal (mkName resultTypeName))))

fixtureMap :: ObjectMap Expr ()
fixtureMap = objectMapFromList
  [ mkDataOa "/T/Cons"
  , mkDataOa "/T/Nil"
  , mkFuncOa "/T/insert" ["/T/insertStep", "/T/Cons"]
  , mkFuncOa "/T/insertStep" ["insert"] -- relative back-reference => 2-SCC
  , mkFuncOa "/T/sorted" ["/T/insert", "Nil"]
  , mkDeclOa "/T/pureCons" "/T/Cons" -- name only in the spelled result type
  ]

testFixtureGroups :: TestTree
testFixtureGroups = testCase "fixture groups: data prelude, SCC pair, dependency order" $ do
  let groups = itemSccGroups H.empty fixtureMap
  let keyOf = mkName
  -- Coverage: every item key in exactly one group.
  let flat = concat groups
  sortOn nameStr flat @?= sortOn nameStr (objectMapKeys fixtureMap)
  length flat @?= length (S.toList (S.fromList flat))
  -- Data prelude first, containing exactly the arrow-less items.
  head groups @?= [keyOf "/T/Cons", keyOf "/T/Nil"]
  -- insert/insertStep form one SCC (via the relative back-reference).
  let sccOf n = listToMaybe [g | g <- groups, keyOf n `elem` g]
  sccOf "/T/insert" @?= Just [keyOf "/T/insert", keyOf "/T/insertStep"]
  -- sorted depends on insert: its group comes after insert's.
  let groupIdx n = head [i | (i, g) <- zip [(0 :: Int)..] groups, keyOf n `elem` g]
  assertBool "insert before sorted" $ groupIdx "/T/insert" < groupIdx "/T/sorted"
  -- Determinism.
  itemSccGroups H.empty fixtureMap @?= groups

testMetaTypeEdges :: TestTree
testMetaTypeEdges = testCase "spelled result types create edges" $ do
  let refs = objArrRefNames (mkDeclOa "/T/pureCons" "/T/Cons")
  assertBool "result-type name collected" $ mkName "/T/Cons" `elem` refs

-- | Generated-program properties: the groups partition the item keys exactly.
propGroupsPartitionKeys :: Property
propGroupsPartitionKeys = property $ do
  prgmsNodes <- forAll genPrgms
  let prgms = map fst3 prgmsNodes
  let omap = mconcat $ map prgmObjMap prgms
  let groups = itemSccGroups H.empty omap
  sortOn nameStr (concat groups) === sortOn nameStr (objectMapKeys omap)

-- | Fragmenting keeps the source-program list shape and moves every item into
-- exactly one fragment.
propFragmentsCoverItems :: Property
propFragmentsCoverItems = property $ do
  prgmsNodes <- forAll genPrgms
  let prgms = map fst3 prgmsNodes
  let omap = mconcat $ map prgmObjMap prgms
  let groups = itemSccGroups H.empty omap
  let fragments = fragmentPrgms groups prgms
  map length fragments === map (const (length prgms)) groups
  let fragKeys = concatMap (concatMap (objectMapKeys . prgmObjMap)) fragments
  sortOn nameStr fragKeys === sortOn nameStr (concatMap (objectMapKeys . prgmObjMap) prgms)
  -- No fragment keeps statement annotations; the annot fragments carry them all.
  concatMap (concatMap prgmAnnots) fragments === []
  let annotFrags = annotFragmentPrgms prgms
  length (concatMap (concatMap prgmAnnots) annotFrags) === length (concatMap prgmAnnots prgms)

objectMapKeys :: ObjectMap e m -> [Name]
objectMapKeys (ObjectMap m) = H.keys m

partitionTests :: TestTree
partitionTests = testGroup "PartitionTests"
  [ testFixtureGroups
  , testMetaTypeEdges
  , THG.testProperty "propGroupsPartitionKeys" propGroupsPartitionKeys
  , THG.testProperty "propFragmentsCoverItems" propFragmentsCoverItems
  ]
