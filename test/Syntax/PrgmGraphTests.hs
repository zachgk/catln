-- | Tests for the program graph: what a @\/Catln\/Doc\/#prgmGraph@ draws when the compiler answers
-- it.
--
-- Answered through a real compilation service rather than by calling the primitive, as
-- 'Syntax.CalendarTests' answers a calendar and for its reason: what is worth testing is that the
-- graph the display carries is the graph the compiler built the page out of. A graph assembled here
-- out of imports written here would test nothing about that.
module Syntax.PrgmGraphTests where

import           CRes                    (CRes (..), prettyCNotes, runCResT)
import           CtService               (CTSS, ctssBaseFiles, ctssKeys,
                                          getEvalAnnots)
import           Data.Aeson              (FromJSON (..), decode, withObject,
                                          (.:))
import qualified Data.HashMap.Strict     as H
import           Data.List               (isInfixOf, isSuffixOf, nub, sort)
import qualified Data.Text.Lazy          as TL
import qualified Data.Text.Lazy.Encoding as TLE
import           Semantics               (testCtssConfig)
import           Semantics.Prgm          (FileImport, impDisp)
import           Semantics.Val           hiding (Val)
import           System.Directory        (makeAbsolute)
import           Test.Tasty
import           Test.Tasty.HUnit        (assertBool, assertEqual,
                                          assertFailure, testCase)

-- | The page the graph below is drawn for, which reaches a vault beside it as well as the standard
-- library.
prgmGraphPage :: FilePath
prgmGraphPage = "test/Integration/code/prgmGraph.ct"

-- | One page of the graph, read back out of the JSON the display carries it in.
data GraphNode = GraphNode { gnPath    :: String
                           , gnName    :: String
                           , gnImports :: [String]
                           , gnCore    :: Bool
                           , gnRoot    :: Bool
                           }

instance FromJSON GraphNode where
  parseJSON = withObject "GraphNode" $ \o -> GraphNode
    <$> o .: "pgnPath"
    <*> o .: "pgnName"
    <*> o .: "pgnImports"
    <*> o .: "pgnCore"
    <*> o .: "pgnRoot"

-- | One service for every case below, which is what keeps the cost of these tests to one compile of
-- the standard library rather than one per case.
graphedPages :: IO CTSS
graphedPages = ctssBaseFiles testCtssConfig [prgmGraphPage]

-- | The graph a page wrote, answered by the compiler the way a doc server asks for it.
graphedBy :: CTSS -> FilePath -> IO [GraphNode]
graphedBy ctss page = do
  prgmName <- pageAt page ctss
  annots <- runCResT $ getEvalAnnots ctss prgmName
  case annots of
    CErr notes -> assertFailure $ "the page did not compile: " ++ prettyCNotes notes
    CRes _ vals -> case [args | (_, TupleVal name args) <- vals, name == "/Catln/Doc/Show/PrgmGraph"] of
      [args] -> case H.lookup "/graph" args of
        Just (StrVal json) -> maybe (assertFailure $ "the answer is not JSON: " ++ json) return $
          decode (TLE.encodeUtf8 (TL.pack json))
        carried -> assertFailure $ "the display carries " ++ show carried ++ " rather than a graph"
      displays -> assertFailure $ "the page displays " ++ show (length displays) ++ " graphs"

-- | The page a service holds at a path, asked for by the path it resolves to.
pageAt :: FilePath -> CTSS -> IO FileImport
pageAt path ctss = do
  pages <- ctssKeys ctss
  resolved <- makeAbsolute path
  case [page | page <- pages, impDisp page `elem` [Just path, Just resolved]] of
    [page] -> return page
    held   -> assertFailure $ "the service holds " ++ show (length held) ++ " pages shown at " ++ path

testPrgmGraphing :: TestTree
testPrgmGraphing = withResource graphedPages (const $ return ()) $ \held -> testGroup "the program graph a page wrote, answered by the compiler" [

  testCase "rooted at the page it was drawn for and at no other" $ do
    ctss <- held
    graph <- graphedBy ctss prgmGraphPage
    assertEqual "the graph is not rooted at the one page it was drawn for"
      [True] (map (const True) (filter gnRoot graph))
    assertBool "the root is not the page the graph was drawn for" $
      all (("code/prgmGraph.ct" `isSuffixOf`) . gnPath) (filter gnRoot graph),

  -- A page is in the graph because something reached it, and what it reaches was reached too, so
  -- every edge lands on a node. An edge that did not would draw an arrow to nothing.
  testCase "with every import an edge to a page of the graph" $ do
    ctss <- held
    graph <- graphedBy ctss prgmGraphPage
    let paths = map gnPath graph
    assertEqual "an import names a page the graph does not hold"
      [] (sort [i | node <- graph, i <- gnImports node, i `notElem` paths]),

  -- What the page itself writes: the standard library, the vault beside it, and one of the vault's
  -- notes. Written out rather than derived, so a change to what the page imports shows up here.
  testCase "carrying the imports the page writes as its own edges" $ do
    ctss <- held
    graph <- graphedBy ctss prgmGraphPage
    let rootImports = concatMap gnImports (filter gnRoot graph)
    assertEqual "the page's own edges are not the imports it writes"
      3 (length rootImports)
    assertBool "the page has no edge to the standard library" $
      any ("stack/core" `isInfixOf`) rootImports
    assertBool "the page has no edge to the vault beside it" $
      any ("catln-tasks" `isSuffixOf`) rootImports
    assertBool "the page has no edge to the note it names" $
      any ("catln-tasks/Tasks.md" `isSuffixOf`) rootImports,

  -- A page which writes @import "core"@ is given the standard library twice, once as it wrote it
  -- and once as Syntax.Parsers.pageImports injects it. The graph draws one arrow: a second import
  -- of a page means nothing, and two arrows between one pair would say it meant something.
  testCase "drawing one edge per page imported, however often it is imported" $ do
    ctss <- held
    graph <- graphedBy ctss prgmGraphPage
    assertEqual "a page is drawn with two edges to the one page"
      [] [gnPath node | node <- graph, length (nub (gnImports node)) /= length (gnImports node)],

  -- The standard library is reached through the page's one import of it, so the pages that import
  -- reaches are what make the graph transitive rather than a listing of one page's imports.
  testCase "reaching past the page's own imports to what those pages import" $ do
    ctss <- held
    graph <- graphedBy ctss prgmGraphPage
    let rootImports = concatMap gnImports (filter gnRoot graph)
    let reached = [gnPath node | node <- graph, "stack/core/list.ct" `isSuffixOf` gnPath node]
    assertEqual "the graph does not hold a page it reaches only through another"
      1 (length reached)
    assertEqual "that page is one the graph's own page imports, so it proves nothing"
      [] (filter (`elem` rootImports) reached),

  -- The import graph and not a listing of the pages a read walked to. A vault does not import the
  -- notes it holds ('rpExtras', not 'rpImports'), so the graph stops at it, and the one note here
  -- is in the graph because the page names it and not because the vault holds it.
  testCase "stopping where a page holds pages it does not import" $ do
    ctss <- held
    graph <- graphedBy ctss prgmGraphPage
    let vault = [node | node <- graph, "catln-tasks" `isSuffixOf` gnPath node]
    assertEqual "the vault is not in the graph, so what it is drawn as proves nothing"
      1 (length vault)
    assertEqual "the vault is drawn as importing the notes it holds"
      [] [i | node <- vault, i <- gnImports node, "catln-tasks/" `isInfixOf` i]
    assertEqual "a note the page does not name is in the graph"
      [] [gnPath node | node <- graph, "Query.md" `isSuffixOf` gnPath node],

  testCase "marking the standard library's pages and nothing else" $ do
    ctss <- held
    graph <- graphedBy ctss prgmGraphPage
    assertBool "no page of the standard library is in the graph" $
      any gnCore graph
    assertEqual "a page marked as the standard library's is not one of its"
      [] [gnPath node | node <- graph, gnCore node, not ("core" `isInfixOf` gnPath node)]
    assertEqual "a page of the standard library is not marked as one"
      [] [gnPath node | node <- graph, not (gnCore node), "stack/core" `isInfixOf` gnPath node],

  -- The label a node is drawn under, which is the path's last part. Every path has one, so a node
  -- with no name would be drawn as an empty box.
  testCase "labelled by the file name of the page each node is" $ do
    ctss <- held
    graph <- graphedBy ctss prgmGraphPage
    assertEqual "a node has no name to be drawn under"
      [] [gnPath node | node <- graph, null (gnName node)]
    assertEqual "a node is not named by the last part of the path it is at"
      [] [gnPath node | node <- graph, not (("/" ++ gnName node) `isSuffixOf` gnPath node)]
                                                                                             ]

prgmGraphTests :: TestTree
prgmGraphTests = testGroup "PrgmGraphTests" [testPrgmGraphing]
