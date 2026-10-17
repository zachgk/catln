--------------------------------------------------------------------
-- |
-- Module    :  CompileExpr
-- Copyright :  (c) Zach Kimberg 2026
-- License   :  MIT
-- Maintainer:  zachary@kimberg.com
-- Stability :  experimental
-- Portability: non-portable
--
-- This module compiles Catln source that is not a page of the program: an expression written after
-- the program was built, compiled against pages that already are, the way one of those pages was
-- compiled against what it imports.
--
-- It is beneath the evaluator, so an evaluation can compile source it is handed
-- ('Eval.parseExprPrim'), and beneath the compilation service, which compiles what a debugger asks
-- to run ('CtService.debugExprCompile').
--------------------------------------------------------------------

module CompileExpr where

import           CRes
import           Data.Char               (isSpace)
import           Data.Graph
import           Data.List               (dropWhileEnd)
import           MapMeta                 (parseMetaIDScope)
import           Semantics.Prgm
import           Semantics.Types         (mkName)
import           Syntax.Ct.Desugarf      (desFinalPass, desPrgm)
import           Syntax.Ct.Desugarf.Expr (desFileImport)
import           Syntax.Ct.MapRawMeta    (addRawPrgmMetaIDs)
import           Syntax.Ct.Parser.Expr   (pExpr)
import qualified Syntax.Ct.Parser.Syntax as PS
import           Syntax.Ct.Prgm
import           Text.Megaparsec         (errorBundlePretty, runParser)
import           Text.Printf
import           TypeCheck               (typecheckPrgms)
import           TypeCheck.Common        (PPrgm, TPrgm, typeCheckToRes)
import           Utils

-- | A typechecked page, under the import it was read as, with the imports it reaches.
type TPrgmNode = (TPrgm, FileImport, [FileImport])

-- | Flattens the program reachable from a file into one. It is polymorphic in the program because
-- every pipeline stage holds one, and scoping a stage to a file has to mean the same thing at each:
-- that file plus everything it imports.
prgmFromGraphData :: (Monoid p) => FileImport -> GraphData p FileImport -> CRes p
prgmFromGraphData prgmName (prgmGraph, nodeFromVertex, vertexFromKey) = case vertexFromKey prgmName of
  Just v -> return $ mconcat $ map (fst3 . nodeFromVertex) $ reachable prgmGraph v
  Nothing -> fail $ printf "Could not find prgm %s" (show prgmName)

-- | One expression read from Catln source, by the parser a page's expressions are read with. The
-- name is what its positions carry in place of a page's path. A failure is the parser's own
-- description of where and why.
parseExprSource :: String -> String -> Either String PS.PExpr
parseExprSource name src = case runParser pExpr name src of
  -- Without the line break the parser ends its description with, so it reads as a message.
  Left err     -> Left $ dropWhileEnd isSpace $ errorBundlePretty err
  Right parsed -> Right parsed

-- | Compiles statements as one page, named @name@, importing the typechecked pages @scope@: desugared,
-- given @adjust@ before its final pass, and typechecked against those pages the way each of them
-- was typechecked against what it imports.
--
-- Answers with the page typechecked, and the program it is run in: that page with all of @scope@.
--
-- Pure: every step is a rewrite of a program that is already read and already built.
compileStatementsIn :: String -> [TPrgmNode] -> [PS.PStatementTree] -> (PPrgm -> CRes PPrgm) -> CRes (TPrgm, TPrgm)
compileStatementsIn name scope statements adjust = do
  let rawPrgm = RawPrgm [] statements
  let pageName = desFileImport $ mkRawFileImport $ RawValue emptyMetaN (mkName name)
  let rawPrgm' = addRawPrgmMetaIDs (parseMetaIDScope pageName) rawPrgm
  (desP, desName, _) <- desPrgm (rawPrgm', pageName, [])
  -- Adjusted before the final pass, which is what reads a page's declarations into its definitions
  -- ('Syntax.Ct.Desugarf.seedDefResultTypes'),
  -- so that a declaration the adjustment adds holds the definitions it declares.
  adjusted <- adjust desP >>= \p -> desFinalPass (p, desName, [], [])
  -- Failing on an error note, as a page's typecheck does ('TypeCheck.typecheckPrgm'): some errors,
  -- such as a definition answering more than it is declared to ('ResultNotGuaranteed'), are noted
  -- beside a typed page rather than in place of one.
  tprgm <- case failOnErrorNotes $ typeCheckToRes $ typecheckPrgms [(adjusted, [], map fst3 scope)] of
    CErr notes   -> CErr notes
    CRes _ []    -> fail $ printf "%s typechecked to no page" name
    CRes _ (t:_) -> return t
  prgm <- prgmFromGraphData desName $ graphFromEdges ((tprgm, desName, map snd3 scope) : scope)
  return (tprgm, prgm)
