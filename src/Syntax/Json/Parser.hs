--------------------------------------------------------------------
-- |
-- Module    :  Syntax.Json.Parser
-- Copyright :  (c) Zach Kimberg 2026
-- License   :  MIT
-- Maintainer:  zachary@kimberg.com
-- Stability :  experimental
-- Portability: non-portable
--
-- Reading a JSON file as the page holding what it says.
--
-- What a JSON file holds is a value and not a program, which is what separates this from
-- "Syntax.Python.Parser": a Python file is read for the declarations it makes, and a JSON file is
-- read for the one thing it is. So the page produced here holds a single declaration, whose body is
-- the whole of the file as a @\/Data\/Json@ — the type @stack\/json\/json.ct@ declares.
--
-- This is not registered as a syntax the compiler knows. It is reached only through the primitive in
-- "Eval.Runtime.Import", which @stack\/json\/importer.ct@ names with @#runtime@ — so what says a
-- @.json@ file is read this way is a Catln declaration a program imports, and not a table in here.
--------------------------------------------------------------------

module Syntax.Json.Parser where

import           CtConstants
import           Data.Aeson        (Value (..), eitherDecodeStrict)
import qualified Data.Aeson.Key    as AK
import qualified Data.Aeson.KeyMap as AKM
import qualified Data.ByteString   as BS
import           Data.Foldable     (toList)
import           Data.Scientific   (toRealFloat)
import qualified Data.Text         as T
import           Semantics.Prgm
import           Semantics.Types   (Constant (CFloat), mkName, partialKey)
import           Syntax.Ct.Builder
import           Syntax.Ct.Prgm
import           Text.Printf

-- | The JSON file at this path, read as the page holding it.
--
-- The text is kept as 'piSource' so the doc server has something to show and the editor something
-- to save, and the file alone is reported as what the reading read — a JSON file names nothing else,
-- so nothing else can make its page stale.
jsonParseFile :: FilePath -> IO ParsedImport
jsonParseFile filename = do
  contents <- BS.readFile filename
  src <- readFile filename
  case eitherDecodeStrict contents of
    Left err -> fail $ printf "%s is not JSON: %s" filename err
    Right value -> return $ ParsedImport (jsonPrgm filename value) (Just src) (Just [ReadFile filename])

-- | The program a JSON file holds: the type's page imported, and the file declared as one value.
jsonPrgm :: FilePath -> Value -> RawPrgm ()
jsonPrgm filename value = RawPrgm [mkRawFileImport (rawStr jsonTypesPage)] (jsonStatements filename value)

-- | The statements that page holds: the name to write to reach the value, the value, and a display
-- of it.
--
-- The name is written into the page as a heading for the reason 'Syntax.FileDir.Parser.fileDirStatements'
-- writes one — it is built from the path rather than chosen, so it is the one part of the page a
-- reader cannot guess and has to reproduce by hand.
--
-- The @#print@ is what puts the document on the page as the tree it is. Without it the page shows the
-- declaration as the source it was generated as, which is the whole file written out again on one
-- line; with it the webdocs are handed the evaluated @\/Data\/Json@ and fold it. A declaration rather
-- than a bare value statement, because a value with no name is a value nothing can be written to
-- reach, and reaching it is what reading a JSON file is for.
jsonStatements :: FilePath -> Value -> [RawStatementTree RawExpr ()]
jsonStatements filename value = [heading, decl, display]
  where
    name = jsonFileName filename
    heading = RawStatementTree (RawAnnot (rawComment $ printf "The JSON in %s. Write `%s` to reach it." filename name)) []
    decl = RawStatementTree (RawDeclStatement $ RawObjArr (Just (rawVal name)) FunctionObj Nothing [] (Just (Just (jsonExpr value), Nothing)) Nothing) []
    display = RawStatementTree (RawAnnot (rawVal printAnnot `applyRawArgs` [(Just $ partialKey (mkName printAnnotText), rawVal name)])) []

-- | One piece of JSON as the expression building it.
--
-- Six arms for JSON's six kinds, which is the whole of the format. A container's children are a
-- 'RawList' rather than a cons chain built by hand: desugaring turns @[a, b]@ into
-- @\/Data\/List\/Cons@ and @\/Data\/List\/Nil@ itself, so building the chain here would produce the
-- same program while printing as @Cons(head= ...)@ wherever the page is shown.
jsonExpr :: Value -> RawExpr ()
jsonExpr Null = rawVal jsonNullPath
jsonExpr (Bool b) = rawVal jsonBoolPath `applyRawArgs` [(Just $ partialKey (mkName jsonValueField), rawVal (if b then truePrim else falsePrim))]
jsonExpr (Number n) = rawVal jsonNumberPath `applyRawArgs` [(Just $ partialKey (mkName jsonValueField), RawCExpr emptyMetaN (CFloat $ toRealFloat n))]
jsonExpr (String t) = rawVal jsonStrPath `applyRawArgs` [(Just $ partialKey (mkName jsonValueField), rawStr (T.unpack t))]
jsonExpr (Array vs) = rawVal jsonArrayPath `applyRawArgs` [(Just $ partialKey (mkName jsonEntriesField), RawList emptyMetaN (map jsonExpr $ toList vs))]
jsonExpr (Object o) = rawVal jsonObjectPath `applyRawArgs` [(Just $ partialKey (mkName jsonEntriesField), RawList emptyMetaN (map memberExpr $ AKM.toAscList o))]
  where
    memberExpr (k, v) = rawVal jsonMemberPath `applyRawArgs` [
      (Just $ partialKey (mkName jsonMemberKeyField), rawStr (AK.toString k)),
      (Just $ partialKey (mkName jsonMemberValueField), jsonExpr v)
                                                             ]
