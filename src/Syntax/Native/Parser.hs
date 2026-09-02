--------------------------------------------------------------------
-- |
-- Module    :  Syntax.Native.Parser
-- Copyright :  (c) Zach Kimberg 2026
-- License   :  MIT
-- Maintainer:  zachary@kimberg.com
-- Stability :  experimental
-- Portability: non-portable
--
-- Reading a page with an importer written in Catln.
--
-- Every other syntax in 'Syntax.Parsers.importSyntaxes' reads a file the compiler knows how to
-- read. This one reads a file the compiler does NOT know how to read, by running a declaration
-- somebody wrote in a @.ct@ file — which is what makes the import system extensible without
-- editing the compiler.
--
-- It is one syntax rather than one per declared importer, and that is what keeps the rest of the
-- read unchanged. 'Syntax.Parsers.importSyntax' looks a syntax up by the head of the import
-- expression and 'Syntax.Parsers.newReading' makes every reader before the read begins, so a
-- syntax that appeared when a page happened to declare one would be a table built while it was
-- being read out of. Instead every import resolved to a Catln-declared importer canonicalizes to
-- the one head name @native@, carrying which importer to run as arguments:
--
-- > native("/abs/x.py", importer= "/abs/stack/python/importer.ct", fn= "parsePython")
--
-- The arguments are NAMED, which is what keeps them out of the way of everything that reads an
-- import: 'Syntax.Parsers.rawImportPath' reads only unnamed arguments, so the path is still the
-- path. It is the same shape @haskell@ already uses to carry its Stack root.
--
-- They are part of @impAbs@, and so part of the page's identity ('Semantics.Prgm.AFileImport'
-- compares by it). That is deliberate: one file read by two different importers genuinely is two
-- pages, and it is the same reason 'Syntax.InferImport.inferFileParser' decides a note's parser
-- from where the file is rather than from who named it.
--------------------------------------------------------------------

module Syntax.Native.Parser where

import           Semantics.Prgm
import           Semantics.Types   (Constant (CStr), nameStr)
import           Syntax.Ct.Builder (rawStr)
import           Syntax.Ct.Prgm
import           Text.Printf

-- | The head name every import read by a Catln-declared importer canonicalizes to.
nativeReference :: String
nativeReference = "native"

-- | The argument naming the page which declares the importer.
nativeImporterArg :: String
nativeImporterArg = "importer"

-- | The argument naming the declaration to run on that page, as it is written there.
nativeFnArg :: String
nativeFnArg = "fn"

-- | Whether a page is one read by an importer written in Catln.
--
-- Asked of the head of the import, which is the one place it is written down. What it is for is
-- 'CtService.ctssClearInvalidations': such a page's program came from running the importer rather
-- than from reading its own file, so a change to the importer has to make the page unread and not
-- merely unbuilt.
isNativeImport :: FileImport -> Bool
isNativeImport imp = (nameStr <$> maybeExprPath (impAbs imp)) == Just nativeReference

-- | What a native import says: the file to read, the page declaring the importer, the declaration
-- to run, and whatever else the import was written with.
data NativeImport = NativeImport {
  niPath     :: FilePath,
  niImporter :: FilePath,
  niFn       :: String,
  -- | The named arguments which are not this syntax's own, passed on to the importer as it was
  -- given them. An importer may declare arguments of its own, as @haskell@ takes a Stack root.
  niArgs     :: [(String, String)]
                                 }
  deriving (Eq, Show)

-- | Read a native import back out of the expression it canonicalized to.
--
-- Reported rather than pattern-failed, because what reaches here is built by
-- 'Syntax.Parsers.canonicalImport' and a shape this cannot read is a disagreement between the two
-- rather than a bad file.
readNativeImport :: MonadFail m => Expr () -> m NativeImport
readNativeImport e = do
  path <- case [s | ObjArr{oaObj=Nothing, oaArr=Just (Just (CExpr _ (CStr s)), _)} <- args] of
    (s:_) -> return s
    []    -> fail $ printf "A %s import names no file to read: %s" nativeReference (show e)
  importer <- named nativeImporterArg
  fn <- named nativeFnArg
  return $ NativeImport path importer fn others
  where
    args = exprAppliedArgs e

    -- Desugaring makes an argument name absolute, and an import is desugared by
    -- 'Syntax.Ct.Desugarf.Expr.desFileImport' before it is ever read back. Both forms are matched
    -- for the reason 'Syntax.Haskell.Parser' matches both: whether a given import has been through
    -- that is not this function's business.
    isArg name obj = nameOf obj == name

    namedArgs = [(nameOf obj, s) | ObjArr{oaObj=Just obj, oaArr=Just (Just (CExpr _ (CStr s)), _)} <- args]
    nameOf obj = case nameStr <$> maybeExprPath obj of
      Just ('/':n) -> n
      Just n       -> n
      Nothing      -> ""

    others = [(n, s) | (n, s) <- namedArgs, n `notElem` [nativeImporterArg, nativeFnArg]]

    named name = case [s | ObjArr{oaObj=Just obj, oaArr=Just (Just (CExpr _ (CStr s)), _)} <- args, isArg name obj] of
      (s:_) -> return s
      []    -> fail $ printf "A %s import has no %s argument: %s" nativeReference name (show e)

-- | Reading a page whose importer is written in Catln.
--
-- No reversion: what a page of some other syntax is written back as is that syntax's business, and
-- an importer says only how to read. A @.py@ page could not be written back before this either
-- ('Syntax.Python.Parser.pySyntax' answers nothing), so nothing is lost that was there.
nativeSyntax :: ImportSyntax
nativeSyntax = ImportSyntax reader (const Nothing) importedLinkDir Nothing
  where
    reader env = return ImportReader{
      irPreload = nativePreload env,
      irParse   = nativeParse env,
      irFinish  = return (),
      irBlocked = nativeBlocked env
                                    }

-- | The pages this one waits on: the page declaring its importer, where that page is not built yet.
--
-- Asked of 'ieReady' rather than by trying to run the importer, so that a page which IS ready is
-- not read twice — see 'ieReady'.
nativeBlocked :: ReadEnv -> Expr () -> IO [RawFileImport]
nativeBlocked env e = do
  ni <- readNativeImport e
  blocked <- ieReady (reImporters env) (niImporter ni)
  return [mkRawFileImport (rawStr $ ibPage b) | b <- maybe [] pure blocked]

-- | Where a native page sits in the graph.
--
-- Its one edge is the page declaring the importer that reads it. Everything else it imports is
-- written in the program the importer produces, and is not known until the importer has run — so a
-- page which cannot be read yet is in the graph with that one edge, which is exactly the edge
-- whoever is driving the read needs in order to know what to build.
--
-- The program is answered where the importer could be run, so that a page whose importer is ready
-- is read in one go rather than preloaded now and parsed again later.
nativePreload :: ReadEnv -> Expr () -> ImportPreloadResult
nativePreload env e = do
  ni <- readNativeImport e
  ready <- ieReady (reImporters env) (niImporter ni)
  case ready of
    Just _  -> return $ PreloadImport [importerImport ni] [] Nothing Nothing
    Nothing -> do
      parsed <- runNative env ni
      return (eagerPreload parsed){plImports = importerImport ni : plImports (eagerPreload parsed)}

-- | Read the page, by running the importer over it.
nativeParse :: ReadEnv -> Expr () -> ImportParseResult
nativeParse env e = runNative env =<< readNativeImport e

-- | Run the importer, and report a page which cannot be run rather than answering an empty one.
--
-- A blocked page must not reach here: 'irBlocked' is asked first and a page waiting on its importer
-- is never parsed. Reaching it anyway is a disagreement between the two, so it is reported with
-- what the service said rather than passed over.
runNative :: ReadEnv -> NativeImport -> IO ParsedImport
runNative env ni = do
  result <- ieInvoke (reImporters env) (niImporter ni) (niFn ni) (niPath ni) (niArgs ni)
  case result of
    Right parsed -> return parsed
    Left blocked -> fail $ printf "The importer %s declared in %s could not read %s: %s" (niFn ni) (ibPage blocked) (niPath ni) (ibWhy blocked)

-- | The import naming the page an importer is declared on.
importerImport :: NativeImport -> RawFileImport
importerImport ni = mkRawFileImport $ rawStr $ niImporter ni
