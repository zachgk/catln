--------------------------------------------------------------------
-- |
-- Module    :  Syntax.Ct.Prgm
-- Copyright :  (c) Zach Kimberg 2020
-- License   :  MIT
-- Maintainer:  zachary@kimberg.com
-- Stability :  experimental
-- Portability: non-portable
--
-- This module defines most of the types that make up the Catln Syntax.
--------------------------------------------------------------------

{-# LANGUAGE DeriveAnyClass            #-}
{-# LANGUAGE DeriveGeneric             #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE InstanceSigs              #-}

module Syntax.Ct.Prgm where

import           Data.Hashable
import qualified Data.HashMap.Strict as H
import           GHC.Generics        (Generic)

import           Data.Aeson          hiding (Object)
import           Data.List           (isPrefixOf, stripPrefix)
import           Data.Maybe          (fromJust, isJust, mapMaybe)
import           Semantics
import           Semantics.Annots    (isAnnot)
import           Semantics.Prgm
import           Semantics.Types
import           System.FilePath     (splitDirectories, takeFileName)
import           Text.Printf
import           Utils

data RawObjArr e m = RawObjArr {
  roaObj    :: !(Maybe (e m)),
  roaBasis  :: !ObjectBasis,
  roaDoc    :: !(Maybe DocComment),
  roaAnnots :: ![CompAnnot (e m)],
  roaArr    :: !(Maybe (Maybe (e m), Maybe (e m))), -- (arrExpr, arrMetaExpr for formatter)
  roaDef    :: !(Maybe (e m))
                               }
  deriving (Eq, Ord, Generic, Hashable, ToJSON, ToJSONKey)

data TypeProperty e m
  = TypePropProj TypeName (e m)
  | TypePropRel TypeName (e m)
  deriving (Eq, Ord, Show, Generic, Hashable, ToJSON)

data RawApplyTerm e m = RATermDeep (e m) | RATermChild (e m)
  deriving (Eq, Ord, Show, Hashable, Generic, ToJSON)
newtype RawApply e m = RawApply [RawApplyTerm e m]
  deriving (Eq, Ord, Show, Hashable, Generic, ToJSON)

-- Expr before desugar
data RawExpr m
  = RawCExpr (Meta m) Constant
  | RawValue (Meta m) TypeName
  | RawHoleExpr (Meta m) Hole
  | RawMacroValue (Meta m) TypeName
  | RawApplyExpr (Meta m) (RawApply RawExpr m)
  | RawFmtStrExpr (Meta m) String String
  | RawTheExpr (RawExpr m) -- ^ Written :TypeName and read as The TypeName
  | RawAliasExpr (RawExpr m) (RawExpr m) -- ^ base aliasExpr
  | RawTupleApply (Meta m) (Meta m, RawExpr m) [(Bool, RawObjArr RawExpr m)] -- Boolean for isSpreadArg
  | RawVarsApply (Meta m) (RawExpr m) [RawObjArr RawExpr m]
  | RawContextApply (Meta m) (Meta m, RawExpr m) [RawObjArr RawExpr m]
  | RawWhere (Meta m) (RawExpr m) (RawExpr m) -- ^ base cond
  | RawParen (RawExpr m)
  | RawMethod (Meta m) (RawExpr m) (RawExpr m) -- ^ base methodValue
  | RawList (Meta m) [RawExpr m]
  | RawTypeProp (Meta m) (RawExpr m) (TypeProperty RawExpr m)
  deriving (Eq, Ord, Show, Generic, Hashable, ToJSON)

data RawStatement e m
  = RawDeclStatement (RawObjArr e m)
  | RawBindStatement (RawObjArr e m) -- Uses <-
  | RawAnnot (CompAnnot (e m))
  deriving (Eq, Ord, Show, Hashable, Generic, ToJSON)

data RawStatementTree e m = RawStatementTree (RawStatement e m) [RawStatementTree e m]
  deriving (Eq, Ord, Show, Hashable, Generic, ToJSON)

type RawFileImport = AFileImport (RawExpr ())
data RawPrgm m = RawPrgm [RawFileImport] [RawStatementTree RawExpr m] -- TODO: Include [Export]
  deriving (Eq, Ord, Show, Hashable, Generic, ToJSON)

data ReplRes m
  = ReplStatement (RawStatementTree RawExpr m)
  | ReplExpr (RawExpr m)
  | ReplErr ParseErrorRes
  deriving (Eq, Show)

-- | Something on disk a reading depended on.
--
-- A reading which walked a tree says so, rather than naming every directory it walked. Both are the
-- same question — has anything the reading looked at been written to — but a reading which resolves
-- what it holds against a whole tree ('Syntax.Obsidian.Parser.notePrgm' resolves a note's links
-- against every note of its vault) would otherwise name every directory of that tree in every page
-- read out of it, which is the tree named once for each page in it.
--
-- What was read is said apart from where it was, because the two are answered differently and only
-- the reading knows which it did. A file is written to when its text changes; a directory when an
-- entry appears in it, goes from it or is renamed within it. Nothing on disk tells them apart
-- without being asked, so a reading which said only a path would have every reader of it ask —
-- 'CtService.readPathWatches' would have to look at the disk to know what to watch.
data ReadPath
  -- | A file whose text was read.
  = ReadFile FilePath
  -- | A directory whose listing was read.
  | ReadDir FilePath
  -- | A directory and the listings under it, at any depth.
  | ReadTree FilePath
  deriving (Eq, Ord, Show, Generic, Hashable)

-- | Whether a reading of a tree walks into an entry named this.
--
-- One rule in one place because four readings must agree on it, and nothing but agreement makes
-- them right: what a tree read walks ('Syntax.Obsidian.Parser.vaultNotes'), what timing a tree
-- looks at ('Syntax.Parsers.treeWrittenAt'), what writing to a tree means ('readsUnder', through
-- 'Syntax.Parsers.readPathWrittenIn'), and what a watch on a tree reports ('CtService.ctssWatch').
-- A path one of them counts and another does not is at best a page read again for a change it never
-- read, and at worst a page never read again for one it did.
--
-- Entries named with a leading dot are passed over. An Obsidian vault keeps its own configuration
-- in one and rewrites it as the vault is used, which is not notes of the vault; a repository keeps
-- its history in another.
readsTreeEntry :: FilePath -> Bool
readsTreeEntry entry = not $ "." `isPrefixOf` takeFileName entry

-- | Whether a reading of the tree under a directory reads at a path.
--
-- Every part of the way down is asked, not just the last, because a reading walks: a directory it
-- never walked into is one whose every entry it never saw.
--
-- Compared a directory at a time rather than as text, or a vault at @notes@ would be read from
-- every write to a @notes-archive@ beside it.
readsUnder :: FilePath -> FilePath -> Bool
readsUnder dir path = case stripPrefix (splitDirectories dir) (splitDirectories path) of
  Just under -> all readsTreeEntry under
  Nothing    -> False

-- | What reading a file as a page produced, and what the reading read to produce it.
data ParsedImport = ParsedImport {
  piPrgm         :: RawPrgm (),
  -- | Imports the parse found which the program does not itself write: the files of a directory,
  -- the notes of an Obsidian vault.
  piExtraImports :: [RawFileImport],
  -- | The text the page was read from, where the page is one file and so has one to show.
  piSource       :: Maybe String,
  -- | Everything on disk this reading depended on: the files it read, and the directories and trees
  -- whose listing decided what the program holds. A page is read again exactly when one of these is
  -- written to, so one left out here is a page which goes stale, and one put in needlessly is a page
  -- read more often than it need be. A directory is written to when an entry is added to it, removed
  -- from it or renamed within it, which is every way a listing of it can change.
  --
  -- 'Nothing' is a reading which cannot say what it read, and has its page read again every time
  -- rather than have a change to it go unseen.
  piReads        :: Maybe [ReadPath]
                                 }

type ImportParseResult = IO ParsedImport

-- | How a file written in one syntax is read as a page, and written back as that syntax.
--
-- The two directions belong together because only the parser which read a file knows what its
-- program means as that syntax. A syntax which cannot be written back says so by answering nothing,
-- rather than by having no reversion at all, so that every syntax is asked the same question and a
-- page which cannot be saved is reported rather than mistaken for one which can.
data ImportParser = ImportParser {
  -- | Reads the file an import names as the program it is.
  ipParse   :: Expr () -> ImportParseResult,
  -- | Writes a page back as the file it was read from, or nothing when that syntax has no reversion.
  --
  -- A reversion writes the program rather than the file: it may write back what the file said in a
  -- form other than the one the file said it in.
  ipUnparse :: RawPrgm () -> Maybe String
                                 }

-- | A parser which reads a syntax but cannot write it back.
readOnlyParser :: (Expr () -> ImportParseResult) -> ImportParser
readOnlyParser parse = ImportParser parse (const Nothing)

instance ExprClass RawExpr where
  getExprMeta expr = case expr of
    RawCExpr m _          -> m
    RawValue m _          -> m
    RawHoleExpr m _       -> m
    RawMacroValue m _     -> m
    RawFmtStrExpr m _ _   -> m
    RawApplyExpr m _      -> m
    RawTheExpr e          -> getExprMeta e
    RawAliasExpr b _      -> getExprMeta b
    RawWhere m _ _        -> m
    RawTupleApply m _ _   -> m
    RawVarsApply m _ _    -> m
    RawContextApply m _ _ -> m
    RawParen e            -> getExprMeta e
    RawMethod m _ _       -> m
    RawList m _           -> m
    RawTypeProp m _ _     -> m

  setExprMeta expr m' = case expr of
    RawCExpr _ c           -> RawCExpr m' c
    RawValue _ v           -> RawValue m' v
    RawHoleExpr _ v        -> RawHoleExpr m' v
    RawMacroValue _ v      -> RawMacroValue m' v
    RawFmtStrExpr _ f s    -> RawFmtStrExpr m' f s
    RawApplyExpr _ v       -> RawApplyExpr m' v
    RawTheExpr e           -> RawTheExpr $ setExprMeta e m'
    RawAliasExpr b a       -> RawAliasExpr (setExprMeta b m') a
    RawWhere _ b c         -> RawWhere m' b c
    RawTupleApply _ b as   -> RawTupleApply m' b as
    RawVarsApply _ b vs    -> RawVarsApply m' b vs
    RawContextApply _ b vs -> RawContextApply m' b vs
    RawParen e             -> RawParen $ setExprMeta e m'
    RawMethod _ b e        -> RawMethod m' b e
    RawList _ ls           -> RawList m' ls
    RawTypeProp _ t v      -> RawTypeProp m' t v

  maybeExprPathM (RawValue m n)               = Just (n, m)
  maybeExprPathM (RawTupleApply _ (_, e) _)   = maybeExprPathM e
  maybeExprPathM (RawVarsApply _ e _)         = maybeExprPathM e
  maybeExprPathM (RawContextApply _ (_, e) _) = maybeExprPathM e
  maybeExprPathM (RawParen e)                 = maybeExprPathM e
  maybeExprPathM (RawMethod _ _ e)            = maybeExprPathM e
  maybeExprPathM (RawWhere _ b _)             = maybeExprPathM b
  maybeExprPathM _                            = Nothing

  exprAppliedArgs = error "Called exprAppliedArgs on a RawExpr. Should have called rawExprAppliedArgs"

  exprAppliedOrdVars (RawValue _ _) = []
  exprAppliedOrdVars (RawTupleApply _ (_, be) _) = exprAppliedOrdVars be
  -- exprAppliedOrdVars (RawVarsApply _ e vars) = H.union (exprAppliedOrdVars e) (H.fromList vars)
  exprAppliedOrdVars RawVarsApply{} = error "Not implemented"
  exprAppliedOrdVars (RawContextApply _ (_, e) _) = exprAppliedOrdVars e
  exprAppliedOrdVars (RawParen e) = exprAppliedOrdVars e
  exprAppliedOrdVars (RawMethod _ _ e) = exprAppliedOrdVars e
  exprAppliedOrdVars _ = error "Unsupported RawExpr exprAppliedOrdVars"

  exprVarArgs RawCExpr{} = H.empty
  exprVarArgs RawHoleExpr{} = H.empty
  exprVarArgs RawValue{} = H.empty
  exprVarArgs RawTheExpr{} = H.empty
  exprVarArgs (RawAliasExpr base alias) = H.unionWith (++) (exprVarArgs base) (exprVarArgs alias)
  exprVarArgs (RawTupleApply _ (_, be) args) = H.unionWith (++) (exprVarArgs be) (unionsWith (++) $ map aux args)
    where
      aux (False, a) = roaVarArgs a
      aux (True, a)  = error $ printf "Not yet implemented %s" (show a)
  exprVarArgs (RawVarsApply _ e vars) = H.unionWith (++) (exprVarArgs e) (unionsWith (++) $ map aux vars)
    where
      aux var = H.singleton (TVVar $ rawInExprSingleton $ fromJust $ roaObj var) [(fromJust $ roaObj var, getExprMeta $ fromJust $ snd $ fromJust $ roaArr var)]
  exprVarArgs (RawContextApply _ (_, e) _) = exprVarArgs e
  exprVarArgs (RawParen e) = exprVarArgs e
  exprVarArgs (RawMethod _ be me) = H.unionWith (++) (exprVarArgs be) (exprVarArgs me)
  exprVarArgs e = error $ printf "Unsupported RawExpr exprVarArgs for %s" (show e)

  exprVarArgsWithSrc = undefined
  mkValue = undefined

instance ObjArrClass RawObjArr where
  oaVarArgs _roa = error "Found call to raw oaVarArgs. Should be using roaVarArgs."
  getOaAnnots = roaAnnots

instance (Show m, Show (e m)) => Show (RawObjArr e m) where
  show RawObjArr{roaObj, roaArr, roaDef} = printf "%s%s%s" (showNoMaybe roaObj) showArr showDef
    where
      showNoMaybe :: (Show a) => Maybe a -> String
      showNoMaybe (Just a) = show a
      showNoMaybe Nothing  = ""

      showArr :: String
      showArr = case roaArr of
        Just (Just e, Just m) -> printf " -> %s = %s" (show m) (show e)
        Just (Just e, _) | isJust roaObj -> printf "= %s" (show e)
        Just (Just e, _) -> show e
        Just (Nothing, Just m) -> printf " -> %s" (show m)
        Just (Nothing, _) -> ""
        Nothing -> ""

      showDef :: String
      -- showDef = maybe "" show roaDef
      showDef = case roaDef of
        Just def -> printf " ? %s" (show def)
        Nothing  -> ""

mkRawFileImport :: RawExpr () -> RawFileImport
mkRawFileImport e = AFileImport e e Nothing Nothing Nothing

rawExprAppliedArgs :: (Show m, MetaDat m) => RawExpr m -> [RawObjArr RawExpr m]
rawExprAppliedArgs (RawValue _ _) = []
rawExprAppliedArgs (RawTupleApply _ (_, be) args) = rawExprAppliedArgs be ++ concatMap aux args
  where
    aux (False, a) = [a]
    aux (True, a)  = error $ printf "Not yet implemented %s" (show a)
rawExprAppliedArgs (RawVarsApply _ e _) = rawExprAppliedArgs e
rawExprAppliedArgs (RawContextApply _ (_, e) _) = rawExprAppliedArgs e
rawExprAppliedArgs (RawParen e) = rawExprAppliedArgs e
rawExprAppliedArgs (RawMethod _ _ e) = rawExprAppliedArgs e
rawExprAppliedArgs _ = error "Unsupported RawExpr rawExprAppliedArgs"

rawExprAppliedArgsMap :: (MetaDat m, Show m) => RawExpr m -> H.HashMap ArgName (Maybe (Meta m, Maybe (RawExpr m)))
rawExprAppliedArgsMap = H.fromList . mapMaybe mapArg . rawExprAppliedArgs
  where
    mapArg :: (MetaDat m, Show m) => RawObjArr RawExpr m -> Maybe (ArgName, Maybe (Meta m, Maybe (RawExpr m)))
    mapArg RawObjArr{roaObj=Just oe, roaArr=Just (arrExpr, oaMExpr)} = Just (rawInExprSingleton oe, Just (getExprMeta $ fromJust oaMExpr, arrExpr))
    mapArg RawObjArr{roaObj=Just oe, roaArr=Nothing} = Just (rawInExprSingleton oe, Nothing)
    mapArg _ = Nothing

rawInExprSingleton :: (MetaDat m, Show m) => RawExpr m -> ArgName
rawInExprSingleton e = maybe
  (partialKey $ makeAbsoluteName $ exprPath e) {pkArgs = H.keysSet $ rawExprAppliedArgsMap e, pkVars = H.keysSet $ exprAppliedVars e} partialToKey (maybeGetSingleton (getExprType e))

roaVarArgs :: (MetaDat m, Show m) => RawObjArr RawExpr m -> H.HashMap TypeVarAux [(RawExpr m, Meta m)]
roaVarArgs RawObjArr{roaArr=(Just (Just argVal, _))} = exprVarArgs argVal
roaVarArgs RawObjArr{roaObj=(Just obj), roaArr= Nothing} = H.singleton (TVArg $ rawInExprSingleton obj) [(obj, emptyMetaE obj)]
roaVarArgs RawObjArr{roaObj=(Just obj), roaArr= Just (Nothing, _)} = H.singleton (TVArg $ rawInExprSingleton obj) [(obj, emptyMetaE obj)]
roaVarArgs oa = error $ printf "exprVarArgs not defined for arg %s" (show oa)

-- | Checks whether a program has a global annot
rawPrgmHasAnnot :: RawPrgm m -> String -> Bool
rawPrgmHasAnnot (RawPrgm _ statements) annot = any checkStatementTree statements
  where
    checkStatementTree (RawStatementTree (RawAnnot a) _) = isAnnot annot a
    checkStatementTree _                                 = False
