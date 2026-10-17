--------------------------------------------------------------------
-- |
-- Module    :  Eval.Runtime.Prgm
-- Copyright :  (c) Zach Kimberg 2026
-- License   :  MIT
-- Maintainer:  zachary@kimberg.com
-- Stability :  experimental
-- Portability: non-portable
--
-- Handing a program its own program.
--
-- Everything else in this project declared what a program IS. This is what gives one to a program:
-- the compiler's own 'Semantics.Prgm.Prgm', converted by the same reading that declared it, so what
-- a Catln program is handed and what the compiler holds cannot come apart.
--
-- Narrowed to a name rather than handed over whole. 'Eval.Common.getValType' builds a
-- 'Semantics.Types.PartialType' by walking every argument of a value, and it is asked for one on
-- every dispatch that touches the value — so a whole program, standard library and all, would be
-- walked structurally over and over. A program asks for the name it is interested in, which is the
-- same shape @\/Catln\/Doc\/filterProgram@ already answers questions in and is bounded by what was
-- asked for rather than by how big the library is.
--------------------------------------------------------------------

module Eval.Runtime.Prgm where

import           Data.Char            (isSpace)
import qualified Data.HashMap.Strict  as H
import qualified Data.HashSet         as S
import           Data.List            (dropWhileEnd, intercalate, nub, sortOn)
import           Data.Maybe           (fromMaybe, listToMaybe, mapMaybe)
import           Eval.Common
import           Eval.Runtime.WebDocs (filterByType)
import           MapMeta              (clearMetaDat, mapMetaPrgm)
import           Semantics.CtType     (fromCt, toCt)
import           Semantics.Prgm
import           Semantics.Types
import           Syntax.Ct.Formatter  (formatRootPrgm)
import           Syntax.Ct.Prgm       (RawExpr, RawPrgm (..),
                                       RawStatementTree (..), isCorePath)
import           Syntax.Ct.Results    (statementSpan)
import           Text.Megaparsec      (SourcePos (..), sourceName)
import           Text.Printf
import           Utils                (graphLookup)

-- | The primitives backing the compiler's own program types.
prgmPrims :: [EPrim]
prgmPrims = [prgmFor, pageAt, typeText, typeDecls]

-- | The argument of @\/Catln\/prgmFor@ naming what to be given.
prgmForNameArg :: String
prgmForNameArg = "/name"

-- | Backs @\/Catln\/prgmFor@: the program being evaluated, narrowed to one name, as a
-- @\/Catln\/Prgm@ a Catln program can read.
--
-- The program comes from the evaluation's own 'Eval.Common.TBEnv', so what is answered is the
-- program the asking page was compiled as part of — the page and everything it reaches.
--
-- 'MapMeta.clearMetaDat' narrows it to the metadata every stage shares, which is what
-- @instance CtType (Prgm Expr ())@ is written at. What that drops is the stage's own supplement,
-- so a program reads the same shape whichever stage it asked about.
prgmFor :: EPrim
prgmFor = EEnvPrim "prgmFor" prim
  where
    prim Env{evTbEnv=TBEnv{tbPrgm}} args = case H.lookup prgmForNameArg args of
      Just (StrVal name) -> Right $ toCt $ mapMetaPrgm clearMetaDat $ filterByType name tbPrgm
      Just v -> Left $ printf "prgmFor was given %s, which is not a name" (show v)
      Nothing -> Left $ printf "prgmFor has no %s argument, only %s" prgmForNameArg (show $ H.keys args)

-- | The argument of @\/Catln\/pageAt@ naming the page to be given.
pageAtSourceArg :: String
pageAtSourceArg = "/source"

-- | Backs @\/Catln\/pageAt@: a page the program was read from, as it was written, as a
-- @\/Catln\/RawPrgm@ a Catln program can read.
--
-- From the pages the evaluation was handed ('evPages') rather than read from the file, so what is
-- answered is the page the program was compiled from, even where the file has changed since. A page
-- it was not handed is reported: an evaluation whose caller holds no pages has none to give, and a
-- page read again here would be a different page from the one compiled.
pageAt :: EPrim
pageAt = EEnvPrim "pageAt" prim
  where
    prim Env{evPages} args = case H.lookup pageAtSourceArg args of
      Just (StrVal source) -> case H.lookup source evPages of
        Just EvalPage{epRaw} -> Right $ toCt epRaw
        Nothing -> Left $ printf "pageAt was asked for %s, which is not a page this evaluation was handed (it holds %s)" source (show $ H.keys evPages)
      Just v -> Left $ printf "pageAt was given %s, which is not a source" (show v)
      Nothing -> Left $ printf "pageAt has no %s argument, only %s" pageAtSourceArg (show $ H.keys args)

-- | The argument of @\/Catln\/typeText@ and @\/Catln\/typeDecls@ holding the type.
typeArg :: String
typeArg = "/type"

-- | The type a primitive was handed as a @\/Catln\/Type@, read back as the one the compiler holds.
typeArgOf :: String -> H.HashMap String Val -> Either String Type
typeArgOf primName args = case H.lookup typeArg args of
  Just v -> case fromCt v of
    Right tp -> Right tp
    Left err -> Left $ printf "%s was given %s, which is not a type: %s" primName (show v) err
  Nothing -> Left $ printf "%s has no %s argument, only %s" primName typeArg (show $ H.keys args)

-- | Backs @\/Catln\/typeText@: a type written out as a reader of Catln would write it, for a reader
-- told what to write rather than for the parser. A type of one shape is written as that shape. A type
-- of several is written as the list of them, since a class is the only way to write a union and the
-- type may not be one. A type the compiler can only describe by what it excludes is written as the
-- compiler shows it.
typeText :: EPrim
typeText = EPrim "typeText" (fmap (StrVal . showTypeSource) . typeArgOf "typeText")

-- | A type as a reader of Catln would write it ('typeText').
showTypeSource :: Type -> String
showTypeSource (UnionType Nothing leafs consts) = case map showPartialSource (splitUnionType leafs) ++ map showConstSource consts of
  [one] -> one
  []    -> "nothing"
  parts -> "one of " ++ intercalate ", " parts
  where
    showConstSource (CInt i)   = show i
    showConstSource (CFloat f) = show f
    showConstSource (CStr t)   = show t
    showConstSource (CChar c)  = show c
showTypeSource tp = show tp

-- | One shape of a type as a reader of Catln would write it: its name, its type arguments, and its
-- arguments, each with its type. One that may hold more arguments than it names is written with the
-- @..@ that says so, where it names any.
showPartialSource :: PartialType -> String
showPartialSource PartialType{ptName, ptVars, ptArgs, ptArgMode} = nameStr ptName ++ vars ++ args
  where
    vars | H.null ptVars = ""
         | otherwise = "[" ++ intercalate ", " [('$' : dropSlash (nameStr (pkName k))) ++ " -> " ++ showTypeSource v | (k, v) <- sortOn fst (H.toList ptVars)] ++ "]"
    args | H.null ptArgs = ""
         | otherwise = "(" ++ intercalate ", " [dropSlash (nameStr (pkName k)) ++ " -> " ++ showTypeSource v | (k, v) <- sortOn fst (H.toList ptArgs)] ++ ")" ++ more
    more = case ptArgMode of
      PtArgMode True _ -> ""
      _                -> ".."
    dropSlash ('/' : n) = n
    dropSlash n         = n

-- | The argument of @\/Catln\/typeDecls@ naming the page whose program the type is read in.
typeDeclsSourceArg :: String
typeDeclsSourceArg = "/source"

-- | Backs @\/Catln\/typeDecls@: the statements declaring what a type is made of, as they were
-- written, so that a reader told to write a value of the type can read what it is.
--
-- That is the declaration of each shape the type holds, of each member of each class it holds, and
-- in turn of what the arguments of each of those are declared to be, each once. A declaration is
-- found by where the program says it was written, which is the statement on its page holding that
-- place most closely, written with its comments. The standard library's own declarations are left
-- out: they are the language a reader of Catln already reads, not what this type adds to it.
typeDecls :: EPrim
typeDecls = EEnvPrim "typeDecls" prim
  where
    prim Env{evTbEnv=TBEnv{tbPrgm=Prgm{prgmObjMap=ObjectMap objMap, prgmCg=ClassGraph cg}}, evPages} args = do
      tp <- typeArgOf "typeDecls" args
      let names = reachedNames S.empty (typeNames tp)
      let places = nub [start | name <- names, Just item <- [H.lookup name objMap], oa <- objItemArrs item, Just (start, _) <- [oaObj oa >>= getMetaPos . getExprMeta]]
      let trees = nub $ mapMaybe statementAt places
      -- Each without the line break a formatted page ends with, so the statements are one per line.
      return $ StrVal $ intercalate "\n" [dropWhileEnd isSpace $ formatRootPrgm (RawPrgm [] [tree]) | tree <- trees]
      where
        objItemArrs ObjectMapItem{omiDecls, omiRuntimeDecls} = omiDecls ++ omiRuntimeDecls

        -- Each name reached once: a shape, the members of a class, and what the arguments of what
        -- was declared are declared to be.
        reachedNames _ [] = []
        reachedNames seen (n:ns)
          | S.member n seen = reachedNames seen ns
          | otherwise = n : reachedNames (S.insert n seen) (ns ++ classMembers n ++ declaredArgNames n)

        classMembers n = case graphLookup (PClassName n) cg of
          Just CGClass{} -> let (_, fromVertex, toVertex) = cg in maybe [] (\v -> let (_, _, members) = fromVertex v in map fromPartialName members) (toVertex (PClassName n))
          _ -> []

        declaredArgNames n = case H.lookup n objMap of
          Just item -> concat [typeNames (getExprType obj) | oa <- objItemArrs item, Just obj <- [oaObj oa]]
          Nothing -> []

        -- Where a program says a name was declared, the statement there: on the page written at that
        -- place, the innermost statement holding it. Not one of the standard library's.
        statementAt :: SourcePos -> Maybe (RawStatementTree RawExpr ())
        statementAt at
          | isCorePath (sourceName at) = Nothing
          | otherwise = do
              EvalPage{epRaw=RawPrgm _ trees} <- H.lookup (sourceName at) evPages
              innermost at trees

        innermost at trees = listToMaybe [fromMaybe tree (innermost at subs) | tree@(RawStatementTree _ subs) <- trees, Just (start, end) <- [treeSpan tree], posKey start <= posKey at, posKey at <= posKey end]
        posKey pos = (sourceLine pos, sourceColumn pos)

-- | Where a statement and the statements under it were written.
treeSpan :: RawStatementTree RawExpr () -> Maybe (SourcePos, SourcePos)
treeSpan (RawStatementTree st subs) = case mapMaybe id (statementSpan st : map treeSpan subs) of
  []  -> Nothing
  sps -> Just (minimum $ map fst sps, maximum $ map snd sps)

-- | The names a type is made of: the name of each shape it holds, of each class it is the members of,
-- and of each type its shapes' arguments and type arguments are.
typeNames :: Type -> [Name]
typeNames (UnionType top leafs _) = concatMap partialNames (splitUnionType leafs) ++ topNames top
  where
    partialNames PartialType{ptName, ptVars, ptArgs} = ptName : concatMap typeNames (H.elems ptVars ++ H.elems ptArgs)
    topNames (Just (preds, _, _)) = case tryPredsToList preds of
      Just ps -> [ptName c | PredClass c <- ps]
      Nothing -> []
    topNames Nothing = []
typeNames (TypeVar _ _ inner) = typeNames inner
