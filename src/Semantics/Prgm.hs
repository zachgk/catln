--------------------------------------------------------------------
-- |
-- Module    :  Semantics.Prgm
-- Copyright :  (c) Zach Kimberg 2020
-- License   :  MIT
-- Maintainer:  zachary@kimberg.com
-- Stability :  experimental
-- Portability: non-portable
--
-- This module defines most of the types that make up a Catln
-- program.
--------------------------------------------------------------------

{-# LANGUAGE DeriveAnyClass            #-}
{-# LANGUAGE DeriveGeneric             #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE FlexibleInstances         #-}
{-# LANGUAGE InstanceSigs              #-}

module Semantics.Prgm where

import           Data.Hashable
import qualified Data.HashMap.Strict as H
import           GHC.Generics        (Generic)

import           Control.Monad       (join)
import           CtConstants
import           Data.Aeson          hiding (Object)
import           Data.Bifunctor      (first)
import           Data.Char           (toLower)
import           Data.List           (nub, stripPrefix)
import           Data.Maybe
import           Data.UUID
import           Data.Void           (Void)
import           Semantics.CtType
import           Semantics.TypeGraph (ReachesTree)
import           Semantics.Types
import           Semantics.Val       (Val (IntVal, StrVal, TupleVal), valArg,
                                      valStrArg)
import           Text.Megaparsec
import           Text.Printf

newtype Import = Import String
  deriving (Eq, Ord, Show)

newtype Export = Export String
  deriving (Eq, Ord, Show)

-- | The type of hole (a gap where an expression should be). Used in inputs to ignore the expression and outputs.
data Hole
  = HoleActive { haName :: Maybe Name } -- ^ A hole such as _ or _name, where the name is optional and treated as an error
  | HoleUndefined -- ^ A hole with the keyword undefined that is an error only in runtime
  | HoleTodefine -- ^ A hole with the keyword todefine that is an error during runtime and commit time
  deriving (Eq, Ord, Show, Generic, Hashable, ToJSON)

-- | A gap where an expression should be, read as a Catln type.
instance CtType Hole where
  ctDoc _ = [
    "A gap where an expression should be.",
    "A [HoleActive] is one written `_` or `_name`, which is an error wherever it is found. A [HoleUndefined] is written `undefined` and is an error only if a run reaches it. A [HoleTodefine] is written `todefine` and is an error both then and when the program is committed.",
    "The three are one type because they are one thing written three ways — a place the program is not finished — and differ only in when the compiler is entitled to complain."
            ]

-- Metadata for the Programs
type ParseErrorRes = ParseErrorBundle String Void
type CodeRangeDat = (SourcePos, SourcePos)
type CodeRange = Maybe CodeRangeDat
class (Eq m, Ord m) => MetaDat m where
  emptyMetaDat :: m
-- | Contains the type, position, and supplemental 'MetaDat' m
data Meta m = Meta {
  getMetaType :: Type,
  getMetaPos  :: CodeRange,
  getMetaId   :: UUID,
  getMetaDat  :: m
                   }
  deriving (Eq, Ord, Generic, ToJSON)

-- | What is known about a piece of a program, read as a Catln type.
--
-- Declared at the metadata every stage shares and nothing more, which is what a program handed one
-- has: 'MapMeta.clearMetaDat' is how a program of any stage is narrowed to this, and what it drops
-- is the stage's own supplement — the reaches tree an evaluation carries. So the [dat] a program
-- reads is a @\/Catln\/Unit@ rather than a lie about what is in it.
--
-- 'ctFieldName' is overridden because the fields are written @getMetaType@ and the default rule —
-- drop the leading run of lowercase — yields @metaType@. Said as the prefix it is rather than as a
-- list of the four names, so a field added to this record is named by the same rule as the others
-- rather than by someone remembering to add it here.
instance CtType (Meta ()) where
  ctPath _ = ctBuiltinNamespace ++ "Meta"
  ctFieldName _ n = case stripPrefix "getMeta" n of
    Just (c:rest) -> toLower c : rest
    _             -> ctDropFieldPrefix n
  ctDoc _ = [
    "What the compiler knows about one piece of a program: the [type] it was inferred to have, where it was written ([pos]), and the [id] that tells it apart from every other piece.",
    "[pos] is absent for a piece of the program nothing wrote — one the compiler made while desugaring, which has a type and an identity but no place in a file.",
    "[dat] is what the stage the program came from added of its own. It is handed over empty: a program is narrowed to the metadata every stage shares before it is read, so what would be there is a different thing depending on which stage was asked, and this way what is read is the same thing whichever was."
            ]

-- | Where something was written, read as a Catln type.
--
-- Written by hand rather than read off megaparsec's shape: its line and column are a @Pos@, a
-- newtype whose @mkPos@ throws below one. Read off the shape, a program could hand back a zero and
-- the conversion would raise rather than report. Numbers here, checked on the way back.
instance CtType SourcePos where
  ctPath _ = ctBuiltinNamespace ++ "SourcePos"
  ctPageInto = ctPageDeclare ctSourcePosDecl
  ctImports _ = [coreStrPage, coreNumPage]
  toCt (SourcePos name line col) = TupleVal (ctBuiltinNamespace ++ "SourcePos") $ H.fromList [
    (runtimeArgKey "file", StrVal name),
    (runtimeArgKey "line", IntVal $ toInteger $ unPos line),
    (runtimeArgKey "column", IntVal $ toInteger $ unPos col)
                                                                                             ]
  fromCt v@(TupleVal n as) | n == ctBuiltinNamespace ++ "SourcePos" = do
    name <- valStrArg n "file" as
    line <- posOf =<< valArg n "line" as
    col <- posOf =<< valArg n "column" as
    return $ SourcePos name line col
    where
      -- mkPos raises below one, so a number that is not a place in a file is reported here.
      posOf (IntVal i) | i >= 1 = Right $ mkPos $ fromInteger i
      posOf bad = Left $ printf "%s is not a line or column of %s" (show bad) (show v)
  fromCt v = Left $ printf "%s is not a %s" (show v) (ctBuiltinNamespace ++ "SourcePos")

ctSourcePosDecl :: CtDecl
ctSourcePosDecl = CtDecl (ctBuiltinNamespace ++ "SourcePos") [CtCon (ctBuiltinNamespace ++ "SourcePos") [] [
    ("file", ctRef strPrim),
    ("line", ctRef intPrim),
    ("column", ctRef intPrim)
                                                                                                          ]] [
  "A place in a file: which [file], and the [line] and [column] within it.",
  "Lines and columns count from one, as an editor shows them. A number below one is not a place and is refused when one is read back."
                                                                                                             ]

-- | MetaDat contains supplemental metadata
instance MetaDat () where
  emptyMetaDat = ()

instance Hashable m => Hashable (Meta m) where
  hashWithSalt s (Meta t _ _ d)      = s `hashWithSalt` t `hashWithSalt` d

instance MetaDat (Maybe ReachesTree) where
  emptyMetaDat = Nothing

emptyMetaT :: (MetaDat m) => Type -> Meta m
emptyMetaT t = Meta t Nothing nil emptyMetaDat

emptyMetaN :: (MetaDat m) => Meta m
emptyMetaN = emptyMetaT PTopType

mWithType :: Type -> Meta m -> Meta m
mWithType t m = m{getMetaType=t}

showCodeRange :: CodeRangeDat -> String
showCodeRange (start, end) = printf "%s:%d:%d-%d:%d" (sourceName start) (unPos $ sourceLine start) (unPos $ sourceColumn start) (unPos $ sourceLine end) (unPos $ sourceColumn end)

data EApp e m
  = EAppArg { eaaArg :: ObjArr e m }
  | EAppVar { eavName :: TypeVarName, eavMeta :: Meta m }
  | EAppSpread { easExpr :: e m }
  deriving (Eq, Ord, Show, Generic, Hashable, ToJSON)

-- | What is applied to an expression, read as a Catln type.
instance CtType (EApp Expr ()) where
  ctDoc _ = [
    "What is applied to an expression: an argument ([EAppArg]), a type parameter ([EAppVar], by [name] and the [meta] of what it is bound to), or the contents of another value spread into it ([EAppSpread]).",
    "An application carries one of these at a time. `f(a, b)` is `f` applied to `a` and then to `b`, so the three arms are what one step of that can be rather than what a call is."
            ]

-- Expr after desugar
data Expr m
  = CExpr { ceMeta :: Meta m, ceConst :: Constant }
  | Value { veMeta :: Meta m, veName :: TypeName }
  | HoleExpr { heMeta :: Meta m, heHole :: Hole }
  | AliasExpr { aeBase :: Expr m, aeAlias :: Expr m } -- ^ AliasExpr baseExpr aliasExpr
  | EWhere { ewMeta :: Meta m, ewBase :: Expr m, ewCond :: Expr m } -- ^ base cond
  | TupleApply { taMeta :: Meta m, taBase :: (Meta m, Expr m), taApp :: EApp Expr m }
  deriving (Eq, Ord, Generic, Hashable, ToJSON)

-- | An expression after desugaring, read as a Catln type.
--
-- The thing a program that rewrites programs rewrites. Everything a Catln program is written with —
-- arguments, type variables, contexts, lists, methods, format strings — has by this point become a
-- [TupleApply] of one of the others, which is why the type a macro would be written against is six
-- arms rather than the twenty the parser produces.
--
-- [TupleApply]'s [base] is handed over as a [\/Catln\/Pair]. It is a @(Meta, Expr)@ in Haskell,
-- carrying the metadata of the application beside the expression being applied to, and naming those
-- two halves would be rewriting the hundred-odd places that match one to improve a declaration.
instance CtType (Expr ()) where
  ctDoc _ = [
    "An expression, after the compiler has reduced what was written to what it means.",
    "A [CExpr] is a constant, a [Value] is a name, and a [HoleExpr] is a gap. An [AliasExpr] is a value given a second name, and an [EWhere] is a value narrowed by a condition. A [TupleApply] is everything else: one thing applied to another, which is what arguments, type parameters, lists and method calls all become.",
    "So reading an expression is reading those six, however the program was written."
            ]

-- Compiler Annotation
type CompAnnot em = em
data AFileImport em = AFileImport {
  impRaw       :: em,
  impAbs       :: em,
  impDisp      :: Maybe String,
  impCalledDir :: Maybe FilePath,
  impDir       :: Maybe FilePath
  } deriving (Generic, ToJSON)
type FileImport = AFileImport (Expr ())

instance (Eq em) => Eq (AFileImport em) where
  a == b = impAbs a == impAbs b

instance (Ord em) => Ord (AFileImport em) where
  compare a b = compare (impAbs a) (impAbs b)

instance (Hashable em) => Hashable (AFileImport em) where
  hashWithSalt s a = s `hashWithSalt` impAbs a

instance (Show em) => Show (AFileImport em) where
  show imp = show $ impAbs imp

type ExprCond e m = Maybe (e m)

data ObjectBasis = FunctionObj | TypeObj | PatternObj | MatchObj | ArgObj
  deriving (Eq, Ord, Show, Generic, Hashable, ToJSON)

-- | What an object is, read as a Catln type.
--
-- The first of the program's own types the compiler declares to Catln, and the reason it is first
-- is that it needs nothing: five constructors carrying nothing, already 'Generic', so the whole
-- instance is its name and what it says about itself.
--
-- Written HERE, beside the type, rather than anywhere above. That is what "Semantics.CtType" being
-- below the evaluator and clear of the syntax buys: a type says how it is read as a Catln one in
-- the file it is declared in, so a constructor added to it cannot fail to reach the page.
instance CtType ObjectBasis where
  ctDoc _ = [
    "What an object is: which of the ways of writing one it was written as.",
    "A [FunctionObj] is a definition or a declaration, a [TypeObj] is a `data` or a `class`, a [PatternObj] is an object matched against, a [MatchObj] is one arm of a match, and an [ArgObj] is an argument standing inside another object.",
    "It is declared by the compiler rather than in a `.ct` file because it is the compiler's own idea of what it is looking at. A program which reads programs is handed this, and what it is called here and what the compiler calls it are one thing."
            ]
data ObjArr e m = ObjArr {
  oaObj    :: !(Maybe (e m)),
  oaBasis  :: !ObjectBasis,
  oaDoc    :: !(Maybe DocComment),
  oaAnnots :: ![CompAnnot (e m)],
  oaArr    :: !(Maybe (Maybe (e m), Meta m))
                               }
  deriving (Eq, Ord, Generic, Hashable, ToJSON, ToJSONKey)


data ObjectMapItem e m = ObjectMapItem {
  omiDecls        :: [ObjArr e m],
  omiRuntimeDecls :: [ObjArr e m],
  omiRefineDecls  :: [ObjArr e m],
  omiDefs         :: [ObjArr e m]
                            }
  deriving (Eq, Ord, Show, Generic, ToJSON, ToJSONKey)
newtype ObjectMap e m = ObjectMap { omItems :: H.HashMap TypeName (ObjectMapItem e m) }
  deriving (Eq, Show, Generic, ToJSON, ToJSONKey)

-- | Everything a program declares and defines, read as a Catln type: each name, and what is written
-- under it.
instance CtType (ObjectMap Expr ()) where
  ctDoc _ = [
    "Everything a program declares and defines, by the name each is written under.",
    "Handed over as a list of pairs, in order of name, so that two programs holding the same definitions are handed over the same way however they were built."
            ]

-- | What is written under one name, read as a Catln type.
instance CtType (ObjectMapItem Expr ()) where
  ctDoc _ = [
    "Everything written under one name: its [decls], the [runtimeDecls] the compiler supplies rather than the program, the [refineDecls] narrowing what a definition returns, and the [defs] themselves.",
    "A declaration says what may be called and a definition says what happens when it is. A name may have several of each, which is how one operation is written once and implemented per type."
            ]

-- | An object and the arrow from it, read as a Catln type.
instance CtType (ObjArr Expr ()) where
  ctDoc _ = [
    "One thing written in a program: an [obj] it matches, and the [arr] it becomes.",
    "[basis] says which way it was written — a definition, a type, a pattern, an arm of a match, or an argument standing inside another. [doc] is what was written about it and [annots] are the annotations on it.",
    "[arr] is absent for something that only matches and produces nothing. When present it is a [Pair] of what it becomes — absent for a declaration, which says the shape without giving the value — and the [Meta] of that."
            ]
data Prgm e m = Prgm {
  prgmObjMap :: ObjectMap e m,
  prgmCg     :: ClassGraph,
  prgmAnnots :: [CompAnnot (e m)]
  -- TODO: Include [Export]
                     }
  deriving (Show, Generic, ToJSON, ToJSONKey)

-- | A whole program, read as a Catln type. What all of this was for.
instance CtType (Prgm Expr ()) where
  ctDoc _ = [
    "A whole program: everything it declares and defines ([objMap]), how its classes and types relate ([cg]), and the annotations written on the program itself ([annots]).",
    "This is what a program reading a program is given. It is the program at whichever stage it was asked for, narrowed to the metadata every stage shares, so what is read is the same shape whichever stage it came from."
            ]

instance (Show m) => Show (Meta m) where
  show :: Meta m -> String
  show (Meta t _ _ _) = show t
  -- show (Meta t p d) = printf "(Meta %s %s (%s))" (show t) (show p) (show d)

-- | Configuration controlling how program elements are shown. It allows a
-- caller (such as the type-inference trace) to override how each metadata
-- annotation is rendered and to inject an inline annotation (e.g. a point id
-- and its final type) at every metadata site. 'defaultShowConfig' reproduces
-- the plain 'Show' output exactly.
data ShowConfig m = ShowConfig
  { scShowMeta :: Meta m -> String -- ^ renders a meta where a type is already displayed
  , scAnnot    :: Meta m -> String -- ^ inline suffix appended at every meta site
  }

defaultShowConfig :: Show m => ShowConfig m
defaultShowConfig = ShowConfig show (const "")

showExprWithConfig :: Show m => ShowConfig m -> Expr m -> String
showExprWithConfig cfg = go
  where
    go (CExpr m c) = printf "%s%s" (show c) (scAnnot cfg m)
    go (Value m name) = printf "Value %s%s" (nameStr name) (scAnnot cfg m)
    go (HoleExpr m hole) = printf "Hole %s %s%s" (scShowMeta cfg m) (show hole) (scAnnot cfg m)
    go (AliasExpr base alias) = printf "%s@%s" (go base) (go alias)
    go (EWhere m base cond) = printf "%s | %s%s" (go base) (go cond) (scAnnot cfg m)
    go (TupleApply m (_, baseExpr) arg) = printf "%s%s%s" baseExpr' (showEAppWithConfig cfg arg) (scAnnot cfg m)
      where
        baseExpr' = case baseExpr of
          Value vm funName -> printf "%s%s" (nameStr funName) (scAnnot cfg vm)
          TupleApply{}     -> go baseExpr
          _                -> printf "(%s)" (go baseExpr)

showEAppWithConfig :: Show m => ShowConfig m -> EApp Expr m -> String
showEAppWithConfig cfg arg = case arg of
  EAppArg a    -> printf "(%s)" (showObjArrWithConfig cfg a)
  EAppVar a v  -> printf "[%s : %s]" (show a) (scShowMeta cfg v)
  EAppSpread a -> printf "(..%s)" (showExprWithConfig cfg a)

showObjArrWithConfig :: Show m => ShowConfig m -> ObjArr Expr m -> String
showObjArrWithConfig cfg ObjArr{oaObj, oaArr} = printf "%s%s" objStr showArr
  where
    objStr = maybe "" (showExprWithConfig cfg) oaObj
    showArr :: String
    showArr = case oaArr of
      Just (Just e, m) | getMetaType m /= PTopType -> printf " -> %s = %s" (scShowMeta cfg m) (showExprWithConfig cfg e)
      Just (Just e, _) | isJust oaObj -> printf "= %s" (showExprWithConfig cfg e)
      Just (Just e, _) -> showExprWithConfig cfg e
      Just (Nothing, m) | getMetaType m /= PTopType -> printf " -> %s" (scShowMeta cfg m)
      Just (Nothing, _) -> ""
      Nothing -> ""

instance Show m => Show (Expr m) where
  show = showExprWithConfig defaultShowConfig

instance (Show m, Show (e m)) => Show (ObjArr e m) where
  show ObjArr{oaObj, oaArr} = printf "%s%s" (showNoMaybe oaObj) showArr
    where
      showNoMaybe :: (Show a) => Maybe a -> String
      showNoMaybe (Just a) = show a
      showNoMaybe Nothing  = ""

      showArr :: String
      showArr = case oaArr of
        Just (Just e, m) | getMetaType m /= PTopType -> printf " -> %s = %s" (show m) (show e)
        Just (Just e, _) | isJust oaObj -> printf "= %s" (show e)
        Just (Just e, _) -> show e
        Just (Nothing, m) | getMetaType m /= PTopType -> printf " -> %s" (show m)
        Just (Nothing, _) -> ""
        Nothing -> ""

instance Hashable SourcePos where
  hashWithSalt s (SourcePos name line col) = s `hashWithSalt` show name `hashWithSalt` unPos line `hashWithSalt` unPos col

instance ToJSON SourcePos where
  toJSON (SourcePos name line col) = object ["name".=name, "line".=unPos line, "col".=unPos col]

type VarArgMap e m = H.HashMap TypeVarAux [(e m, Meta m)]

instance Semigroup (ObjectMapItem e m) where
  (ObjectMapItem decls1 runtimeDecls1 refineDecls1 defs1) <> (ObjectMapItem decls2 runtimeDecls2 refineDecls2 defs2) = ObjectMapItem (decls1 ++ decls2) (runtimeDecls1 ++ runtimeDecls2) (refineDecls1 ++ refineDecls2) (defs1 ++ defs2)

instance Monoid (ObjectMapItem e m) where
  mempty = ObjectMapItem [] [] [] []

instance Semigroup (ObjectMap e m) where
  (ObjectMap map1) <> (ObjectMap map2) = ObjectMap $ H.unionWith merge map1 map2
    where
      merge (ObjectMapItem decl1 runtimeDecl1 refineDecl1 def1) (ObjectMapItem decl2 runtimeDecl2 refineDecl2 def2) = ObjectMapItem (decl1 ++ decl2) (runtimeDecl1 ++ runtimeDecl2) (refineDecl1 ++ refineDecl2) (def1 ++ def2)

instance Monoid (ObjectMap e m) where
  mempty = ObjectMap H.empty

instance Semigroup (Prgm e m) where
  (<>) :: Prgm e m -> Prgm e m -> Prgm e m
  (Prgm objMap1 classGraph1 annots1) <> (Prgm objMap2 classGraph2 annots2) = Prgm
    (objMap1 <> objMap2)
    (classGraph1 <> classGraph2)
    (annots1 <> annots2)

instance Monoid (Prgm e m) where
  mempty = Prgm mempty mempty []

-- | Represents the VarArgMap by extracting information from a partial
type ArgMetaMapWithSrcItem m = ([(Expr m, Meta m)], Type)
type ArgMetaMapWithSrc m = H.HashMap TypeVarAux ([(Expr m, Meta m)], Type)

-- | Represents multiple possible 'ArgMetaMapWithSrc' that can be achieved by different matches (can choose with match)
type PossArgMetaMapWithSrc m = [ArgMetaMapWithSrc m]

class ExprClass e where
  -- | Returns the metadata for the top level of an expression
  getExprMeta ::  e m -> Meta m

  -- | Returns the metadata for the top level of an expression
  setExprMeta :: e m -> Meta m -> e m

  -- | Returns the value at the base of an expression, if it exists
  maybeExprPathM :: e m -> Maybe (TypeName, Meta m)

  -- | Returns all arguments applied to a value
  exprAppliedArgs :: (Show m, MetaDat m) => e m -> [ObjArr e m]

  -- | Returns all vars applied to a value
  exprAppliedOrdVars :: (Show m, MetaDat m) => e m -> [(TypeVarName, Meta m)]

  -- | Returns all arguments located recursively in an expression
  exprVarArgs :: (MetaDat m, Show m) => e m -> VarArgMap e m

  -- | The 'exprVarArgsWithSrc' is similar to the 'exprArgs' function.
  -- | It differs in that it accepts an additional partial type that is equivalent to the expression and it pull the corresponding parts of the partial matching the args in the expression
  exprVarArgsWithSrc :: (TypeGraph tg, MetaDat m, Show m, Show (e m)) => TypeEnv tg -> e m -> PartialType -> PossArgMetaMapWithSrc m

  mkValue :: (MetaDat m) => Name -> e m


instance ExprClass Expr where
  getExprMeta expr = case expr of
    CExpr m _        -> m
    Value m _        -> m
    HoleExpr m _     -> m
    AliasExpr b _    -> getExprMeta b
    TupleApply m _ _ -> m
    EWhere m _ _     -> m

  setExprMeta expr m' = case expr of
    CExpr _ c         -> CExpr m' c
    Value _ v         -> Value m' v
    HoleExpr _ v      -> HoleExpr m' v
    AliasExpr b a     -> AliasExpr (setExprMeta b m') a
    TupleApply _ b as -> TupleApply m' b as
    EWhere _ b c      -> EWhere m' b c

  maybeExprPathM (Value m n)             = Just (n, m)
  maybeExprPathM (TupleApply _ (_, e) _) = maybeExprPathM e
  maybeExprPathM (AliasExpr b _)         = maybeExprPathM b
  maybeExprPathM (EWhere _ b _)          = maybeExprPathM b
  maybeExprPathM _                       = Nothing

  exprAppliedArgs (Value _ _)               = []
  exprAppliedArgs CExpr{}                   = []
  exprAppliedArgs HoleExpr{}                = []
  exprAppliedArgs (TupleApply _ (_, be) (EAppArg ae)) = ae : exprAppliedArgs be
  exprAppliedArgs (TupleApply _ (_, be) EAppVar{}) = exprAppliedArgs be
  exprAppliedArgs (TupleApply _ (_, be) (EAppSpread ae)) = exprAppliedArgs ae ++ exprAppliedArgs be
  exprAppliedArgs (AliasExpr b _)           = exprAppliedArgs b
  exprAppliedArgs (EWhere _ b _)              = exprAppliedArgs b

  exprAppliedOrdVars (Value _ _)              = []
  exprAppliedOrdVars CExpr{}                  = []
  exprAppliedOrdVars HoleExpr{}               = []
  exprAppliedOrdVars (TupleApply _ (_, be) (EAppVar n m)) = (n, m) : exprAppliedOrdVars be
  exprAppliedOrdVars (TupleApply _ (_, be) _) = exprAppliedOrdVars be
  exprAppliedOrdVars (AliasExpr b _)          = exprAppliedOrdVars b
  exprAppliedOrdVars (EWhere _ b _)           = exprAppliedOrdVars b

  exprVarArgs CExpr{} = H.empty
  exprVarArgs Value{} = H.empty
  exprVarArgs HoleExpr{} = H.empty
  exprVarArgs (AliasExpr base n) = H.insertWith (++) (TVArg $ inExprSingleton n) [(n, getExprMeta base)] (exprVarArgs base)
  exprVarArgs (EWhere _ base _) = exprVarArgs base
  -- For an alias destructure (@field@name@) the arg's value is matched by the field but bound
  -- under the alias name (the name the body references), so key the field's arr-meta by the alias
  -- and recurse into the base — mirroring 'exprVarArgsWithSrc'. A bare binding uses the obj itself.
  exprVarArgs (TupleApply _ (_, be) (EAppArg ObjArr{oaObj=Just n, oaArr=Just (Nothing, arrM)})) = H.insertWith (++) (TVArg $ inExprSingleton bindName) [(n, arrM)] (H.unionWith (++) (exprVarArgs be) (exprVarArgs baseN))
    where (bindName, baseN) = case n of
            AliasExpr b a -> (a, b)
            _             -> (n, n)
  exprVarArgs (TupleApply _ _ (EAppArg ObjArr{oaObj, oaArr=Just (Nothing, _)})) = error $ printf "Unexpected unhandled obj type in exprVarArgs: %s" (show oaObj)
  exprVarArgs (TupleApply _ (_, be) (EAppArg ObjArr{oaArr=Just (Just e, _)})) = H.unionWith (++) (exprVarArgs be) (exprVarArgs e)
  exprVarArgs (TupleApply _ (_, be) (EAppVar n m)) = H.insertWith (++) (TVVar n) [(Value (emptyMetaT $ partialToTypeSingleton n) (pkName n), m)] (exprVarArgs be)
  exprVarArgs (TupleApply _ _ (EAppArg ObjArr{oaObj, oaArr=Nothing})) = error $ printf "Not yet implemented: %s" (show oaObj)
  exprVarArgs (TupleApply _ (_, be) (EAppSpread arg)) = H.unionWith (++) (exprVarArgs arg) (exprVarArgs be)

  exprVarArgsWithSrc typeEnv expr src = aux expr
    where
      aux CExpr{} = [H.empty]
      aux Value{} = [H.empty]
      aux HoleExpr{} = [H.empty]
      aux (EWhere _ base cond) = case maybeExprPath cond of
        Just n | nameStr n == operatorHasArrow -> case (H.lookup (partialKey (mkName operatorArgL)) condArgs, H.lookup (partialKey (mkName operatorArgR)) condArgs) of
                  (Just (Just (_, Just l)), Just (Just (_, Just r))) -> case (getExprType $ exprPropagateTypes l, getExprType $ exprPropagateTypes r) of
                    (lt@(UnionType Nothing _ _), rt) | isJust (maybeGetSingleton lt) -> concatMap findArrowMatches bases'
                      where
                        findArrowMatches base' = case typeGraphQuery typeEnv (fmap snd base') $ getSingleton lt of
                          [] -> [H.empty]
                          qr -> let fromEachArrow = fmap (\qrt -> fst $ intersectTypesWithVarEnv typeEnv (fmap snd base') qrt rt) qr
                                in joinPossVarArgMetaWithSrc typeEnv (map (fmap ([],)) fromEachArrow) [base']
                    _ -> [H.empty]
                  _ -> error $ printf "Could not find /l and /r in %s in ?->" (show condArgs)
        _ -> bases'
        where
          bases' = exprVarArgsWithSrc typeEnv base src
          condArgs = exprAppliedArgsMap cond
      aux (AliasExpr base n) = map (H.insertWith (mergeVarArgMetaWithSrcItem typeEnv) (TVArg $ inExprSingleton n) ([(n, getExprMeta base)], singletonType src)) (exprVarArgsWithSrc typeEnv base src)
      aux (TupleApply _ (_, be) arg) = joinPossVarArgMetaWithSrc typeEnv (exprVarArgsWithSrc typeEnv be src) (fromArg arg)
        where
          fromArg :: (MetaDat m, Show m) => EApp Expr m -> [ArgMetaMapWithSrc m]
          fromArg (EAppArg ObjArr{oaObj=Just obj, oaArr=Just (Just e, _)}) = case typeGetArgLoose (inExprSingleton obj) src of
            Just (UnionType Nothing srcArg []) -> joinAllPossVarArgMetaWithSrc typeEnv $ map (exprVarArgsWithSrc typeEnv e) $ splitUnionType srcArg
            Just t@(UnionType (Just _) _ _) -> [(,t) <$> exprVarArgs e]
            _ -> [H.empty]
          fromArg (EAppArg ObjArr{oaArr=Just (Just e, _)}) = exprVarArgsWithSrc typeEnv e src
          fromArg (EAppArg ObjArr {oaObj=Just obj, oaArr=Just (Nothing, arrM)})
            -- A destructuring pattern (e.g. @Box(v)@) whose head does not name an argument of
            -- the source (so it is a data type, not a higher-order arg like @f(y)@): reconcile
            -- it to the source argument it fills by type (the only one whose declared type the
            -- pattern's head names), then match the pattern against that argument's type to bind
            -- its inner variables (e.g. @v@). The head is compared syntactically ('exprPath')
            -- because 'inExprSingleton' resolves via the expression's type in later phases.
            | headName `notElem` srcArgNames && not (H.null (exprAppliedArgsMap obj))
            , Just argKey <- reconciledKey
            , Just (UnionType Nothing srcArg []) <- typeGetArg argKey src
            = joinAllPossVarArgMetaWithSrc typeEnv $ map (exprVarArgsWithSrc typeEnv obj) $ splitUnionType srcArg
            | otherwise = case getMetaType arrM of
                (TypeVar v _ _) -> [H.insertWith (mergeVarArgMetaWithSrcItem typeEnv) v ([(obj, arrM)], fromMaybe PTopType $ typeGetArgLoose structKey src) mainArg]
                -- A declared argument spelled as a class application linking the object's
                -- variables (@lst -> List[/T=$_/T]@) BINDS those variables from the source
                -- content's linked dimension (@lst=Cons[/T=Integer](…)@ binds @/T=Integer@) —
                -- the argument position is a linked occurrence of the variable, so the content's
                -- element evidence is the variable's per-call value. Without this only an
                -- explicit application (@sorted[/T=Integer](…)@) could bind, and a declaration's
                -- linked result (@-> List[/T=$_/T]@) always presented at the variable's declared
                -- bound instead of the call's element type.
                declT -> [foldr insertLinked mainArg (linkedDimBinds typeEnv declT (fromMaybe PTopType (typeGetArgLoose structKey src)))]
                  where insertLinked (v, d) = H.insertWith (mergeVarArgMetaWithSrcItem typeEnv) v ([(obj, arrM)], d)
            where
              structKey = inExprSingleton obj
              -- For an alias destructure (@field@name@, e.g. @first@f@) the matched field is
              -- named by the base (@first@, so 'structKey' looks up its value in 'src') but is
              -- /bound/ under the alias (@f@) — that is the name the body references. Bind under
              -- the alias key so the body resolves; a bare binding (no alias) uses 'structKey'.
              bindKey = case obj of
                AliasExpr _ aliasE -> inExprSingleton aliasE
                _                  -> structKey
              headName = makeAbsoluteName (exprPath obj)
              srcArgNames = map (makeAbsoluteName . pkName) (H.keys (ptArgs src))
              reconciledKey = case filter (patternHeadNamesArg . snd) (H.toList (ptArgs src)) of
                [(argName, _)] -> Just argName
                _              -> Nothing
              patternHeadNamesArg argType = case maybeGetSingleton argType of
                Just p  -> headName == makeAbsoluteName (ptName p)
                Nothing -> False
              -- The declared-meta fallback must also cover a source slot that resolves to ⊤:
              -- an upper-unbounded source ('typeGetArg' on a @..@-mode partial) reports an
              -- absent argument as @Just ⊤@ ("may still be present"), which is no information —
              -- while the value taking this arrow necessarily matches the object, so its
              -- content lies within the declared meta (declaration-boundedness). Without this
              -- the desugared @main@ wrapper's @TVArg /io@ return resolves to ⊤ against the
              -- generic @/ContextIn[$V](value=$V)..@ hop and TreeBuild reports
              -- "Arrow has indeterminate return type".
              mainArg = H.singleton (TVArg bindKey) ([(obj, arrM)],
                case typeGetArgLoose structKey src of
                  Just t | t /= PTopType -> t
                  _                      -> getMetaType arrM)
          fromArg (EAppVar vn vm) = [H.singleton (TVVar vn) ([(Value (emptyMetaT $ partialToTypeSingleton vn) (pkName vn), vm)], H.lookupDefault PTopType vn (ptVars src))]
          fromArg (EAppSpread s) = exprVarArgsWithSrc typeEnv s src
          fromArg oa = error $ printf "Invalid oa %s" (show oa)


  mkValue n = Value (emptyMetaT $ typeVal n) n

class ObjArrClass oa where
  -- | See exprArgs
  oaVarArgs :: (ExprClass e, MetaDat m, Show m, Show (e m)) => oa e m -> H.HashMap TypeVarAux [(e m, Meta m)]

  getOaAnnots :: oa e m -> [CompAnnot (e m)]

instance ObjArrClass ObjArr where

  oaVarArgs oa = exprArg oa
    where
      exprArg ObjArr{oaArr=Just (Just e, _)} = exprVarArgs e
      exprArg ObjArr{oaObj=Just obj, oaArr=Just (_, m)} = H.singleton (TVArg $ inExprSingleton obj) [(obj, m)]
      exprArg _ = error $ printf "Invalid oa %s" (show oa)

  getOaAnnots = oaAnnots

-- | The variable bindings a declared argument's LINKED spelling extracts from the source
-- content: for a declared type that is a pure class application whose parameters link the
-- object's variables (@List[/T=$_/T]@ — 'TVExt'), each linked variable binds to the JOIN of the
-- content's dimensions at the class parameter (@Cons[/T=Integer](…)@ yields @/T=Integer@; a bare
-- atom member like @Nil@ pins nothing). Strictly conservative — the same projection discipline
-- as the declaration-bounded dim tightening in "Semantics.Types": var-carrying dimensions,
-- unknown content shapes, or a ⊤ join produce no binding (a ⊤ entry carries no information).
linkedDimBinds :: (TypeGraph tg) => TypeEnv tg -> Type -> Type -> [(TypeVarAux, Type)]
linkedDimBinds typeEnv declT content = case declT of
  UnionType (Just (PredsOne (PredClass q), negs, [])) pos []
    | H.null negs, H.null pos, H.null (ptArgs q)
    -> fromTemplate q
  -- A declared DATA spelling whose parameters link the object's variables
  -- (@b -> Box[$T -> $_T]@) binds them the same way as the class-application form: the
  -- content's dimension at the parameter is the variable's per-call value
  -- (@b= Box[/T=Integer](…)@ yields @/T=Integer@). Single-leaf declared spellings only —
  -- the same strictly conservative projection.
  UnionType Nothing declLeafs []
    | [q] <- splitUnionType declLeafs
    -> fromTemplate q
  _ -> []
  where
    -- The template's var-slot reference may be spelled 'TVExt' (@List[/T=$_/T]@) or 'TVInt'
    -- (@Box[/T=$/U]@ from an explicit-parameter declaration @unwrap[$U](b -> Box[$T -> $U])@ —
    -- the parser spells @$U@ 'TVInt' context-free, so the reference lands internal-spelled
    -- inside the nested partial's slot). Both denote the declaring object's variable:
    -- 'substituteTVExt' already reads a var-slot value against the ambient env regardless of
    -- loc, so the linked-occurrence read here must bind under the same rule or the two
    -- surfaces disagree (the explicit spelling's binding was silently dropped, leaving the
    -- declaration route at ⊤ and the call unreduced at eval).
    fromTemplate q =
      [ (TVVar k, dim)
      | (k', TypeVar (TVVar k) _ _) <- H.toList (ptVars q)
      , Just dim <- [contentDim (ptName q) k']
      , dim /= PTopType ]
    -- The join is over the leaves that CARRY the dimension. A bare atom member (@Nil@, @Leaf@)
    -- has no parameters, so it says nothing about the variable rather than saying it is empty:
    -- read as ∅ it would be a binding, and a binding to ∅ makes every occurrence of the variable
    -- uninhabited — an arm's field built against a @$MV = ∅@ destination has an empty raw call
    -- source, which is vacuously already at the destination, so the call is never rewritten and
    -- flows through as a runtime value. Content made only of atoms binds nothing.
    contentDim qName k' = case content of
      UnionType Nothing leafs [] -> do
        dims <- mapM (leafDim k') (splitUnionType leafs)
        case catMaybes dims of
          [] -> Nothing
          ds -> Just (unionAllTypes typeEnv ds)
      UnionType (Just (PredsOne (PredClass q'), negs, [])) pos []
        | H.null negs, H.null pos -> join (leafDim k' q')
      -- A conjunction of class predicates (@⊤ | Map[/K=Integer] ∧ BinTree[/K=Integer]@ — a value
      -- known both by the declaration it was handed to and by the arrow that produced it) is
      -- read at the conjunct spelling the TEMPLATE's own class; the other conjuncts are the same
      -- value's other classes, and their dimensions are its dimensions under different names.
      UnionType (Just (PredsAnd conjs, negs, [])) pos []
        | H.null negs, H.null pos
        , [q'] <- [ c | PredsOne (PredClass c) <- conjs, ptName c == qName ]
        -> join (leafDim k' q')
      _ -> Nothing
    -- @Nothing@: a shape the projection does not read (no binding at all); @Just Nothing@: an atom,
    -- which carries no dimension; @Just (Just d)@: the leaf's dimension.
    leafDim k' l = case H.lookup k' (ptVars l) of
      Just e | not (typeHasVars e) -> Just (Just e)
             | otherwise -> Nothing
      Nothing | H.null (ptVars l) && H.null (ptArgs l) -> Just Nothing
              | otherwise -> Nothing

mergeVarArgMetaWithSrcItem :: (TypeGraph tg) => TypeEnv tg -> ([(Expr m, Meta m)], Type) -> ([(Expr m, Meta m)], Type) -> ([(Expr m, Meta m)], Type)
mergeVarArgMetaWithSrcItem typeEnv (m1s, t1) (m2s, t2) = (m1s ++ m2s, intersectTypes typeEnv t1 t2)

joinPossVarArgMetaWithSrc :: (TypeGraph tg) => TypeEnv tg -> PossArgMetaMapWithSrc m -> PossArgMetaMapWithSrc m -> PossArgMetaMapWithSrc m
joinPossVarArgMetaWithSrc typeEnv aVarArgMaps bVarArgMaps = [H.unionWith (mergeVarArgMetaWithSrcItem typeEnv) aVarArgMap bVarArgMap | aVarArgMap <- aVarArgMaps, bVarArgMap <- bVarArgMaps]

joinAllPossVarArgMetaWithSrc :: (TypeGraph tg) => TypeEnv tg -> [PossArgMetaMapWithSrc m] -> PossArgMetaMapWithSrc m
joinAllPossVarArgMetaWithSrc _ []     = [H.empty]
joinAllPossVarArgMetaWithSrc typeEnv (x:xs) = foldr (joinPossVarArgMetaWithSrc typeEnv) x xs

-- | The /non-linear/ argument groups of an input pattern: each group is the set of argument
-- field references that a definition binds to a single shared variable in two or more positions
-- (e.g. @f(a@x, b@x)@ binds both @a@ and @b@ to @x@, yielding @[[a, b]]@). The positions of a
-- group must hold /equal/ values for the clause to apply — the runtime equality check is built
-- from this (see 'TreeBuild'). Each entry pairs the field reference with its arrow meta (the
-- declared type of that position). Linear patterns (every variable used once) yield @[]@.
-- | The /non-linear/ argument groups of an input pattern: each group is the set of argument
-- positions a definition binds to a single shared variable in two or more places (e.g.
-- @f(a@x, b@x)@). The positions of a group must hold /equal/ values for the clause to apply — the
-- runtime equality check is built from this (see 'TreeBuild'/'Eval'). Each entry pairs the
-- argument's /key reference/ (whose 'exprPath' names the input field to read — @a@/@b@ for a named
-- function, @/l@/@/r@ for an operator) with that position's arrow meta.
--
-- A position counts only when it is an /alias/ binding (@\@x@): @a@x@ (the alias is on the argument
-- itself, @oaObj@) or @/l= _@x@ from @(_@x op _@x)@ (the alias is nested in the argument's value).
-- A bare value pattern such as @boolEq(l= False, r= False)@ is a constant /match/, not a binding,
-- so it is never grouped — treating @False@ as a shared variable would put a spurious equality
-- guard on such clauses. Linear patterns yield @[]@.
exprNonLinearArgGroups :: (MetaDat m, Show m) => Expr m -> [[(Expr m, Meta m)]]
exprNonLinearArgGroups e =
  [ [ entry | (v', entry) <- argInfos, v' == v ]
  | v <- nub [v | (v, _) <- argInfos]
  , length [() | (v', _) <- argInfos, v' == v] >= 2
  ]
  where
    argInfos = mapMaybe argInfo (exprAppliedArgs e)
    -- (boundVarName, (argKeyRef, argMeta)) for an /aliased/ argument; 'Nothing' otherwise.
    argInfo oa = case (oaObj oa, oaArr oa) of
      (Just (AliasExpr base alias), Just (Nothing, m)) -> (, (base, m)) <$> maybeExprPath alias
      (Just o, Just (Just (AliasExpr _ alias), m))     -> (, (o, m))    <$> maybeExprPath alias
      _                                                -> Nothing

mkIOObjArr :: (MetaDat m, Show m) => Meta m -> ArgName -> Expr m -> ObjArr Expr m
mkIOObjArr m argName argVal = ObjArr (Just (Value (emptyMetaT $ partialToTypeSingleton argName) (pkName argName))) ArgObj Nothing [] (Just (Just argVal, m))

mkIObjArr :: (MetaDat m, Show m) => Meta m -> ArgName -> ObjArr Expr m
mkIObjArr m argName = ObjArr (Just (Value (emptyMetaT $ partialToTypeSingleton argName) (pkName argName))) ArgObj Nothing [] (Just (Nothing, m))

mkOObjArr :: (MetaDat m, Show m) => Expr m -> ObjArr Expr m
mkOObjArr argVal = ObjArr Nothing ArgObj Nothing [] (Just (Just argVal, emptyMetaN ))


mapMetaDat :: (m1 -> m2) -> Meta m1 -> Meta m2
mapMetaDat f m@Meta{getMetaDat} = m{getMetaDat=f getMetaDat}

-- | Maps the objArr.oaArr.expr
mapTupleArgValue :: (e m -> e m) -> ObjArr e m -> ObjArr e m
mapTupleArgValue f oa@ObjArr{oaArr} = oa{oaArr=fmap (first (fmap f)) oaArr}

exprAppliedVars :: (ExprClass e, Show m, MetaDat m) => e m -> H.HashMap TypeVarName (Meta m)
exprAppliedVars = H.fromList . exprAppliedOrdVars

getExprType :: (ExprClass e) => e m -> Type
getExprType = getMetaType . getExprMeta

inExprSingleton :: (ExprClass e, MetaDat m, Show m, Show (e m)) => e m -> ArgName
inExprSingleton e = maybe
  (partialKey $ makeAbsoluteName $ exprPath e) {pkArgs = H.keysSet $ exprAppliedArgsMap e, pkVars = H.keysSet $ exprAppliedVars e} partialToKey (maybeGetSingleton (getExprType e))

maybeExprPath :: (ExprClass e) => e m -> Maybe TypeName
maybeExprPath = fmap fst . maybeExprPathM

exprPathM :: (ExprClass e) => e m -> (TypeName, Meta m)
exprPathM = fromMaybe (error "No exprPath found") . maybeExprPathM

exprPath :: (ExprClass e) => e m -> TypeName
exprPath = fst . exprPathM

exprArgs :: (ExprClass e, MetaDat m, Show m) => e m -> H.HashMap ArgName [(e m, Meta m)]
exprArgs e = H.fromList $ mapMaybe aux $ H.toList $ exprVarArgs e
  where
    aux (TVArg a, ms)      = Just (a, ms)
    aux (TVVar _, _)       = Nothing
    aux (TVApplied _ _, _) = Nothing

exprAppliedArgsMap :: (ExprClass e, MetaDat m, Show m, Show (e m)) => e m -> H.HashMap ArgName (Maybe (Meta m, Maybe (e m)))
exprAppliedArgsMap = H.fromList . mapMaybe mapArg . exprAppliedArgs
  where
    mapArg ObjArr{oaObj=Just oe, oaArr=Just (arrExpr, oaM)} = Just (inExprSingleton oe, Just (oaM, arrExpr))
    mapArg ObjArr{oaObj=Just oe, oaArr=Nothing} = Just (inExprSingleton oe, Nothing)
    mapArg _ = Nothing

oaObjExpr :: (MetaDat m, ExprClass e, Show m, Show (e m)) => ObjArr e m -> e m
oaObjExpr ObjArr{oaObj=Just e} = e
oaObjExpr oa = error $ printf "oaObjExpr with no input expression: %s" (show oa)


oaObjPath :: (MetaDat m, ExprClass e, Show m, Show (e m)) => ObjArr e m -> TypeName
oaObjPath = exprPath . oaObjExpr

-- | Whether a declaration declares a value rather than a type.
--
-- Both are written the same way and both are 'omiDecls' entries with no arrow, so the two are told
-- apart by what their arguments are given: @Task(contents= "Read it", state= False)@ gives each of
-- its arguments an expression and is the task it names, while @Task(contents -> String, state ->
-- Boolean)@ gives each of them a type and is the declaration of what a task is.
--
-- An object applying no arguments at all gives nothing either way, and a bare name is only ever the
-- declaration of a type, so it is not one. That matters: the standard library declares many of them
-- and several are displayable, so reading a bare name as a value would have the compiler evaluate
-- @String@, @True@ or @#typeInfer@ as though each were an instance of itself.
isValueObjArr :: (MetaDat m, ExprClass e, Show m, Show (e m)) => ObjArr e m -> Bool
isValueObjArr ObjArr{oaArr=Just _} = False
isValueObjArr ObjArr{oaObj=Nothing} = False
isValueObjArr oa = case exprAppliedArgs $ oaObjExpr oa of
  []   -> False
  args -> all givenAValue args
  where
    givenAValue ObjArr{oaArr=Just (Just _, _)} = True
    givenAValue _                              = False

getOaArrExpr :: (MetaDat m, ExprClass e, Show m, Show (e m)) => ObjArr e m -> e m
getOaArrExpr ObjArr{oaArr=Just (Just e, _)} = e
getOaArrExpr oa = error $ printf "oaArrExpr with no output expression: %s" (show oa)

getOaArrM :: (MetaDat m, ExprClass e, Show m, Show (e m)) => ObjArr e m -> Meta m
getOaArrM ObjArr{oaArr=Just (_, m)} = m
getOaArrM oa = error $ printf "getOaArrM with no output meta: %s" (show oa)


varNamesWithPrefix :: Name -> [Name]
varNamesWithPrefix n = [n, makeAbsoluteName (mkName ('$' : tail (nameStr n)))]

nullObjectMap :: ObjectMap e m -> Bool
nullObjectMap (ObjectMap objMap) = H.null objMap

flatObjectMapItem :: ObjectMapItem e m -> [ObjArr e m]
flatObjectMapItem (ObjectMapItem decls runtimeDecls refineDecls defs) = decls ++ runtimeDecls ++ refineDecls ++ defs

flatObjectMap :: ObjectMap e m -> [ObjArr e m]
flatObjectMap (ObjectMap objMap) = concatMap flatObjectMapItem (H.elems objMap)

-- | Whether an object arrow carries the given annotation, by its ABSOLUTE path.
--
-- The annotation path may still be relative (@#refine@\/@#runtime@) before name resolution or
-- already resolved to the absolute @\/Catln\/#refine@, so match either form. Every reader that
-- classifies an object by an annotation must ask this way: an exact compare against the absolute
-- name silently answers 'False' for every not-yet-resolved object, and the classification then
-- disagrees with the one 'singletonObjectMap' made (a runtime declaration read as a plain
-- declaration keeps declaration precedence and so enters the object universe beside the real
-- declaration, where it bounds itself and erases the declaration's type parameters).
oaHasAnnotRel :: (ExprClass e) => String -> ObjArr e m -> Bool
oaHasAnnotRel name oa = any (\a -> relativeNameMatches (exprPath a) (mkName name)) (oaAnnots oa)

-- | Whether an object arrow is a runtime declaration (a declaration whose body is supplied by the
-- evaluator runtime). It is DEFINITION-like: it needs no separate definition to fulfill a
-- declaration, and it must not widen the object universe the way a declaration does.
isRuntimeDecl :: (ExprClass e) => ObjArr e m -> Bool
isRuntimeDecl oa@ObjArr{oaArr=Just (Nothing, _)} = oaHasAnnotRel runtimeAnnot oa
isRuntimeDecl _                                  = False

singletonObjectMap :: (ExprClass e, MetaDat m, Show (e m), Show m) => ObjArr e m -> ObjectMap e m
singletonObjectMap oa = ObjectMap $ H.singleton (makeAbsoluteName $ oaObjPath oa) omi
  where
    isRefine = oaHasAnnotRel refineAnnot oa
    isRuntime = oaHasAnnotRel runtimeAnnot oa
    omi = case oa of
      ObjArr{oaArr=Nothing}            | isRefine  -> ObjectMapItem [] [] [oa] [] -- refine type object
      ObjArr{oaArr=Nothing}                        -> ObjectMapItem [oa] [] [] [] -- type object
      ObjArr{oaArr=Just (Nothing, _)}  | isRefine  -> ObjectMapItem [] [] [oa] [] -- refine declaration
      ObjArr{oaArr=Just (Nothing, _)}  | isRuntime -> ObjectMapItem [] [oa] [] [] -- runtime declaration
      ObjArr{oaArr=Just (Nothing, _)}              -> ObjectMapItem [oa] [] [] [] -- declaration
      _                                            -> ObjectMapItem [] [] [] [oa] -- definition

objectMapFromList :: (ExprClass e, MetaDat m, Show (e m), Show m) => [ObjArr e m] -> ObjectMap e m
objectMapFromList objMap = mconcat $ map singletonObjectMap objMap

mapMObjectMap :: Monad m => (ObjArr e meta1 -> m (ObjArr e meta2)) -> ObjectMap e meta1 -> m (ObjectMap e meta2)
mapMObjectMap f (ObjectMap objMap) = ObjectMap <$> mapM (mapMOMI f) objMap
  where
    mapMOMI :: Monad m => (ObjArr e meta1 -> m (ObjArr e meta2)) -> ObjectMapItem e meta1 -> m (ObjectMapItem e meta2)
    mapMOMI f2 (ObjectMapItem decls runtimeDecls refineDecls defs) = do
      decls' <- mapM f2 decls
      runtimeDecls' <- mapM f2 runtimeDecls
      refineDecls' <- mapM f2 refineDecls
      defs'  <- mapM f2 defs
      return $ ObjectMapItem decls' runtimeDecls' refineDecls' defs'


filterObjectMap :: (ObjArr e m -> Bool) -> ObjectMap e m -> ObjectMap e m
filterObjectMap f (ObjectMap objMap) = ObjectMap $ H.mapMaybe f' objMap
  where
    f' (ObjectMapItem decls runtimeDecls refineDecls defs) = case (filter f decls, filter f runtimeDecls, filter f refineDecls, filter f defs) of
      ([], [], [], [])                                -> Nothing
      (decls', runtimeDecls', refineDecls', defs') -> Just $ ObjectMapItem decls' runtimeDecls' refineDecls' defs'

-- | Gets all recursive sub expression objects from an expression's arguments. Helper for 'getRecursiveObjs'
getRecursiveObjsExpr :: (ExprClass e, MetaDat m, Show (e m), Show m) => e m -> [e m]
getRecursiveObjsExpr expr | isNothing (maybeExprPath expr) = []
getRecursiveObjsExpr expr = concat [varSubObjects, argSubObjects, recursedSubObjects]
  where
    varSubObjects = map mkValue $ concatMap (varNamesWithPrefix . pkName . fst) $ exprAppliedOrdVars expr
    argSubObjects = concatMap (filter (isJust . maybeExprPath) . exprFromTupleArg) (exprAppliedArgs expr)
    exprFromTupleArg ObjArr{oaObj, oaArr=Just (maybeOaObjExpr, _)} = maybeToList oaObj ++ maybeToList maybeOaObjExpr
    exprFromTupleArg ObjArr{oaArr=Nothing} = []
    recursedSubObjects = concatMap getRecursiveObjsExpr argSubObjects

-- | Gets an object and all sub-objects (recursively) from it's arguments
getRecursiveObjs :: (ExprClass e, Show m, Show (e m), MetaDat m) => ObjArr e m -> ObjectMap e m
getRecursiveObjs ObjArr{oaBasis} | oaBasis == MatchObj = mempty
getRecursiveObjs oa@ObjArr{oaBasis, oaObj=Just objE} = objectMapFromList (oa : recursedSubObjects)
  where
    recursedSubObjects = map toObjMapItem $ getRecursiveObjsExpr objE
    toObjMapItem e = ObjArr (Just e) oaBasis Nothing [] Nothing
getRecursiveObjs oa = error $ printf "getRecursiveObjs with no input expression: %s" (show oa)

-- | The names an object DEFINES or DECLARES: its own object path plus the variables BOUND in its
-- object pattern — argument names, type variables, and destructuring aliases (@head\@ah@ binds
-- @ah@). These are all legitimate names in a term-rewriting language. What is NOT collected is
-- USAGE: the matching CRITERIA on the value side of a pattern (the constructor\/field heads @Cons@,
-- @head@ — references to existing names, not new bindings) and the arrow BODY (@this.eq.rel(..)@
-- USES @rel@, it does not define it). Collecting those fabricates names for terms that name no
-- object — a bare @rel@ absolutised to @/rel@ — polluting the type-name universe ('teNames').
-- 'exprVarArgs' is exactly the binding classifier (it keys aliases\/args\/vars and never criteria),
-- so the bound names come from it; the criteria and body are simply not traversed.
getAllObjArrNames :: (ExprClass e, MetaDat m, Show m, Show (e m)) => ObjArr e m -> [Name]
getAllObjArrNames oa@ObjArr{oaObj} = oaObjPath oa : (maybe [] getAllExprNames oaObj ++ maybe [] boundNames oaObj)
  where
    -- The OBJECT pattern's names — its head plus every arg name recursed through
    -- 'getAllExprNames'\/'exprAppliedArgs' — are all part of the DEFINITION: a parameter
    -- (@tst(d= T(x))@'s @d@) or an entry point (@main{io->IO}@ desugared to
    -- @ContextIn(value= main)@) is declared here, and its 'BoundedByKnown' relative bound (@~d@,
    -- @~main@) resolves only if the name is in 'teNames'. The arrow BODY is intentionally NOT
    -- traversed: a name USED there (@this.eq.rel@'s @rel@) must not mint an object. Aliases
    -- ('exprVarArgs') are added on top — 'getAllExprNames' takes an alias's base, not its new name.
    boundNames objE = mapMaybe varAuxName $ H.keys $ exprVarArgs objE
    varAuxName (TVArg n)       = Just (pkName n)
    varAuxName (TVVar n)       = Just (pkName n)
    varAuxName (TVApplied b _) = varAuxName b

getAllExprNames :: (ExprClass e, MetaDat m, Show m, Show (e m)) => e m -> [Name]
getAllExprNames e = maybeToList (maybeExprPath e) ++ concatMap argAllNames (exprAppliedArgs e) ++ concatMap fromOrdVar (exprAppliedOrdVars e)
  where
    fromOrdVar (v, _) = varNamesWithPrefix $ pkName v
    -- An arg's value side introduces its HEAD name: the entry point @ContextIn(value= main)@
    -- declares @main@ (a module-scoped @/Scratch/main@, a converted Python def's
    -- @value= say_hello(msg -> String)@) right here — it appears nowhere else — and its
    -- 'BoundedByKnown' relative bound (@~main@) resolves only if the name is in 'teNames'
    -- (this generalizes the former @data(/main)@ workaround in @stack/core/data.ct@). A
    -- matching-criterion head (@b= Box(v)@'s @Box@) is a reference to an existing name, so
    -- collecting it is idempotent — the same holds for criteria heads in the arg-name
    -- position (@unwrap(Box(v))@), which 'getAllObjArrNames' has always collected. Only the
    -- head is taken; nested bindings remain 'boundNames'\/'exprVarArgs' territory.
    argAllNames aoa = getAllObjArrNames aoa ++ argValueHeadName aoa
    argValueHeadName ObjArr{oaArr=Just (Just ve, _)} = maybeToList (maybeExprPath ve)
    argValueHeadName _                               = []

-- | Updates the types based on the format as they are fixed for inputs (due to arrows this does not work for output expressions)
exprPropagateTypes :: (MetaDat m, Show m) => Expr m -> Expr m
exprPropagateTypes (CExpr m c) = CExpr (mWithType (constantType c) m) c
exprPropagateTypes (Value m n) | getMetaType m == PTopType = Value (mWithType t m) n
  where
    t = relTypeVal n
exprPropagateTypes (Value m n) = Value m n
exprPropagateTypes (HoleExpr m h) = HoleExpr m h
exprPropagateTypes (AliasExpr base alias) = AliasExpr base' alias'
  where
    base' = exprPropagateTypes base
    alias' = exprPropagateTypes alias
exprPropagateTypes (EWhere m base cond) = EWhere m' base' cond'
  where
    base' = exprPropagateTypes base
    cond' = exprPropagateTypes cond
    -- A 'PredExpr' stores the condition as a single 'PartialType'. A boolean condition usually types
    -- as a singleton; a relational condition (e.g. @$this.gte(r= 0)@ from @Int__gte(0)@) types as a
    -- 'TopType' carrying the relation/expr partial, which we reuse. Anything else degrades to the
    -- base type with no predicate — sound (over-approximate) and load-safe, mirroring the
    -- typechecker's own tolerance for a non-singleton @where@ condition (see 'Constrain' ConWhere).
    m' = mWithType withPred m
    withPred = case condPredPartial of
      Just p  -> typeAddPreds (getExprType base') (PredsOne $ PredExpr p)
      Nothing -> getExprType base'
    condPredPartial = case maybeGetSingleton (getExprType cond') of
      Just p  -> Just p
      Nothing -> case getExprType cond' of
        TopType _ (PredsOne (PredRel p))  -> Just p
        TopType _ (PredsOne (PredExpr p)) -> Just p
        _                                 -> Nothing
exprPropagateTypes mainExpr@(TupleApply m (bm, be) tupleApplyArgs) = do
  let be' = exprPropagateTypes be
  let bm' = mWithType (getMetaType $ getExprMeta be') bm
  case tupleApplyArgs of
      (EAppArg arg@ObjArr{oaObj=Just argObj, oaArr=Just (Just argVal, argM)}) -> do
        let argName = inExprSingleton argObj
        let argVal' = exprPropagateTypes argVal
        let tp' = typeSetArg argName (getExprType argVal') (getExprType be')
        let m' = mWithType tp' m
        TupleApply m' (bm', be') (EAppArg arg{oaArr=Just (Just argVal', argM)})
      (EAppArg ObjArr{oaObj=Just argObj, oaArr=Just (Nothing, argM)}) -> do
        let argName = inExprSingleton argObj
        let tp' = typeSetArg argName (getMetaType argM) (getExprType be')
        let m' = mWithType tp' m
        TupleApply m' (bm', be') (EAppArg $ mkIObjArr argM argName)
      (EAppVar vn vm) -> do
        let tp' = typeSetVar vn (getMetaType vm) (getExprType be')
        let m' = mWithType tp' m
        TupleApply m' (bm', be') (EAppVar vn vm)
      (EAppSpread a) -> do
        let m' = mWithType (spreadType H.empty $ getMetaType bm') m
        TupleApply m' (bm', be') (EAppSpread $ exprPropagateTypes a)
      _ -> error $ printf "Unexpected ObjArr in exprPropagateTypes (probably because arrow only ObjArr): %s" (show mainExpr)
