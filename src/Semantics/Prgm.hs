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
{-# LANGUAGE PatternSynonyms           #-}

module Semantics.Prgm where

import           Data.Hashable
import qualified Data.HashMap.Strict as H
import qualified Data.HashSet        as S
import           GHC.Generics        (Generic)

import           Control.Monad       (join)
import           CtConstants
import           Data.Aeson          hiding (Object)
import           Data.Bifunctor      (first)
import           Data.Char           (toLower)
import           Data.List           (isPrefixOf, nub, stripPrefix)
import           Data.Maybe
import           Data.UUID
import           Data.Void           (Void)
import           Semantics.CtType
import           Semantics.TypeGraph (ReachesTree)
import           Semantics.Types
import           Semantics.Val       (Val (IntVal, TupleVal), pattern StrVal,
                                      valArg, valStrArg)
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
-- | Everything a program declares and defines.
--
-- 'omItems' holds the arrows whose input can be represented by a 'PartialType': an object, which is
-- a name and the arguments it takes. That representation is what the rest of the compiler is built
-- around, and keeping them in a map by the type name of their input is what lets a call find the
-- arrows it may take by looking up its own name rather than asking every arrow of the program.
--
-- 'omDecls' holds the declarations whose input can not: one written from every object matching a
-- predicate rather than from an object. An arrow from a class (@(_ -> String) -> String\/Text..@)
-- is written from every member of the class, which is no one name to keep it under, and it is not an
-- object of its own that a value could be. It holds declarations only for now; definitions written
-- the same way will likely need to be kept here too.
data ObjectMap e m = ObjectMapWith {
  omItems :: H.HashMap TypeName (ObjectMapItem e m),
  omDecls :: [ObjArr e m]
  }
  deriving (Eq, Show, Generic, ToJSON, ToJSONKey)

-- | An object map read as its items alone. Only a match: building one from items alone would leave
-- out its 'omDecls', which a map rebuilt from another's items must not lose without saying so, so a
-- map is built with 'ObjectMapWith' or 'objectMapItems'.
pattern ObjectMap :: H.HashMap TypeName (ObjectMapItem e m) -> ObjectMap e m
pattern ObjectMap items <- ObjectMapWith items _
{-# COMPLETE ObjectMap #-}

-- | An object map of items alone, with no declaration written from a set of objects.
objectMapItems :: H.HashMap TypeName (ObjectMapItem e m) -> ObjectMap e m
objectMapItems items = ObjectMapWith items []

-- | Everything a program declares and defines, read as a Catln type: each name, and what is written
-- under it.
instance CtType (ObjectMap Expr ()) where
  ctDoc _ = [
    "Everything a program declares and defines, by the name each is written under.",
    "Handed over as a list of pairs, in order of name, so that two programs holding the same definitions are handed over the same way however they were built.",
    "[items] are what is written from an object, under the object's name. [decls] are the declarations written from a set of objects instead, such as an arrow from a class, which no one name stands for."
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
  (ObjectMapWith map1 decls1) <> (ObjectMapWith map2 decls2) = ObjectMapWith (H.unionWith merge map1 map2) (decls1 ++ decls2)
    where
      merge (ObjectMapItem decl1 runtimeDecl1 refineDecl1 def1) (ObjectMapItem decl2 runtimeDecl2 refineDecl2 def2) = ObjectMapItem (decl1 ++ decl2) (runtimeDecl1 ++ runtimeDecl2) (refineDecl1 ++ refineDecl2) (def1 ++ def2)

instance Monoid (ObjectMap e m) where
  mempty = ObjectMapWith H.empty []

instance Semigroup (Prgm e m) where
  (<>) :: Prgm e m -> Prgm e m -> Prgm e m
  (Prgm objMap1 classGraph1 annots1) <> (Prgm objMap2 classGraph2 annots2) = Prgm
    (objMap1 <> objMap2)
    (classGraph1 <> classGraph2)
    (annots1 <> annots2)

instance Monoid (Prgm e m) where
  mempty = Prgm mempty mempty []

-- | What one argument or type variable is bound to at a call, with the expressions that bind it.
--
-- A type variable is bound by each argument whose declared type is the variable itself
-- (@(l :: $T == r :: $T)@), and each such DIRECT occurrence is one of its members, so they JOIN
-- ('abJoin'). Every other reading constrains the one value and MEETS ('abMeet'): an argument
-- bound twice, a linked dimension (@lst -> ConsList[$T]@, 'linkedDimBinds'), the variable's own
-- slot on the call ('EAppVar'). The binding is the two met ('abType'). They are kept apart while the
-- bindings are combined so the result does not depend on the order the readings arrive in: the
-- call's own @[$T]@ slot coming first must not turn the occurrences after it into a meet.
data ArgBind m = ArgBind
  { abMetas :: [(Expr m, Meta m)]
  , abJoin  :: Maybe Type -- ^ The join of the direct occurrences; 'Nothing' where there are none
  , abMeet  :: Type       -- ^ The meet of every other reading; ⊤ where there are none
  , abType  :: Type       -- ^ The binding: 'abJoin' met with 'abMeet'
  }

-- | A reading that constrains the value: it meets the others.
meetBind :: [(Expr m, Meta m)] -> Type -> ArgBind m
meetBind ms t = ArgBind ms Nothing t t

-- | A direct occurrence of a type variable: it joins the other direct occurrences. A ⊤ says nothing
-- (an argument the source does not spell), so it is no member and joins nothing.
directBind :: [(Expr m, Meta m)] -> Type -> ArgBind m
directBind ms PTopType = meetBind ms PTopType
directBind ms t        = ArgBind ms (Just t) PTopType t

mergeArgBind :: (TypeGraph tg) => TypeEnv tg -> ArgBind m -> ArgBind m -> ArgBind m
mergeArgBind typeEnv (ArgBind m1s j1 o1 _) (ArgBind m2s j2 o2 _) = ArgBind (m1s ++ m2s) j o t
  where
    j = case (j1, j2) of
      (Nothing, _)     -> j2
      (_, Nothing)     -> j1
      (Just a, Just b) -> Just (unionTypes typeEnv a b)
    o = intersectTypes typeEnv o1 o2
    t = case j of
      Nothing -> o
      Just j' -> intersectTypes typeEnv o j'

type ArgMetaMapWithSrcItem m = ArgBind m
type ArgMetaMapWithSrc m = H.HashMap TypeVarAux (ArgBind m)

-- | Represents multiple possible 'ArgMetaMapWithSrc' that can be achieved by different matches (can choose with match)
type PossArgMetaMapWithSrc m = [ArgMetaMapWithSrc m]

class ExprClass e where
  -- | Returns the metadata for the top level of an expression
  getExprMeta ::  e m -> Meta m

  -- | Returns the metadata for the top level of an expression
  setExprMeta :: e m -> Meta m -> e m

  -- | Returns the value at the base of an expression, if it exists
  maybeExprPathM :: e m -> Maybe (TypeName, Meta m)

  -- | The base and condition of an expression that is a @where@ at its root (@a | a.isZ == True@).
  -- Only the desugared 'Expr' has one; the raw forms read as having none.
  exprWhereCond :: e m -> Maybe (e m, e m)
  exprWhereCond _ = Nothing

  -- | The base and alias of an expression that is an alias at its root (@x\@fx@). Only the
  -- desugared 'Expr' has one; the raw forms read as having none.
  exprAliasParts :: e m -> Maybe (e m, e m)
  exprAliasParts _ = Nothing

  -- | Returns all arguments applied to a value
  exprAppliedArgs :: (Show m, MetaDat m) => e m -> [ObjArr e m]

  -- | Whether a value is applied to a spread (@Context(value, .._)@): it takes arguments beyond
  -- the ones it is applied to. Only the desugared forms spread; the raw forms read as not.
  exprSpreadsArgs :: e m -> Bool
  exprSpreadsArgs _ = False

  -- | The expressions written directly inside an expression: an applied value, each argument's
  -- key and value, a spread value, an alias's base and name, a condition. Only the desugared forms
  -- are walked; the raw forms read as having none.
  exprChildren :: e m -> [e m]
  exprChildren _ = []

  -- | Returns all vars applied to a value
  exprAppliedOrdVars :: (Show m, MetaDat m) => e m -> [(TypeVarName, Meta m)]

  -- | Returns all arguments located recursively in an expression
  exprVarArgs :: (MetaDat m, Show m) => e m -> VarArgMap e m

  -- | The 'exprVarArgsWithSrc' is similar to the 'exprArgs' function.
  -- | It differs in that it accepts an additional partial type that is equivalent to the expression and it pull the corresponding parts of the partial matching the args in the expression
  exprVarArgsWithSrc :: (TypeGraph tg, MetaDat m, Show m, Show (e m)) => TypeEnv tg -> e m -> PartialType -> PossArgMetaMapWithSrc m

  mkValue :: (MetaDat m) => Name -> e m


instance ExprClass Expr where
  exprSpreadsArgs (TupleApply _ (_, be) arg) = case arg of
    EAppSpread{} -> True
    _            -> exprSpreadsArgs be
  exprSpreadsArgs (AliasExpr b _) = exprSpreadsArgs b
  exprSpreadsArgs (EWhere _ b _) = exprSpreadsArgs b
  -- A bare spread (@(.._)@, @X..@) is the name alone given the spread type rather than applied to
  -- a spread ('semiDesExpr'), so it is read off that type: a name admitting unlisted args.
  exprSpreadsArgs (Value m _) = case getMetaType m of
    UnionType Nothing leafs [] -> any (not . ptArgUb . ptArgMode) (splitUnionType leafs)
    _                          -> False
  exprSpreadsArgs _ = False
  exprChildren (AliasExpr b a) = [b, a]
  exprChildren (EWhere _ b c) = [b, c]
  exprChildren (TupleApply _ (_, b) arg) = b : case arg of
    EAppArg ObjArr{oaObj, oaArr} -> maybeToList oaObj ++ maybe [] (maybeToList . fst) oaArr
    EAppVar{}                    -> []
    EAppSpread a                 -> [a]
  exprChildren _ = []
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

  exprWhereCond (EWhere _ b c) = Just (b, c)
  exprWhereCond _              = Nothing
  exprAliasParts (AliasExpr b a) = Just (b, a)
  exprAliasParts _               = Nothing

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
  -- A guard matching an arrow's definition ('arrowDefGuardParts') binds what its pattern names.
  exprVarArgs (EWhere _ base cond)
    | Just (_, _, pat) <- arrowDefGuardParts cond = H.unionWith (++) (exprVarArgs base) (bodyPatternVarArgs pat)
  exprVarArgs (EWhere _ base _) = exprVarArgs base
  -- For an alias destructure (@field@name@) the arg's value is matched by the field but bound
  -- under the alias name (the name the body references), so key the field's arr-meta by the alias
  -- and recurse into the base — mirroring 'exprVarArgsWithSrc'. A bare binding uses the obj itself.
  exprVarArgs (TupleApply _ (_, be) (EAppArg ObjArr{oaObj=Just n, oaArr=Just (Nothing, arrM)})) = H.insertWith (++) (TVArg $ inExprSingleton bindName) [(n, arrM)] (H.unionWith (++) (exprVarArgs be) (exprVarArgs baseN))
    where (bindName, baseN) = case n of
            AliasExpr b a -> (a, b)
            _             -> (n, n)
  exprVarArgs (TupleApply _ _ (EAppArg ObjArr{oaObj, oaArr=Just (Nothing, _)})) = error $ printf "Unexpected unhandled obj type in exprVarArgs: %s" (show oaObj)
  -- A body pattern ('bodyPatternParts') binds its parameter as the higher-order parameter it is,
  -- and the pattern's variables. Not the parameter's own parameters: they are the matched
  -- function's, named only inside the pattern.
  exprVarArgs (TupleApply _ (_, be) (EAppArg oa))
    | Just (obj, pat, arrM) <- bodyPatternParts oa
    = H.insertWith (++) (TVArg $ inExprSingleton obj) [(obj, arrM)] (H.unionWith (++) (exprVarArgs be) (bodyPatternVarArgs pat))
  exprVarArgs (TupleApply _ (_, be) (EAppArg ObjArr{oaArr=Just (Just e, _)})) = H.unionWith (++) (exprVarArgs be) (exprVarArgs e)
  exprVarArgs (TupleApply _ (_, be) (EAppVar n m)) = H.insertWith (++) (TVVar n) [(Value (emptyMetaT $ partialToTypeSingleton n) (pkName n), m)] (exprVarArgs be)
  exprVarArgs (TupleApply _ _ (EAppArg ObjArr{oaObj, oaArr=Nothing})) = error $ printf "Not yet implemented: %s" (show oaObj)
  exprVarArgs (TupleApply _ (_, be) (EAppSpread arg)) = H.unionWith (++) (exprVarArgs arg) (exprVarArgs be)

  exprVarArgsWithSrc typeEnv expr src = aux expr
    where
      aux CExpr{} = [H.empty]
      aux Value{} = [H.empty]
      aux HoleExpr{} = [H.empty]
      -- A guard matching an arrow's definition ('arrowDefGuardParts') binds what its pattern says of
      -- its variables, as a body pattern does: the definition is found where the arm is chosen.
      aux (EWhere _ base cond)
        | Just (_, _, pat) <- arrowDefGuardParts cond
        = let patVars = H.map (\entries -> meetBind entries (getMetaType (snd (head entries)))) (bodyPatternVarArgs pat)
          in map (`H.union` patVars) (exprVarArgsWithSrc typeEnv base src)
      aux (EWhere _ base cond) = case maybeExprPath cond of
        Just n | nameStr n == operatorHasArrow -> case (H.lookup (partialKey (mkName operatorArgL)) condArgs, H.lookup (partialKey (mkName operatorArgR)) condArgs) of
                  (Just (Just (_, Just l)), Just (Just (_, Just r))) -> case (getExprType $ exprPropagateTypes l, getExprType $ exprPropagateTypes r) of
                    (lt@(UnionType Nothing _ _), rt) | isJust (maybeGetSingleton lt) -> concatMap findArrowMatches bases'
                      where
                        findArrowMatches base' = case typeGraphQuery typeEnv (ambientEnv $ (fmap abType base')) $ getSingleton lt of
                          [] -> [H.empty]
                          qr -> let fromEachArrow = fmap (\qrt -> envAmbient $ fst $ intersectTypesWithVarEnv typeEnv (ambientEnv $ (fmap abType base')) qrt rt) qr
                                in joinPossVarArgMetaWithSrc typeEnv (map (fmap (meetBind [])) fromEachArrow) [base']
                    _ -> [H.empty]
                  _ -> error $ printf "Could not find /l and /r in %s in ?->" (show condArgs)
        _ -> bases'
        where
          bases' = exprVarArgsWithSrc typeEnv base src
          condArgs = exprAppliedArgsMap cond
      aux (AliasExpr base n) = map (H.insertWith (mergeArgBind typeEnv) (TVArg $ inExprSingleton n) (meetBind [(n, getExprMeta base)] (singletonType src))) (exprVarArgsWithSrc typeEnv base src)
      aux (TupleApply _ (_, be) arg) = joinPossVarArgMetaWithSrc typeEnv (exprVarArgsWithSrc typeEnv be src) (fromArg arg)
        where
          fromArg :: (MetaDat m, Show m) => EApp Expr m -> [ArgMetaMapWithSrc m]
          -- A body pattern ('bodyPatternParts') reads its slot as the higher-order parameter it is.
          -- Its variables are what the pattern says of them, as nothing in the source holds them:
          -- they are bound where the arm is chosen for a function whose body matches.
          fromArg (EAppArg oa)
            | Just (_, pat, arrM) <- bodyPatternParts oa
            = let patVars = H.map (\entries -> meetBind entries (getMetaType (snd (head entries)))) (bodyPatternVarArgs pat)
              in map (`H.union` patVars) (fromArg (EAppArg oa{oaArr=Just (Nothing, arrM)}))
          fromArg (EAppArg ObjArr{oaObj=Just obj, oaArr=Just (Just e, _)}) = case typeGetArgLoose (inExprSingleton obj) src of
            Just (UnionType Nothing srcArg []) -> joinAllPossVarArgMetaWithSrc typeEnv $ map (exprVarArgsWithSrc typeEnv e) $ splitUnionType srcArg
            -- A pattern destructuring a CLASS-typed slot (@cs= Cons(head, tail)@ against
            -- @∀ConsList[/T=Char]@) matches the class's members of the pattern's name, so it binds
            -- what those members hold (@head=Char@), as a union's leaves are matched one by one
            -- above. A class with no such member binds the pattern's variables to the slot.
            Just t@(UnionType (Just _) _ _)
              | isJust (maybeExprPath e)
              , members@(_:_) <- splitUnionType (expandTypeWithNames typeEnv H.empty t (S.singleton (pkName (inExprSingleton e))))
              -> joinAllPossVarArgMetaWithSrc typeEnv $ map (exprVarArgsWithSrc typeEnv e) members
              | otherwise -> [(`meetBind` t) <$> exprVarArgs e]
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
                (TypeVar v _ _) -> [H.insertWith (mergeArgBind typeEnv) v (bindOcc v [(obj, arrM)] (fromMaybe PTopType srcArgOut)) mainArg]
                  where bindOcc TVVar{} = directBind
                        bindOcc _       = meetBind
                -- A declared argument spelled as a class application linking the object's
                -- variables (@lst -> List[/T=$_/T]@) BINDS those variables from the source
                -- content's linked dimension (@lst=Cons[/T=Integer](…)@ binds @/T=Integer@) —
                -- the argument position is a linked occurrence of the variable, so the content's
                -- element evidence is the variable's per-call value. Without this only an
                -- explicit application (@sorted[/T=Integer](…)@) could bind, and a declaration's
                -- linked result (@-> List[/T=$_/T]@) always presented at the variable's declared
                -- bound instead of the call's element type.
                declT -> [foldr insertLinked mainArg (linkedDimBinds typeEnv declT (fromMaybe PTopType srcArgOut))]
                  where insertLinked (v, d) = H.insertWith (mergeArgBind typeEnv) v (meetBind [(obj, arrM)] d)
            where
              structKey = inExprSingleton obj
              -- What the source passes for this argument, read as the argument's declared type
              -- ('arrM') reads it: for a plain argument its value, for a higher-order argument
              -- (@fn(v -> $_A) -> $B@) its RESULT — the input object the slot holds resolved
              -- through the environment's arrows ('hoSlotResult').
              srcArgOut = hoSlotResult typeEnv H.empty structKey <$> typeGetArgLoose structKey src
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
              mainArg = H.singleton (TVArg bindKey) (meetBind [(obj, arrM)] $
                case typeGetArgLoose structKey src of
                  Just t | t /= PTopType -> t
                  _                      -> getMetaType arrM)
          -- The variable's value read OUT of the source's slot, in the point's currency: the slot may
          -- name the source's other parameters (@/M=∀Monad[/T=$^A]@ — 'siblingRefsToParent'),
          -- which outside the source are its bindings ('slotAtPoint').
          fromArg (EAppVar vn vm) = [H.singleton (TVVar vn) (meetBind [(Value (emptyMetaT $ partialToTypeSingleton vn) (pkName vn), vm)] (slotAtPoint typeEnv H.empty src (H.lookupDefault PTopType vn (ptVars src))))]
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

-- | Combines the bindings two parts of an object's match make ('mergeArgBind').
joinPossVarArgMetaWithSrc :: (TypeGraph tg) => TypeEnv tg -> PossArgMetaMapWithSrc m -> PossArgMetaMapWithSrc m -> PossArgMetaMapWithSrc m
joinPossVarArgMetaWithSrc typeEnv aVarArgMaps bVarArgMaps = [H.unionWith (mergeArgBind typeEnv) aVarArgMap bVarArgMap | aVarArgMap <- aVarArgMaps, bVarArgMap <- bVarArgMaps]

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
      -- A body pattern's value is a pattern over a function's definition, not a value of the slot.
      _ | isJust (bodyPatternParts oa) -> Nothing
      (Just (AliasExpr base alias), Just (Nothing, m)) -> (, (base, m)) <$> maybeExprPath alias
      (Just o, Just (Just (AliasExpr _ alias), m))     -> (, (o, m))    <$> maybeExprPath alias
      _                                                -> Nothing

-- | An input argument written with its arrow's body as a pattern: @f(x, params)= pattern@ in a
-- definition's object. The argument is the higher-order parameter @f(x, params)@, as it is written
-- without the pattern, and the arm applies only to a function whose body matches the pattern — a
-- pattern over the function's definition rather than over a value it holds. The same shape in an
-- output is a lambda, and an input argument whose key takes no parameters (@field= pattern@) is a
-- destructure of the value in the slot, so it is the key's parameters that tell this one apart.
--
-- Its parts are the parameter (@f(x, params)@), the pattern, and the parameter's result meta.
bodyPatternParts :: (MetaDat m, Show m) => ObjArr Expr m -> Maybe (Expr m, Expr m, Meta m)
bodyPatternParts ObjArr{oaObj=Just obj, oaArr=Just (Just pat, arrM)}
  | not (Prelude.null (exprAppliedArgs obj)) = Just (obj, pat, arrM)
bodyPatternParts _ = Nothing

-- | The arguments of the function a definition's object defines: its own, except for a function in
-- a context, which is defined under the context's name (@ContextIn(value= f(x), ctx)@,
-- 'oaDefinedName') and is given the arguments of the call it holds.
objDefinedArgs :: (MetaDat m, Show m) => Expr m -> [ObjArr Expr m]
objDefinedArgs obj = case H.lookup (partialKey (mkName contextValStr)) (exprAppliedArgsMap obj) of
  Just (Just (_, Just v)) | maybe False ((== ContextInStr) . nameStr . makeAbsoluteName) (maybeExprPath obj) -> exprAppliedArgs v
  _ -> exprAppliedArgs obj

-- | Whether a name is the parameter a spread of parameters is kept as (@..ps@ in @f(..ps)@,
-- 'spreadParamsPrefix'), which stands for whatever parameters a function has.
isSpreadParamsName :: Name -> Bool
isSpreadParamsName n = case npathPath (getPath (nameStr n)) of
  [] -> False
  ps -> spreadParamsPrefix `isPrefixOf` last ps

-- | Whether a node of a body pattern is a sub-function variable: a name applied to exactly the
-- parameters of the function the pattern is written over, each by its key alone (@a(x, params)@
-- in @f(x, params)= a(x, params) + b(x, params)@). It stands for a part of the body, and binds the
-- function that part is: the same parameters, with that part as its body. It is written in the
-- shape of a higher-order parameter because that is what it binds. The desugared pattern holds it
-- as the alias of a hole (@_\@a(x, params)@, 'Syntax.Ct.Desugarf.Expr.desBodyPattern'), the hole
-- being the part it matches, so it is bound as every alias is: the hole's meta is its result.
bodyPatternSubFn :: (ExprClass e, MetaDat m, Show m, Show (e m)) => S.HashSet ArgName -> e m -> Bool
bodyPatternSubFn params e = isJust (maybeExprPath e)
  && not (Prelude.null args)
  && all keyOnly args
  && Prelude.null (exprAppliedOrdVars e)
  && H.keysSet (exprAppliedArgsMap e) == params
  where
    args = exprAppliedArgs e
    keyOnly ObjArr{oaObj=Just k, oaArr=Just (Nothing, _)} = exprIsName k
    keyOnly _                                             = False

-- | The sub-function variables of a body pattern ('bodyPatternSubFn'), each the higher-order
-- parameter it binds.
bodyPatternSubFns :: (ExprClass e, MetaDat m, Show m, Show (e m)) => e m -> e m -> [e m]
bodyPatternSubFns obj = go
  where
    params = H.keysSet (exprAppliedArgsMap obj)
    go e | bodyPatternSubFn params e = [e]
    go e = concatMap go (exprChildren e)

-- | The variables a body pattern binds: its aliases. A literal variable (@(n :: Float)@) is the
-- alias of a typed hole, bound to the hole's meta, and a sub-function variable ('bodyPatternSubFn')
-- the alias of an untyped one, bound as the higher-order parameter it is with the hole's meta its
-- result. The parameters of the function the pattern is over are not bound: inside the pattern they
-- refer to that function's parameters, which are not the definition's.
bodyPatternVarArgs :: (MetaDat m, Show m) => Expr m -> VarArgMap Expr m
bodyPatternVarArgs = go
  where
    go (AliasExpr base n) = H.insertWith (++) (TVArg $ inExprSingleton n) [(n, getExprMeta base)] (go base)
    go (EWhere _ b _) = go b
    go (TupleApply _ (_, be) arg) = H.unionWith (++) (go be) $ case arg of
      EAppArg ObjArr{oaArr=Just (Just v, _)} -> go v
      EAppSpread a                           -> go a
      _                                      -> H.empty
    go _ = H.empty

-- | A guard matching the definition an arrow reaches, @(obj \@-> T)\@pattern@ (as
-- @(${fun}(x= x, params= params) \@-> Float)\@Some(value\@body)@): the arrow's input, its result
-- type and the pattern. The arm applies when what @\@->@ answers matches the pattern — @Some@ of
-- the one definition reached from the input by an arrow to the type, or @Nothing@ when there is
-- none — and is chosen where it is built ('TreeBuild.BodyPattern'), as a body pattern is. A name
-- the pattern gives the definition is the function it is ('Syntax.Ct.Desugarf.Expr.desArrowDefMatch').
arrowDefGuardParts :: (ExprClass e, MetaDat m, Show m, Show (e m)) => e m -> Maybe (e m, e m, e m)
arrowDefGuardParts cond
  | Just (op, pat) <- exprAliasParts cond
  , Just n <- maybeExprPath op
  , nameStr n == operatorArrowDef
  , Just (Just (_, Just l)) <- H.lookup (partialKey (mkName operatorArgL)) args
  , Just (Just (_, Just r)) <- H.lookup (partialKey (mkName operatorArgR)) args
  = Just (l, r, pat)
  where args = maybe H.empty (exprAppliedArgsMap . fst) (exprAliasParts cond)
arrowDefGuardParts _ = Nothing

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

-- | The key an argument written as this expression is filled under.
--
-- A plain name (@count@ in @f(count -> Integer)@, or in the call @f(count= 1)@) is keyed by the name
-- written, made absolute. It is a KEY and not a reference, so what that name would resolve to as a
-- reference is not what it means: were it read off the name's inferred type, a parameter sharing its
-- name with a function (@\/Data\/List\/count@) would be keyed by that function, and the declaration,
-- the definition, and the body's references to the parameter would all look it up there.
--
-- Any other form is keyed by the arguments and type variables WRITTEN on it, under its inferred
-- name where its type is one partial (its path otherwise). The key is the argument's identity, so it
-- must not move as inference types the argument. Read off the inferred type, it did: a declared
-- higher-order argument's input object carries the declaration's variables as parameters
-- (@fn[/B=$^B](acc=$/B)@, 'Semantics.Types.tagParentVarRefs'), and its key became
-- @fn[/B](acc)@ while every call still wrote @fn(acc)@ — so a call's arguments no longer fitted the
-- declaration's argument sets, and the declaration was refuted as a candidate for the call.
inExprSingleton :: (ExprClass e, MetaDat m, Show m, Show (e m)) => e m -> ArgName
inExprSingleton e
  | exprIsName e = partialKey $ makeAbsoluteName $ exprPath e
  | otherwise = (partialKey name){pkArgs = H.keysSet $ exprAppliedArgsMap e, pkVars = H.keysSet $ exprAppliedVars e}
  where
    name = maybe (makeAbsoluteName $ exprPath e) ptName (maybeGetSingleton (getExprType e))

-- | Whether an expression is only a name: a base value with nothing applied to it, no arguments and
-- no type variables (an alias @head\@h@ is its base name, @head@).
exprIsName :: (ExprClass e, MetaDat m, Show m) => e m -> Bool
exprIsName e = isJust (maybeExprPath e) && Prelude.null (exprAppliedArgs e) && Prelude.null (exprAppliedOrdVars e)

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
nullObjectMap (ObjectMapWith objMap decls) = H.null objMap && Prelude.null decls

flatObjectMapItem :: ObjectMapItem e m -> [ObjArr e m]
flatObjectMapItem (ObjectMapItem decls runtimeDecls refineDecls defs) = decls ++ runtimeDecls ++ refineDecls ++ defs

-- | The arrows of a map's items: everything written from an object. The declarations written from
-- a set of objects ('omDecls') are not among them; 'allObjArrs' is both.
flatObjectMap :: ObjectMap e m -> [ObjArr e m]
flatObjectMap (ObjectMap objMap) = concatMap flatObjectMapItem (H.elems objMap)

-- | Every arrow of a map: its items' ('flatObjectMap') and its declarations written from a set of
-- objects ('omDecls').
allObjArrs :: ObjectMap e m -> [ObjArr e m]
allObjArrs om = flatObjectMap om ++ omDecls om

-- | The arrows of an item that say which objects exist: all of them but its refinement
-- declarations. A refinement claims a narrower result for inputs its declarations already admit
-- (that it stays inside them is checked on its own), so it adds no object. Read as one, it enters
-- the object universe at declaration precedence, and where the declaration was solved earlier (a
-- class instance's page refining a core operator) it is the only declaration there: the arms are
-- then bounded by the refinement instead, lose the declaration's type parameters, and fail the
-- declaration coverage check.
universeObjectMapItem :: ObjectMapItem e m -> [ObjArr e m]
universeObjectMapItem item = flatObjectMapItem item{omiRefineDecls = []}

-- | 'universeObjectMapItem' over a whole map.
universeObjectMap :: ObjectMap e m -> [ObjArr e m]
universeObjectMap (ObjectMap objMap) = concatMap universeObjectMapItem (H.elems objMap)

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

-- | Whether an object arrow is a declaration marked @#invalid@: one whose promise holds only on part
-- of its domain, so it is neither checked for fulfillment nor tested on its promise.
isInvalidDecl :: (ExprClass e) => ObjArr e m -> Bool
isInvalidDecl = oaHasAnnotRel invalidAnnot

-- | Whether an object arrow is a runtime declaration (a declaration whose body is supplied by the
-- evaluator runtime). It is DEFINITION-like: it needs no separate definition to fulfill a
-- declaration, and it must not widen the object universe the way a declaration does.
isRuntimeDecl :: (ExprClass e) => ObjArr e m -> Bool
isRuntimeDecl oa@ObjArr{oaArr=Just (Nothing, _)} = oaHasAnnotRel runtimeAnnot oa
isRuntimeDecl _                                  = False

-- | How a name's arrows are quantified over their results.
--
-- A 'QuantUnique' name maps each input to one result (∀ input, ∃! output): wherever two of its arms
-- overlap they must agree, and a refinement of it must hold of every arm. A 'QuantExist' name maps an
-- input to any of its arms' results (∀ input, ∃ output): its overlapping arms are alternative
-- rewrites, and a refinement of it holds when some arm establishes it for each input.
data Quant = QuantUnique | QuantExist
  deriving (Eq, Ord, Show, Generic, Hashable, ToJSON)

-- | The quantification of a name: 'QuantExist' when one of its declarations is marked @#exist@ and
-- 'QuantUnique' otherwise. Only a declaration may carry it (see 'TypeCheck.Decode').
omiQuant :: (ExprClass e) => ObjectMapItem e m -> Quant
omiQuant omi
  | any (oaHasAnnotRel existAnnot) (omiDecls omi ++ omiRuntimeDecls omi) = QuantExist
  | otherwise = QuantUnique

-- | The function an object defines. That is its own name, except for a definition in a context
-- (@ContextIn(value= main, io)@), which is kept under the context's name — the name a call in a
-- context is looked up by — but defines the function it holds.
oaDefinedName :: (MetaDat m, ExprClass e, Show m, Show (e m)) => ObjArr e m -> TypeName
oaDefinedName oa = case H.lookup (partialKey (mkName contextValStr)) (exprAppliedArgsMap $ oaObjExpr oa) of
  Just (Just (_, Just v)) | nameStr (oaObjPath oa) == ContextInStr, Just n <- maybeExprPath v -> makeAbsoluteName n
  _ -> makeAbsoluteName (oaObjPath oa)

-- | The name an object's precedence is ranked under: what it defines among the objects kept under
-- its name. A definition in a context ranks with the other definitions of its own function in a
-- context (@/ContextIn/main@) — not with every function in a context, and not with the function
-- it holds, whose nested patterns (@exit(val -> Integer)@ in @ContextIn(value= exit(..), io)@) are
-- objects of that name in their own right.
oaPrecedenceName :: (MetaDat m, ExprClass e, Show m, Show (e m)) => ObjArr e m -> TypeName
oaPrecedenceName oa
  | nameStr (oaObjPath oa) == ContextInStr = mkName (ContextInStr ++ nameStr (oaDefinedName oa))
  | otherwise = makeAbsoluteName (oaObjPath oa)

-- | What an object written from a set of objects ('omDecls') is written as: the set, as the type of
-- the hole standing for its values, and the arguments applied to each of them, by name and type.
-- @(_ -> String)@ has none, and is a declaration of what each value converts to. @(_ ->
-- Univariate)(x -> Float)@ has @x@, and is a declaration of what each value gives once it is applied.
setDeclParts :: (MetaDat m, Show m) => Expr m -> Maybe (Type, [(ArgName, Type)])
setDeclParts (HoleExpr m _) = Just (getMetaType m, [])
setDeclParts (TupleApply _ (_, b) (EAppArg ObjArr{oaObj=Just argObj, oaArr=Just (_, argM)})) = do
  (set, args) <- setDeclParts b
  return (set, args ++ [(inExprSingleton argObj, getMetaType argM)])
setDeclParts _ = Nothing

singletonObjectMap :: (ExprClass e, MetaDat m, Show (e m), Show m) => ObjArr e m -> ObjectMap e m
singletonObjectMap oa@ObjArr{oaObj=Just obj} | isNothing (maybeExprPath obj) = ObjectMapWith H.empty [oa] -- written from a set of objects ('omDecls')
singletonObjectMap oa = objectMapItems $ H.singleton (makeAbsoluteName $ oaObjPath oa) omi
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
mapMObjectMap f (ObjectMapWith objMap setDecls) = ObjectMapWith <$> mapM (mapMOMI f) objMap <*> mapM f setDecls
  where
    mapMOMI :: Monad m => (ObjArr e meta1 -> m (ObjArr e meta2)) -> ObjectMapItem e meta1 -> m (ObjectMapItem e meta2)
    mapMOMI f2 (ObjectMapItem decls runtimeDecls refineDecls defs) = do
      decls' <- mapM f2 decls
      runtimeDecls' <- mapM f2 runtimeDecls
      refineDecls' <- mapM f2 refineDecls
      defs'  <- mapM f2 defs
      return $ ObjectMapItem decls' runtimeDecls' refineDecls' defs'


-- | The map's items filtered by @f@. The declarations written from a set of objects ('omDecls') are
-- kept as they are: a filter over objects asks what an object is called or how it is written, which
-- a set of objects has no single answer to, so a caller narrowing those does so itself.
filterObjectMap :: (ObjArr e m -> Bool) -> ObjectMap e m -> ObjectMap e m
filterObjectMap f (ObjectMapWith objMap setDecls) = ObjectMapWith (H.mapMaybe f' objMap) setDecls
  where
    f' (ObjectMapItem decls runtimeDecls refineDecls defs) = case (filter f decls, filter f runtimeDecls, filter f refineDecls, filter f defs) of
      ([], [], [], [])                                -> Nothing
      (decls', runtimeDecls', refineDecls', defs') -> Just $ ObjectMapItem decls' runtimeDecls' refineDecls' defs'

-- | Gets all recursive sub expression objects from an expression's arguments. Helper for 'getRecursiveObjs'
getRecursiveObjsExpr :: (ExprClass e, MetaDat m, Show (e m), Show m) => e m -> [e m]
getRecursiveObjsExpr expr | isNothing (maybeExprPath expr) = []
getRecursiveObjsExpr expr = concat [varSubObjects, argSubObjects, recursedSubObjects, guardSubObjects]
  where
    -- A guard matching an arrow's definition ('arrowDefGuardParts') binds the definition as the
    -- higher-order parameter it names, as a body pattern's sub-function variable is bound.
    guardSubObjects = case exprWhereCond expr of
      Just (_, cond) | Just (defInput, _, pat) <- arrowDefGuardParts cond -> bodyPatternSubFns defInput pat
      _ -> []
    varSubObjects = map mkValue $ concatMap (varNamesWithPrefix . pkName . fst) $ exprAppliedOrdVars expr
    argSubObjects = concatMap (filter (isJust . maybeExprPath) . exprFromTupleArg) (exprAppliedArgs expr)
    -- A body pattern's value ('bodyPatternParts') is a pattern over a function's definition: what
    -- it is written with are parts of that definition, not objects of this one.
    -- Its sub-function variables are the higher-order parameters they bind, as its key is one.
    exprFromTupleArg ObjArr{oaObj=Just o, oaArr=Just (Just pat, _)} | not (Prelude.null (exprAppliedArgs o)) = o : bodyPatternSubFns o pat
    exprFromTupleArg ObjArr{oaObj, oaArr=Just (maybeOaObjExpr, _)} = maybeToList oaObj ++ maybeToList maybeOaObjExpr
    exprFromTupleArg ObjArr{oaArr=Nothing} = []
    recursedSubObjects = concatMap getRecursiveObjsExpr argSubObjects

-- | Gets an object and all sub-objects (recursively) from it's arguments
--
-- A sub-object is an 'ArgObj' whatever the object it was found in. It is something written
-- inside another object's arguments, not a declaration of its own, so it must not outrank one in
-- 'TypeCheck.TypeUtils.objectPrecedence'. Inheriting the parent's basis made a call written as a
-- pattern (@dshowCall(s= mmap(m -> ConsList, fn(v)))@) a bodiless 'FunctionObj', which takes
-- the precedence of a type object. That beat @mmap@'s real declaration, which
-- 'TypeCheck.TypeUtils.filterBestPrecedence' then dropped from the object universe, leaving every
-- other call of @mmap@ bounded by the one pattern.
getRecursiveObjs :: (ExprClass e, Show m, Show (e m), MetaDat m) => ObjArr e m -> ObjectMap e m
getRecursiveObjs ObjArr{oaBasis} | oaBasis == MatchObj = mempty
getRecursiveObjs oa@ObjArr{oaObj=Just objE} = objectMapFromList (oa : recursedSubObjects)
  where
    recursedSubObjects = map toObjMapItem $ getRecursiveObjsExpr objE
    toObjMapItem e = ObjArr (Just e) ArgObj Nothing [] Nothing
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
getAllObjArrNames = uncurry (++) . getAllObjArrNameSplit

-- | 'getAllObjArrNames' with the arguments' value heads apart from the rest.
--
-- A value head is either a name declared here (an entry point's @main@) or a reference to one
-- declared elsewhere (a criterion's @Box@). Only the reader holding every other name can tell
-- the two apart, which 'Semantics.mkTypeEnv' does. Absolutised as if declared, a RELATIVE
-- reference named a second object: the receiver @this= Task/Simple(v)@ of an arm
-- @:Task/Simple(v).calendarDay@ minted @/Task/Simple@ beside the declared
-- @/Data/Note/Task/Simple@. Resolving @Task/Simple@ then found both, and the phantom leaf stayed
-- in the arm's typed input.
getAllObjArrNameSplit :: (ExprClass e, MetaDat m, Show m, Show (e m)) => ObjArr e m -> ([Name], [Name])
getAllObjArrNameSplit oa@ObjArr{oaObj} = (oaObjPath oa : exprNames ++ maybe [] boundNames oaObj, exprHeads)
  where
    (exprNames, exprHeads) = maybe ([], []) getAllExprNameSplit oaObj
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
getAllExprNames = uncurry (++) . getAllExprNameSplit

-- | 'getAllExprNames' with the arguments' value heads apart, as 'getAllObjArrNameSplit' says.
getAllExprNameSplit :: (ExprClass e, MetaDat m, Show m, Show (e m)) => e m -> ([Name], [Name])
getAllExprNameSplit e = (maybeToList (maybeExprPath e) ++ concatMap fst argSplits ++ concatMap fromOrdVar (exprAppliedOrdVars e), concatMap snd argSplits)
  where
    argSplits = map argAllNames (exprAppliedArgs e)
    fromOrdVar (v, _) = varNamesWithPrefix $ pkName v
    -- An arg's value side introduces its HEAD name: the entry point @ContextIn(value= main)@
    -- declares @main@ (a module-scoped @/Scratch/main@, a converted Python def's
    -- @value= say_hello(msg -> String)@) right here — it appears nowhere else — and its
    -- 'BoundedByKnown' relative bound (@~main@) resolves only if the name is in 'teNames'
    -- (this generalizes the former @data(/main)@ workaround in @stack/core/data.ct@). A
    -- matching-criterion head (@b= Box(v)@'s @Box@) is a reference to an existing name, so
    -- collecting it is idempotent — the same holds for criteria heads in the arg-name
    -- position (@unwrap(Box(v))@), which 'getAllObjArrNames' has always collected. Only the
    -- head is taken; nested bindings remain 'boundNames'\/'exprVarArgs' territory. A head is only
    -- idempotent when written absolute, so heads are returned apart for 'Semantics.mkTypeEnv' to
    -- resolve ('getAllObjArrNameSplit').
    argAllNames aoa = let (ns, hs) = getAllObjArrNameSplit aoa in (ns, hs ++ argValueHeadName aoa)
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
