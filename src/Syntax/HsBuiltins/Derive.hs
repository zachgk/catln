--------------------------------------------------------------------
-- |
-- Module    :  Syntax.HsBuiltins.Derive
-- Copyright :  (c) Zach Kimberg 2026
-- License   :  MIT
-- Maintainer:  zachary@kimberg.com
-- Stability :  experimental
-- Portability: non-portable
--
-- A Haskell type read as a Catln one, derived from its shape.
--
-- A definition the compiler supplies rather than one written in a @.ct@ file needs three things to
-- agree with each other: the declaration written onto the builtins page, the value a Haskell value
-- is handed to an evaluation as, and the Haskell value read back out of one. Written by hand those
-- are three places to keep in step per type, and 'Eval.Common.valResultFiles' records what that
-- costs — a third copy of @\/Catln\/BuildFile@'s argument keys had them wrong, looking up @path@
-- where an evaluated value keys @\/path@, and nothing noticed because the tests called the
-- evaluation and never the command.
--
-- So all three are derived from one 'GHC.Generics.Rep'. A type opts in by deriving 'Generic' and
-- writing @instance CtType T@, and what it is called, what it declares, and how it converts cannot
-- then disagree, because they are read off the same thing.
--
-- Generics rather than TemplateHaskell because @base@ already supplies it and the repo already
-- writes @deriving (Generic, ToJSON)@ in some twenty places, so nothing new has to be depended on
-- or learned.
--------------------------------------------------------------------

{-# LANGUAGE DataKinds            #-}
{-# LANGUAGE DefaultSignatures    #-}
{-# LANGUAGE FlexibleContexts     #-}
{-# LANGUAGE FlexibleInstances    #-}
{-# LANGUAGE KindSignatures       #-}
{-# LANGUAGE ScopedTypeVariables  #-}
{-# LANGUAGE TypeOperators        #-}
{-# LANGUAGE UndecidableInstances #-}

module Syntax.HsBuiltins.Derive where

import           CtConstants
import           Data.Char               (isLower, isUpper, toLower)
import qualified Data.HashMap.Strict     as H
import           Data.Kind               (Type)
import           Data.List               (intercalate, union)
import           Data.Proxy              (Proxy (..))
import           Eval.Common             (Args, Val (..), valArg, valList,
                                          valListOf)
import           GHC.Generics
import           GHC.TypeLits            (ErrorMessage (..), KnownSymbol,
                                          TypeError, symbolVal)
import           Syntax.Ct.Builder
import           Syntax.Ct.Parser.Syntax (PExpr, PStatementTree)
import           Syntax.Ct.Prgm          (RawStatement (..),
                                          RawStatementTree (..))
import           Text.Printf

-- | Where a derived type is declared: beside @\/Catln\/BuildFile@ and @\/Catln\/CatlnResult@, which
-- are the compiler's other own types.
--
-- The namespace and nothing more — a name saying it came from Haskell rather than from a @.ct@ file
-- would be saying what the page it is declared on already says.
ctBuiltinNamespace :: String
ctBuiltinNamespace = "/Catln/"

-- | The pages of the standard library a derived type's fields can need. Only these, because only
-- these have 'CtType' instances; a type the core does not declare has nothing to be mapped onto.
coreStrPage, coreNumPage, coreBoolPage, coreMaybePage, coreListPage :: FilePath
coreStrPage = "core/str.ct"
coreNumPage = "core/num.ct"
coreBoolPage = "core/bool.ct"
coreMaybePage = "core/maybe.ct"
coreListPage = "core/list.ct"

-- | What a page must hold in order to name the types a walk has reached: the pages to import, the
-- declarations to carry, and the Catln paths already accounted for.
--
-- One accumulator rather than a walk each for imports and declarations, since the two are found at
-- the same points and a second walk could find a different set.
--
-- 'cpSeen' is keyed on the Catln path rather than on 'cpDecls' because a type the standard library
-- declares contributes an import and no declaration, and would otherwise be walked again at every
-- field that mentions it. A type is marked seen BEFORE its fields are walked, which is what makes a
-- type reaching itself terminate.
--
-- Both lists are appended to rather than prepended, so the page reads in the order the walk found
-- things: a type before the types of its fields.
data CtPage = CtPage {
    cpSeen    :: [String],
    cpImports :: [FilePath],
    cpDecls   :: [PStatementTree]
  }

emptyCtPage :: CtPage
emptyCtPage = CtPage [] [] []

-- | How a type's own names are written out, decided once at the top and carried down through the
-- walk of its representation.
--
-- 'coCon' rather than the constructor's Haskell name directly, because which path a constructor
-- gets depends on how many there are — a lone constructor is the type and a constructor of several
-- sits under it — and that is a question about the datatype which the walk cannot ask once it is
-- down at one constructor.
data CtOpts = CtOpts {
    coPath  :: String,
    coCon   :: String -> String,
    coField :: String -> String
  }

-- | One constructor as the page declares it: the path it is declared at, and each argument by name
-- and declared type. Empty arguments is a constructor written as a bare name, as
-- @\/Data\/Maybe\/Nothing@ is.
data CtCon = CtCon {
    ccPath   :: String,
    ccFields :: [(String, PExpr)]
  }

-- | A Haskell type read as a Catln one.
--
-- Every method has a default read off 'Rep', so a derived type's whole instance is
-- @instance CtType T@. The standard library's own types are instances written by hand instead: they
-- are already declared in @.ct@, so what they contribute to a page is an import and nothing else.
class CtType a where

  -- | The Catln path the type is declared at.
  ctPath :: Proxy a -> String
  default ctPath :: (Generic a, GCtDatatype (Rep a)) => Proxy a -> String
  ctPath _ = ctBuiltinNamespace ++ gCtDatatypeName (Proxy :: Proxy (Rep a))

  -- | The type as written where it is USED, which for a container is its path with its element type
  -- applied: @\/Data\/List[$T -> \/Data\/String]@, as @stack\/core\/result.ct@ writes it. The path
  -- alone for everything else.
  ctTypeExpr :: Proxy a -> PExpr
  ctTypeExpr = rawVal . ctPath

  -- | What a page holding the type must import for 'ctTypeExpr' to name something. Nothing for a
  -- type declared on the page itself.
  ctImports :: Proxy a -> [FilePath]
  ctImports _ = []

  -- | The Catln name of one of the type's record fields.
  --
  -- The default drops the prefix a Haskell record field is written with — @sampleName@ becomes
  -- @name@ and @bfPath@ becomes @path@ — which is the convention every record in this repo is
  -- already written in, and is what makes a derived declaration read like the hand-written
  -- @BuildFile(path -> String, contents -> String)@ it stands beside. A field written without one
  -- ('ctDropFieldPrefix' being the judge of that) is left alone.
  ctFieldName :: Proxy a -> String -> String
  ctFieldName _ = ctDropFieldPrefix

  -- | What the page says about the type, beneath its declaration: one entry per paragraph, as the
  -- page's own comments are written and as a @.ct@ doc comment reads. Nothing by default, which is a
  -- declaration with nothing written on it rather than an empty comment.
  ctDoc :: Proxy a -> [String]
  ctDoc _ = []

  -- | The page, with this type and everything its fields reach added to what it was given.
  ctPageInto :: Proxy a -> CtPage -> CtPage
  default ctPageInto :: (Generic a, GCtCons (Rep a), GCtDatatype (Rep a)) => Proxy a -> CtPage -> CtPage
  ctPageInto p page
    | ctPath p `elem` cpSeen page = page
    | otherwise = gCtConsPage (Proxy :: Proxy (Rep a)) page{
        cpSeen = cpSeen page ++ [ctPath p],
        cpImports = cpImports page `union` ctImports p,
        cpDecls = cpDecls page ++ [ctDeclOf p]
      }

  -- | The Catln value a Haskell value is.
  toCt :: a -> Val
  default toCt :: (Generic a, GCtCons (Rep a), GCtDatatype (Rep a)) => a -> Val
  toCt x = gToCt (ctOptsOf (Proxy :: Proxy a)) (from x)

  -- | The Haskell value a Catln one is, or why it is not one.
  fromCt :: Val -> Either String a
  default fromCt :: (Generic a, GCtCons (Rep a), GCtDatatype (Rep a)) => Val -> Either String a
  fromCt (TupleVal n as) = case gFromCt opts n as of
      Just res -> to <$> res
      -- Named rather than reporting whichever arm was tried last, since what a reader of the error
      -- needs is what would have been accepted.
      Nothing -> Left $ printf "%s is not a %s, which is %s" n (ctPath p) (intercalate " or " $ map (coCon opts) $ gCtConNames (Proxy :: Proxy (Rep a)))
    where
      p = Proxy :: Proxy a
      opts = ctOptsOf p
  fromCt v = Left $ printf "%s is not a %s" (show v) (ctPath (Proxy :: Proxy a))

-- | The page a type and everything it reaches makes, on its own.
ctPageOf :: (CtType a) => Proxy a -> CtPage
ctPageOf p = ctPageInto p emptyCtPage

-- | A page given a type which declares nothing of its own: the pages it needs, and nothing else.
--
-- This is what every hand-written instance below uses for 'ctPageInto'. A container additionally
-- walks its element type, since what a @[a]@ needs is whatever an @a@ needs.
ctPageImport :: (CtType a) => Proxy a -> CtPage -> CtPage
ctPageImport p page
  | ctPath p `elem` cpSeen page = page
  | otherwise = page{cpSeen = cpSeen page ++ [ctPath p], cpImports = cpImports page `union` ctImports p}

-- | How a type's constructors are named, which is what depends on how many of them there are.
--
-- One constructor is the type itself: a Haskell record's constructor name is an artifact of Haskell
-- needing one, and @data(\/Catln\/T(…))@ is what the same type written in @.ct@ would be. Several
-- constructors sit in the type's own namespace, as @\/Data\/Maybe\/Some@, @\/Data\/List\/Cons@ and
-- @\/Data\/Primitive\/True@ do.
ctOptsOf :: forall a. (CtType a, Generic a, GCtCons (Rep a)) => Proxy a -> CtOpts
ctOptsOf p = CtOpts (ctPath p) conPath (ctFieldName p)
  where
    single = case gCtConNames (Proxy :: Proxy (Rep a)) of
      [_] -> True
      _   -> False
    conPath c | single = ctPath p
              | otherwise = ctPath p ++ "/" ++ c

-- | The statement declaring a type, with whatever it says about itself beneath it.
--
-- A sum is ONE statement with its constructors written inline in the class's member list, which is
-- how @stack\/core\/bool.ct@ writes @class(Boolean, [True, False])@ and @stack\/core\/maybe.ct@
-- writes @class(Maybe[$T], [Maybe\/Some[$T -> $_T](value -> $T), Maybe\/Nothing])@ — rather than a
-- class and a @data@ per arm, which would be the same thing said in more places.
ctDeclOf :: forall a. (CtType a, Generic a, GCtCons (Rep a)) => Proxy a -> PStatementTree
ctDeclOf p = RawStatementTree statement doc
  where
    cons = gCtCons (ctOptsOf p) (Proxy :: Proxy (Rep a))
    statement = case cons of
      [con] -> dataSt (ctConExpr con)
      _     -> classWithObjs (rawVal $ ctPath p) (map ctConExpr cons) []
    doc = [RawStatementTree (RawAnnot (rawComment d)) [] | d <- ctDoc p]

-- | One constructor as it is written in a declaration: its path applied to each argument's declared
-- type. A constructor with no arguments is the bare path, since 'applyRawEIArgs' of nothing is what
-- it was given.
ctConExpr :: CtCon -> PExpr
ctConExpr (CtCon path fields) = rawVal path `applyRawEIArgs` map field fields
  where
    -- The argument's name as WRITTEN, not as a 'Semantics.Types.partialKey' — which would make it
    -- absolute (@\/name@) here, where the parser leaves a written @name@ relative for the desugar to
    -- make absolute. Both reach the same declaration; only this one reads back as what a person
    -- would have typed.
    field (argName, argType) = (rawVal argName, IArgM (Just argType))

-- | The Catln name of a Haskell record field: the type's prefix dropped, and what is left
-- uncapitalised. @sampleName@ is @name@ and @bfPath@ is @path@.
--
-- A prefix is a leading run of lowercase letters followed by an uppercase one, which is what the
-- convention actually is. A field written with no prefix (@radius@) has no uppercase letter to find
-- and is left as it is, and so is one that is all prefix.
ctDropFieldPrefix :: String -> String
ctDropFieldPrefix n = case span isLower n of
  (prefix, c:rest) | not (null prefix) && isUpper c -> toLower c : rest
  _                                                 -> n

-- | The datatype level of a representation, which is where the type's own name is.
--
-- A class of its own rather than another method of 'GCtCons', so that the constructor and sum
-- instances do not each have to carry an implementation of a question only the datatype can answer.
class GCtDatatype (f :: Type -> Type) where
  gCtDatatypeName :: proxy f -> String

instance (Datatype d) => GCtDatatype (M1 D d f) where
  gCtDatatypeName _ = datatypeName (undefined :: M1 D d f p)

-- | The constructors of a representation.
class GCtCons (f :: Type -> Type) where
  -- | Every constructor's Haskell name, in the order written. Asked without 'CtOpts' because it is
  -- what 'ctOptsOf' counts in order to build them.
  gCtConNames :: proxy f -> [String]
  -- | Every constructor as the page declares it.
  gCtCons :: CtOpts -> proxy f -> [CtCon]
  -- | The pages and declarations every constructor's fields reach.
  gCtConsPage :: proxy f -> CtPage -> CtPage
  -- | The value, which is the constructor reached and its arguments.
  gToCt :: CtOpts -> f p -> Val
  -- | The value read back, or 'Nothing' where the name is no constructor of this branch — which is
  -- what distinguishes a wrong constructor from a right one whose arguments do not fit.
  gFromCt :: CtOpts -> String -> Args -> Maybe (Either String (f p))

instance (GCtCons f) => GCtCons (M1 D d f) where
  gCtConNames _ = gCtConNames (Proxy :: Proxy f)
  gCtCons opts _ = gCtCons opts (Proxy :: Proxy f)
  gCtConsPage _ = gCtConsPage (Proxy :: Proxy f)
  gToCt opts (M1 x) = gToCt opts x
  gFromCt opts n as = fmap M1 <$> gFromCt opts n as

instance (GCtCons f, GCtCons g) => GCtCons (f :+: g) where
  gCtConNames _ = gCtConNames (Proxy :: Proxy f) ++ gCtConNames (Proxy :: Proxy g)
  gCtCons opts _ = gCtCons opts (Proxy :: Proxy f) ++ gCtCons opts (Proxy :: Proxy g)
  gCtConsPage _ = gCtConsPage (Proxy :: Proxy g) . gCtConsPage (Proxy :: Proxy f)
  gToCt opts (L1 x) = gToCt opts x
  gToCt opts (R1 x) = gToCt opts x
  gFromCt opts n as = case fmap L1 <$> gFromCt opts n as of
    Just res -> Just res
    Nothing  -> fmap R1 <$> gFromCt opts n as

instance (Constructor c, GCtFields f) => GCtCons (M1 C c f) where
  gCtConNames _ = [conName (undefined :: M1 C c f p)]
  gCtCons opts _ = [CtCon (coCon opts $ conName (undefined :: M1 C c f p)) (gCtFields opts (Proxy :: Proxy f))]
  gCtConsPage _ = gCtFieldsPage (Proxy :: Proxy f)
  gToCt opts (M1 x) = TupleVal (coCon opts $ conName (undefined :: M1 C c f p)) (gToArgs opts x)
  gFromCt opts n as
    | n == coCon opts (conName (undefined :: M1 C c f p)) = Just $ M1 <$> gFromArgs opts as
    | otherwise = Nothing

-- | The fields of one constructor.
class GCtFields (f :: Type -> Type) where
  -- | Each argument by the name it is declared under and the type it is declared as.
  gCtFields :: CtOpts -> proxy f -> [(String, PExpr)]
  -- | The pages and declarations the fields' own types reach, which is where a page gets the
  -- declaration of a record held inside another record.
  gCtFieldsPage :: proxy f -> CtPage -> CtPage
  -- | The arguments a value carries, keyed as an evaluated value keys them.
  gToArgs :: CtOpts -> f p -> Args
  -- | The fields read back out of those arguments.
  gFromArgs :: CtOpts -> Args -> Either String (f p)

instance GCtFields U1 where
  gCtFields _ _ = []
  gCtFieldsPage _ = id
  gToArgs _ _ = H.empty
  gFromArgs _ _ = Right U1

instance (GCtFields f, GCtFields g) => GCtFields (f :*: g) where
  gCtFields opts _ = gCtFields opts (Proxy :: Proxy f) ++ gCtFields opts (Proxy :: Proxy g)
  gCtFieldsPage _ = gCtFieldsPage (Proxy :: Proxy g) . gCtFieldsPage (Proxy :: Proxy f)
  gToArgs opts (x :*: y) = H.union (gToArgs opts x) (gToArgs opts y)
  gFromArgs opts as = (:*:) <$> gFromArgs opts as <*> gFromArgs opts as

instance (KnownSymbol name, CtType t) => GCtFields (M1 S ('MetaSel ('Just name) su ss ds) (K1 R t)) where
  gCtFields opts _ = [(fieldNameOf opts (Proxy :: Proxy name), ctTypeExpr (Proxy :: Proxy t))]
  gCtFieldsPage _ = ctPageInto (Proxy :: Proxy t)
  gToArgs opts (M1 (K1 x)) = H.singleton (runtimeArgKey $ fieldNameOf opts (Proxy :: Proxy name)) (toCt x)
  gFromArgs opts as = M1 . K1 <$> (fromCt =<< valArg (coPath opts) argName as)
    where argName = fieldNameOf opts (Proxy :: Proxy name)

-- | A constructor whose fields have no names has nothing to declare its arguments under. Inventing
-- them would be naming an argument something the @.ct@ side could never have chosen, so this is
-- refused where it is written rather than papered over.
instance (TypeError ('Text "A Catln argument is named, and this constructor's fields are not."
                     ':$$: 'Text "Write the Haskell constructor with record syntax to give them names."))
    => GCtFields (M1 S ('MetaSel 'Nothing su ss ds) (K1 R t)) where
  gCtFields = undefined
  gCtFieldsPage = undefined
  gToArgs = undefined
  gFromArgs = undefined

-- | The Catln name of the field a selector stands for.
fieldNameOf :: forall name p. (KnownSymbol name) => CtOpts -> p name -> String
fieldNameOf opts = coField opts . symbolVal

-- | A 'String' is the primitive @\/Data\/String@, and not a list of characters — the core declares
-- no character type for a list of them to be of.
--
-- Overlapping is what says so: @String@ is @[Char]@, so without it the instance for a list would
-- claim it.
instance {-# OVERLAPPING #-} CtType String where
  ctPath _ = strPrim
  ctImports _ = [coreStrPage]
  ctPageInto = ctPageImport
  toCt = StrVal
  fromCt (StrVal s) = Right s
  fromCt v          = Left $ printf "%s is not a %s" (show v) strPrim

instance CtType Integer where
  ctPath _ = intPrim
  ctImports _ = [coreNumPage]
  ctPageInto = ctPageImport
  toCt = IntVal
  fromCt (IntVal i) = Right i
  fromCt v          = Left $ printf "%s is not a %s" (show v) intPrim

-- | An 'Int' is the same Catln type as an 'Integer', which is unbounded. A value that does not fit
-- is reported rather than wrapped: a silently truncated number is the kind of thing that is found
-- much later and somewhere else.
instance CtType Int where
  ctPath _ = intPrim
  ctImports _ = [coreNumPage]
  ctPageInto = ctPageImport
  toCt = IntVal . toInteger
  fromCt (IntVal i)
    | i >= toInteger (minBound :: Int) && i <= toInteger (maxBound :: Int) = Right $ fromInteger i
    | otherwise = Left $ printf "%s does not fit in an Int" (show i)
  fromCt v = Left $ printf "%s is not a %s" (show v) intPrim

instance CtType Double where
  ctPath _ = floatPrim
  ctImports _ = [coreNumPage]
  ctPageInto = ctPageImport
  toCt = FloatVal
  fromCt (FloatVal f) = Right f
  fromCt v            = Left $ printf "%s is not a %s" (show v) floatPrim

instance CtType Bool where
  ctPath _ = ctBooleanPath
  ctImports _ = [coreBoolPage]
  ctPageInto = ctPageImport
  toCt True  = TupleVal truePrim H.empty
  toCt False = TupleVal falsePrim H.empty
  fromCt (TupleVal n _) | n == truePrim = Right True
  fromCt (TupleVal n _) | n == falsePrim = Right False
  fromCt v = Left $ printf "%s is not a %s" (show v) ctBooleanPath

instance (CtType a) => CtType (Maybe a) where
  ctPath _ = ctMaybePath
  ctTypeExpr _ = ctElemTypeExpr ctMaybePath (Proxy :: Proxy a)
  ctImports _ = [coreMaybePage]
  ctPageInto _ = ctPageInto (Proxy :: Proxy a) . ctPageImport (Proxy :: Proxy (Maybe a))
  toCt Nothing = TupleVal ctMaybeNothing H.empty
  toCt (Just x) = TupleVal ctMaybeSome $ H.singleton (runtimeArgKey ctMaybeSomeValue) (toCt x)
  fromCt (TupleVal n _) | n == ctMaybeNothing = Right Nothing
  fromCt (TupleVal n as) | n == ctMaybeSome = Just <$> (fromCt =<< valArg n ctMaybeSomeValue as)
  fromCt v = Left $ printf "%s is not a %s" (show v) ctMaybePath

-- | A list is a @\/Data\/Cons@ chain ending in a @\/Data\/Nil@, which 'Eval.Common.valListOf' and
-- 'Eval.Common.valList' are already the one place that knows.
instance {-# OVERLAPPABLE #-} (CtType a) => CtType [a] where
  ctPath _ = ctListPath
  ctTypeExpr _ = ctElemTypeExpr ctListPath (Proxy :: Proxy a)
  ctImports _ = [coreListPage]
  ctPageInto _ = ctPageInto (Proxy :: Proxy a) . ctPageImport (Proxy :: Proxy [a])
  toCt = valListOf . map toCt
  fromCt v = traverse fromCt =<< valList v

-- | A parameterized type as it is written where it is used: its path with its element type applied
-- to 'CtConstants.ctElemVar', which is @\/Data\/List[$T -> \/Catln\/BuildFile]@ in
-- @stack\/core\/result.ct@.
ctElemTypeExpr :: (CtType a) => String -> Proxy a -> PExpr
ctElemTypeExpr path p = rawVal path `applyRawExprEVars` [(rawVal ctElemVar, Just (ctTypeExpr p))]
