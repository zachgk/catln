--------------------------------------------------------------------
-- |
-- Module    :  Semantics.CtType
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
-- are three places to keep in step per type, and 'Syntax.HsBuiltins.Types.valResultFiles' records
-- what that costs — a third copy of @\/Catln\/BuildFile@'s argument keys had them wrong, looking up
-- @path@ where an evaluated value keys @\/path@, and nothing noticed because the tests called the
-- evaluation and never the command.
--
-- So all three are derived from one 'GHC.Generics.Rep'. A type opts in by deriving 'Generic' and
-- writing @instance CtType T@, and what it is called, what it declares, and how it converts cannot
-- then disagree, because they are read off the same thing.
--
-- Generics rather than TemplateHaskell because @base@ already supplies it and the repo already
-- writes @deriving (Generic, ToJSON)@ in some twenty places, so nothing new has to be depended on
-- or learned.
--
-- What a type DECLARES is described here ('CtDecl') rather than built as syntax, and
-- "Syntax.HsBuiltins.Render" turns the description into statements. That is what keeps this module
-- clear of "Syntax.Ct.Builder" — which imports "Semantics.Prgm", so a class producing statements
-- could never have instances written beside the semantic types. Conversion is polymorphic in the
-- runtime half of a value ("Semantics.Val") for the same reason, since a converted value names no
-- runtime arm.
--------------------------------------------------------------------

{-# LANGUAGE DataKinds            #-}
{-# LANGUAGE DefaultSignatures    #-}
{-# LANGUAGE FlexibleContexts     #-}
{-# LANGUAGE FlexibleInstances    #-}
{-# LANGUAGE KindSignatures       #-}
{-# LANGUAGE ScopedTypeVariables  #-}
{-# LANGUAGE TypeOperators        #-}
{-# LANGUAGE UndecidableInstances #-}

module Semantics.CtType where

import           CtConstants
import           Data.Char           (isLower, isUpper, toLower)
import           Data.Hashable       (Hashable)
import qualified Data.HashMap.Strict as H
import qualified Data.HashSet        as S
import           Data.Kind           (Type)
import           Data.List           (intercalate, sort, sortOn, union)
import           Data.Proxy          (Proxy (..))
import           Data.UUID           (UUID)
import qualified Data.UUID           as UUID
import           GHC.Generics
import           GHC.TypeLits        (ErrorMessage (..), KnownSymbol, TypeError,
                                      symbolVal)
import           Semantics.Val
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
coreStrPage = "core/string.ct"
coreNumPage = "core/num.ct"
coreBoolPage = "core/bool.ct"
coreMaybePage = "core/maybe.ct"
coreListPage = "core/list.ct"

-- | A Catln type as it is written where it is USED: a path, and whatever is applied to it.
--
-- A description rather than an expression, so that reading a Haskell type does not depend on the
-- syntax it is eventually written in. "Syntax.HsBuiltins.Render" is what turns one into
-- @\/Data\/List[$T -> \/Data\/String]@.
data CtTypeRef = CtTypeRef {
    ctrPath :: String,
    ctrVars :: [(String, CtTypeRef)]
  } deriving (Eq, Show)

-- | The path alone, which is how every type not carrying parameters is written.
ctRef :: String -> CtTypeRef
ctRef path = CtTypeRef path []

-- | One constructor as the page declares it: the path it is declared at, the type parameters it is
-- declared with, and each argument by name and declared type. Empty arguments is a constructor
-- written as a bare name, as @\/Data\/Maybe\/Nothing@ is.
--
-- 'ccVars' are the parameters DECLARED, bound to nothing — the @[$A, $B]@ of
-- @data(\/Catln\/Pair[$A, $B](first -> $A, second -> $B))@, which is how @stack\/core\/data.ct@
-- writes @data(\/Context[$V](value -> $V, .._))@. A parameter APPLIED to something is a 'CtTypeRef'
-- instead, which is the other half of the same syntax and was the only half that existed: a derived
-- type could say @\/Data\/List[$T -> \/Data\/String]@ where it was used and had no way to declare
-- the @[$T]@ that makes it mean anything.
data CtCon = CtCon {
    ccPath   :: String,
    ccVars   :: [String],
    ccFields :: [(String, CtTypeRef)]
  } deriving (Eq, Show)

-- | A type as the page declares it. One constructor is a @data@ statement and the constructor IS
-- the type; several are one @class@ with the arms inline, which is how @stack\/core\/bool.ct@
-- writes @class(Boolean, [True, False])@ — rather than a class and a @data@ per arm, which would be
-- the same thing said in more places.
data CtDecl = CtDecl {
    cdPath :: String,
    cdCons :: [CtCon],
    -- | What the page says about the type, beneath its declaration: one entry per paragraph.
    cdDoc  :: [String]
  } deriving (Eq, Show)

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
    cpDecls   :: [CtDecl]
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
  ctTypeRef :: Proxy a -> CtTypeRef
  ctTypeRef = ctRef . ctPath

  -- | What a page holding the type must import for 'ctTypeRef' to name something. Nothing for a
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
  --
  -- Polymorphic in the runtime half of a value: a converted value names no closure and no action, so
  -- it belongs to whichever evaluation asks for it and needs no coercion at the boundary.
  toCt :: a -> Val r
  default toCt :: (Generic a, GCtCons (Rep a), GCtDatatype (Rep a)) => a -> Val r
  toCt x = gToCt (ctOptsOf (Proxy :: Proxy a)) (from x)

  -- | The Haskell value a Catln one is, or why it is not one.
  fromCt :: (RuntimeValue r) => Val r -> Either String a
  default fromCt :: (Generic a, GCtCons (Rep a), GCtDatatype (Rep a), RuntimeValue r) => Val r -> Either String a
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

-- | A page given a type which declares something of its own, written rather than read off a shape.
--
-- For a type whose Catln form is not its Haskell form: a Haskell pair carries its parts positionally
-- and a Catln one has to name them, so what the page declares is a record no Haskell type is the
-- 'GHC.Generics.Rep' of. 'ctPageInto' cannot derive that, and 'ctPageImport' would leave the page
-- naming a type nothing declares.
--
-- Marked seen by the same path as 'ctPageInto', so a container reached at twenty fields declares
-- itself once.
ctPageDeclare :: (CtType a) => CtDecl -> Proxy a -> CtPage -> CtPage
ctPageDeclare decl p page
  | ctPath p `elem` cpSeen page = page
  | otherwise = page{
      cpSeen = cpSeen page ++ [ctPath p],
      cpImports = cpImports page `union` ctImports p,
      cpDecls = cpDecls page ++ [decl]
    }

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

-- | The declaration of a type, with whatever it says about itself.
ctDeclOf :: forall a. (CtType a, Generic a, GCtCons (Rep a)) => Proxy a -> CtDecl
ctDeclOf p = CtDecl (ctPath p) (gCtCons (ctOptsOf p) (Proxy :: Proxy (Rep a))) (ctDoc p)

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
  gToCt :: CtOpts -> f p -> Val r
  -- | The value read back, or 'Nothing' where the name is no constructor of this branch — which is
  -- what distinguishes a wrong constructor from a right one whose arguments do not fit.
  gFromCt :: (RuntimeValue r) => CtOpts -> String -> Args r -> Maybe (Either String (f p))

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
  gCtCons opts _ = [CtCon (coCon opts $ conName (undefined :: M1 C c f p)) [] (gCtFields opts (Proxy :: Proxy f))]
  gCtConsPage _ = gCtFieldsPage (Proxy :: Proxy f)
  gToCt opts (M1 x) = TupleVal (coCon opts $ conName (undefined :: M1 C c f p)) (gToArgs opts x)
  gFromCt opts n as
    | n == coCon opts (conName (undefined :: M1 C c f p)) = Just $ M1 <$> gFromArgs opts as
    | otherwise = Nothing

-- | The fields of one constructor.
class GCtFields (f :: Type -> Type) where
  -- | Each argument by the name it is declared under and the type it is declared as.
  gCtFields :: CtOpts -> proxy f -> [(String, CtTypeRef)]
  -- | The pages and declarations the fields' own types reach, which is where a page gets the
  -- declaration of a record held inside another record.
  gCtFieldsPage :: proxy f -> CtPage -> CtPage
  -- | The arguments a value carries, keyed as an evaluated value keys them.
  gToArgs :: CtOpts -> f p -> Args r
  -- | The fields read back out of those arguments.
  gFromArgs :: (RuntimeValue r) => CtOpts -> Args r -> Either String (f p)

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
  gCtFields opts _ = [(fieldNameOf opts (Proxy :: Proxy name), ctTypeRef (Proxy :: Proxy t))]
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

-- | A 'String' is @\/Data\/String@ rather than a @\/Data\/List@ of characters: the core declares a
-- string as a type of its own holding its characters, so that it displays as its text.
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
  ctTypeRef _ = ctElemTypeRef ctMaybePath (Proxy :: Proxy a)
  ctImports _ = [coreMaybePage]
  ctPageInto _ = ctPageInto (Proxy :: Proxy a) . ctPageImport (Proxy :: Proxy (Maybe a))
  toCt Nothing = TupleVal ctMaybeNothing H.empty
  toCt (Just x) = TupleVal ctMaybeSome $ H.singleton (runtimeArgKey ctMaybeSomeValue) (toCt x)
  fromCt (TupleVal n _) | n == ctMaybeNothing = Right Nothing
  fromCt (TupleVal n as) | n == ctMaybeSome = Just <$> (fromCt =<< valArg n ctMaybeSomeValue as)
  fromCt v = Left $ printf "%s is not a %s" (show v) ctMaybePath

-- | A list is a @\/Data\/Cons@ chain ending in a @\/Data\/Nil@, which 'Semantics.Val.valListOf' and
-- 'Semantics.Val.valList' are already the one place that knows.
instance {-# OVERLAPPABLE #-} (CtType a) => CtType [a] where
  ctPath _ = ctListPath
  ctTypeRef _ = ctElemTypeRef ctListPath (Proxy :: Proxy a)
  ctImports _ = [coreListPage]
  ctPageInto _ = ctPageInto (Proxy :: Proxy a) . ctPageImport (Proxy :: Proxy [a])
  toCt = valListOf . map toCt
  fromCt v = traverse fromCt =<< valList v

-- | A parameterized type as it is written where it is used: its path with its element type applied
-- to 'CtConstants.ctElemVar', which is @\/Data\/List[$T -> \/Catln\/BuildFile]@ in
-- @stack\/core\/result.ct@.
ctElemTypeRef :: (CtType a) => String -> Proxy a -> CtTypeRef
ctElemTypeRef path p = CtTypeRef path [(ctElemVar, ctTypeRef p)]

-- | A character is @\/Data\/Char@, which @stack\/core\/char.ct@ declares.
--
-- Reached through 'Semantics.Types.Constant', whose @CChar@ arm a program writes as a character
-- literal such as @'a'@.
instance CtType Char where
  ctPath _ = charPrim
  ctImports _ = [coreStrPage]
  ctPageInto = ctPageImport
  toCt = CharVal
  fromCt (CharVal c) = Right c
  fromCt v           = Left $ printf "%s is not a %s" (show v) charPrim

-- | The two parts of a pair, named, since a Catln argument is named and a Haskell pair's are not.
--
-- @\/Catln\/Pair@ is declared by the compiler rather than in a @.ct@ file because it exists for the
-- sake of what the compiler has to hand over: a map is a list of these, and the program AST is full
-- of anonymous pairs. Naming them here rather than renaming them in Haskell is what keeps the
-- reading from costing the compiler's own types a rewrite — @TupleApply@ carries a
-- @(Meta m, Expr m)@ at some hundred sites, and giving those two halves Catln names does not have
-- to disturb any of them.
ctPairPath, ctPairFirst, ctPairSecond, ctPairFirstVar, ctPairSecondVar :: String
ctPairPath = ctBuiltinNamespace ++ "Pair"
ctPairFirst = "first"
ctPairSecond = "second"
ctPairFirstVar = "$A"
ctPairSecondVar = "$B"

-- | @data(\/Catln\/Pair[$A, $B](first -> $A, second -> $B))@, written rather than derived: no
-- Haskell type has this shape, since the Haskell pair it stands for names nothing.
ctPairDecl :: CtDecl
ctPairDecl = CtDecl ctPairPath [CtCon ctPairPath [ctPairFirstVar, ctPairSecondVar] [
    (ctPairFirst, ctRef ctPairFirstVar),
    (ctPairSecond, ctRef ctPairSecondVar)
                                                                                   ]] [
  "Two values together, the [first] and the [second].",
  "It is declared by the compiler because it is what the compiler has to hand over. A map is a list of these, and the program the compiler is compiling holds pairs in a dozen places that have no names of their own in Haskell. Naming them here is what lets those places be read without being rewritten."
                                                                                      ]

instance (CtType a, CtType b) => CtType (a, b) where
  ctPath _ = ctPairPath
  ctTypeRef _ = CtTypeRef ctPairPath [
    (ctPairFirstVar, ctTypeRef (Proxy :: Proxy a)),
    (ctPairSecondVar, ctTypeRef (Proxy :: Proxy b))
                                     ]
  ctPageInto _ = ctPageInto (Proxy :: Proxy b)
               . ctPageInto (Proxy :: Proxy a)
               . ctPageDeclare ctPairDecl (Proxy :: Proxy (a, b))
  toCt (x, y) = TupleVal ctPairPath $ H.fromList [
    (runtimeArgKey ctPairFirst, toCt x),
    (runtimeArgKey ctPairSecond, toCt y)
                                                 ]
  fromCt (TupleVal n as) | n == ctPairPath =
    (,) <$> (fromCt =<< valArg n ctPairFirst as) <*> (fromCt =<< valArg n ctPairSecond as)
  fromCt v = Left $ printf "%s is not a %s" (show v) ctPairPath

-- | A value carrying nothing.
--
-- Declared rather than mapped onto the anonymous type @()@ that @stack\/core\/primitives.ct@ says
-- is the unit type. A field of this type is one the compiler HAS and has nothing in, which is a
-- thing worth being able to name and read; the anonymous type is a matching form as much as a
-- value, and reading a field as one would be saying something about how it matches rather than
-- about what it holds.
ctUnitPath :: String
ctUnitPath = ctBuiltinNamespace ++ "Unit"

ctUnitDecl :: CtDecl
ctUnitDecl = CtDecl ctUnitPath [CtCon ctUnitPath [] []] [
  "A value carrying nothing.",
  "What a field whose contents the compiler does not keep is handed over as. A program is given the field, so that what it reads has the shape the compiler holds, and there is nothing in it."
                                                        ]

instance CtType () where
  ctPath _ = ctUnitPath
  ctPageInto = ctPageDeclare ctUnitDecl
  toCt () = TupleVal ctUnitPath H.empty
  fromCt (TupleVal n _) | n == ctUnitPath = Right ()
  fromCt v = Left $ printf "%s is not a %s" (show v) ctUnitPath

-- | An identifier is the text of it.
--
-- Its structure is not the compiler's: a 'UUID' is sixteen bytes whose halves mean nothing on their
-- own, so handing over the halves would be handing over an accident of how it is stored. The text is
-- the whole of what one is FOR — telling one apart from another and saying which — and it reads back
-- as the same identifier or is refused.
instance CtType UUID where
  ctPath _ = strPrim
  ctImports _ = [coreStrPage]
  ctPageInto = ctPageImport
  toCt = StrVal . UUID.toString
  fromCt (StrVal s) = maybe (Left $ printf "%s is not an id" (show s)) Right (UUID.fromString s)
  fromCt v          = Left $ printf "%s is not a %s" (show v) strPrim

-- | Three values together, for the same reason a pair is two.
--
-- A separate declaration rather than a pair of a pair: @Pair[$A, Pair[$B, $C]]@ would be read
-- @second.first@ where the Haskell it stands for is read @b@, so the shape a program is handed
-- would be an artifact of there being no triple rather than of anything about the value.
ctTriplePath, ctTripleThird, ctTripleThirdVar :: String
ctTriplePath = ctBuiltinNamespace ++ "Triple"
ctTripleThird = "third"
ctTripleThirdVar = "$C"

ctTripleDecl :: CtDecl
ctTripleDecl = CtDecl ctTriplePath [CtCon ctTriplePath [ctPairFirstVar, ctPairSecondVar, ctTripleThirdVar] [
    (ctPairFirst, ctRef ctPairFirstVar),
    (ctPairSecond, ctRef ctPairSecondVar),
    (ctTripleThird, ctRef ctTripleThirdVar)
                                                                                                          ]] [
  "Three values together, the [first], the [second] and the [third].",
  "Declared beside [/Catln/Pair] and for its reason. It is not a pair holding a pair: that would be read as [second.first] where what it stands for is read as one thing, and the shape handed over would say something about there being no triple rather than about the value."
                                                                                                             ]

instance (CtType a, CtType b, CtType c) => CtType (a, b, c) where
  ctPath _ = ctTriplePath
  ctTypeRef _ = CtTypeRef ctTriplePath [
    (ctPairFirstVar, ctTypeRef (Proxy :: Proxy a)),
    (ctPairSecondVar, ctTypeRef (Proxy :: Proxy b)),
    (ctTripleThirdVar, ctTypeRef (Proxy :: Proxy c))
                                       ]
  ctPageInto _ = ctPageInto (Proxy :: Proxy c)
               . ctPageInto (Proxy :: Proxy b)
               . ctPageInto (Proxy :: Proxy a)
               . ctPageDeclare ctTripleDecl (Proxy :: Proxy (a, b, c))
  toCt (x, y, z) = TupleVal ctTriplePath $ H.fromList [
    (runtimeArgKey ctPairFirst, toCt x),
    (runtimeArgKey ctPairSecond, toCt y),
    (runtimeArgKey ctTripleThird, toCt z)
                                                      ]
  fromCt (TupleVal n as) | n == ctTriplePath =
    (,,) <$> (fromCt =<< valArg n ctPairFirst as)
         <*> (fromCt =<< valArg n ctPairSecond as)
         <*> (fromCt =<< valArg n ctTripleThird as)
  fromCt v = Left $ printf "%s is not a %s" (show v) ctTriplePath

-- | A set is the list of what is in it, in order.
--
-- Sorted rather than left as the set iterates. A 'S.HashSet' has no order of its own, so the value
-- a program is handed would otherwise depend on hashing — which makes a golden record whichever
-- order this build happened to produce, and makes two equal sets convert to two unequal values.
-- 'Ord' is what pays for that, and every key the compiler puts in a set has one.
instance (CtType a, Ord a, Hashable a) => CtType (S.HashSet a) where
  ctPath _ = ctListPath
  ctTypeRef _ = ctElemTypeRef ctListPath (Proxy :: Proxy a)
  ctImports _ = [coreListPage]
  ctPageInto _ = ctPageInto (Proxy :: Proxy a) . ctPageImport (Proxy :: Proxy (S.HashSet a))
  toCt = valListOf . map toCt . sort . S.toList
  fromCt v = S.fromList <$> (traverse fromCt =<< valList v)

-- | A map is the list of its entries, each an entry of @\/Catln\/Pair@, in order of key.
--
-- Sorted for 'S.HashSet''s reason, and by the key because that is what a map is keyed by and so
-- what makes the order say something rather than merely be fixed.
instance (CtType k, CtType v, Ord k, Hashable k) => CtType (H.HashMap k v) where
  ctPath _ = ctListPath
  ctTypeRef _ = ctElemTypeRef ctListPath (Proxy :: Proxy (k, v))
  ctImports _ = [coreListPage]
  ctPageInto _ = ctPageInto (Proxy :: Proxy (k, v)) . ctPageImport (Proxy :: Proxy (H.HashMap k v))
  toCt = valListOf . map toCt . sortOn fst . H.toList
  fromCt v = H.fromList <$> (traverse fromCt =<< valList v)
