--------------------------------------------------------------------
-- |
-- Module    :  Testing.Generation
-- Copyright :  (c) Zach Kimberg 2023
-- License   :  MIT
-- Maintainer:  zachary@kimberg.com
-- Stability :  experimental
-- Portability: non-portable
--
-- This module defines arbitrary code generation for property testing.
-- It helps test both the compiler itself and code written in it.
--------------------------------------------------------------------

module Testing.Generation where

import           Control.Monad
import           CtConstants
import           Data.Either             (partitionEithers)
import           Data.Graph              (graphFromEdges)
import qualified Data.HashMap.Strict     as H
import qualified Data.HashSet            as S
import           Data.List               (partition)
import qualified Data.List               as L
import           Data.Maybe
import           Eval.Common             (Val)
import           Hedgehog
import qualified Hedgehog.Gen            as HG
import           Hedgehog.Range          (linear, linearFrac)
import           Semantics               (classGraphFromObjs, mkTypeEnv)
import           Semantics.Prgm
import           Semantics.Types
import           Semantics.Val           hiding (Val)
import           Syntax.Ct.Desugarf.Expr (desFileImport)
import           Syntax.Ct.Prgm          (RawExpr (RawCExpr), mkRawFileImport)
import           Text.Printf
import           Utils

-- | A fixed pool of variable names shared by every generator that writes or binds a variable
-- ('genTypeVar', 'genWithTVExtOn', 'genVaenv'), so a good fraction of the generated references
-- name a variable that some generated environment actually binds.
varNamePool :: [Name]
varNamePool = map mkName ["a", "b", "c"]

-- | The shape a type draw is held to. Every constraint here is met BY CONSTRUCTION: a source
-- that could violate it is left out of the weight table, never drawn and rejected. A rejected
-- draw is a discard charged to the property, and a property that exhausts its discard budget
-- reports "gave up after N discards" — a FAIL that says nothing about the algebra, and one that
-- raising the iteration count manufactures on its own. The same rule governs the program draw
-- ('PrgmNeed') and the placement generators below.
data GenSpec = GenSpec
  { gsGround  :: Bool   -- ^ no type variable anywhere: no bare variable, no injected 'TVExt'
                        -- slot, and no declared slot whose meta carries one
  , gsConsts  :: Bool   -- ^ constants may appear, positively or as a top's exclusions
  , gsExtPool :: [Name] -- ^ the names a bare variable or an injected 'TVExt' slot may use;
                        -- empty means neither is drawn
  }

defaultSpec :: GenSpec
defaultSpec = GenSpec{gsGround = False, gsConsts = True, gsExtPool = varNamePool}

-- | Bare type variables as direct inputs ('TVVar'\/'TVArg' × 'TVInt'\/'TVExt'). With no
-- external env a variable's standalone reading is unconstrained, i.e. ⊤ ('vaenvLookup').
genTypeVar :: Prgm Expr () -> Gen Type
genTypeVar = genTypeVarWith defaultSpec

-- | 'genTypeVar' under a 'GenSpec'. The caller offers this generator only when the spec's pool
-- is non-empty ('genTypeWith').
genTypeVarWith :: GenSpec -> Prgm Expr () -> Gen Type
genTypeVarWith spec prgm = do
  n   <- HG.element (gsExtPool spec)
  aux <- HG.element [TVVar (partialKey n), TVArg (partialKey n)]
  loc <- HG.element [TVInt, TVExt]
  -- The occurrence-local INNER (the occurrence denotes var ∩ inner): ⊤, carrying no knowledge,
  -- two thirds of the time, else a small union type. A union draw is never a bare variable, ⊤
  -- or ⊥ ('genUnionType' is non-degenerate), which is what an inner needs: producers of an
  -- inner install only closed values; ⊤ is the other branch; and a ⊥ inner would make the
  -- occurrence empty and every law about it trivial.
  inner <- HG.frequency [(2, return PTopType), (1, HG.small (genUnionTypeWith spec prgm))]
  return $ TypeVar aux loc inner

-- | The four argument-set intervals ('PtArgMode'). Each is exact under all the set operations:
-- the meet of a lower-bounded partial with one typing an arg it need not have
-- (@F(x).. ∩ F(y=Y)..??@) is the lower-bounded partial with that arg optional (@F(x, y?=Y)..@).
argModes :: [PtArgMode]
argModes = [PtArgExact, PtArgAny, PtArgMode False True S.empty, PtArgUnbounded]

-- | A partial re-moded to one of the 'argModes'. A lower-bounded one makes some of its listed
-- args optional ('ptArgOpt'), as a meet or difference does, kept canonical as they keep it.
remodePartial :: TypeGraph tg => TypeEnv tg -> PartialType -> Gen PartialType
remodePartial typeEnv p = do
  mode <- HG.element argModes
  -- the lower bound is vacuous with no args, so keep it set then (avoids producing a
  -- representation set-equivalent to a different one)
  let mode' = if H.null (ptArgs p) then mode{ptArgLb=True} else mode
  opt <- if ptArgLb mode' then S.fromList <$> HG.subsequence (H.keys (ptArgs p)) else return S.empty
  return $ withOptionalArgs typeEnv opt p{ptArgMode = mode'}

-- | A constant drawn from the constant kinds a type can hold.
--
-- No string constant: a string is a list of characters and not a primitive, so a type never holds
-- one ('Semantics.Types.constantType' types a string literal as a string of any characters). A type
-- built with one is a type nothing can produce.
genConstant :: Gen Constant
genConstant = HG.choice [ CInt <$> HG.integral (linear (-100) 100)
                        , CFloat <$> HG.double (linearFrac (-100) 100)
                        , CChar <$> HG.unicode ]

-- | Constants deduplicated, keeping the first occurrence: a repeated constant in a union is not a
-- different type, and two representations of one set would make the compaction laws compare
-- syntactically distinct equals.
nubConsts :: [Constant] -> [Constant]
nubConsts = foldr (\c acc -> if c `elem` acc then acc else c : acc) []

-- | A class-graph node's name as a positive LEAF spells it. The desugarer keys some class-graph
-- nodes by the source's relative name (@Point@ — 'desMultiTypeDef', 'desClassDecl') while the
-- object map holds the absolute one (@/Point@), and the compiler resolves every type-position name
-- before the algebra sees it. A relatively-spelled leaf is therefore a form the algebra never
-- receives, and it judges one as OUTSIDE the universe ('relNameAdmits' admits only universe
-- names), so laws such as @~Point ∩ Point = Point@ would fail on the spelling alone. A node name
-- that resolves to exactly one object path (a name may carry several declarations) is drawn
-- under that path's spelling; relation and class atoms keep the node's own spelling, which the
-- compiler does produce.
leafSpelling :: Prgm Expr () -> TypeName -> TypeName
leafSpelling (Prgm objMap _ _) n = case L.nub (relativeNameFilter n (map oaObjPath (flatObjectMap objMap))) of
  [absolute] -> absolute
  _          -> n

-- | Partition the class graph's nodes. 'graphToNodes' returns BOTH the classes ('CGClass') and the
-- data types that are instances of one ('CGType'), and the two are not interchangeable in a type:
--
--   * a CLASS name in type position resolves to a @PredClass@ top, never a positive leaf, and
--   * a DATA-TYPE name resolves to a positive leaf, never a @PredClass@
--
-- — the rule 'resolveRelPartial' implements. A 'PredClass' over a data type errors out of
-- 'expandPartial' (@Unknown class@), and a class drawn as a bare 'PartialType' is a leaf the
-- compiler never produces. A 'PredRel' is well-formed over either kind of node.
classNodeNames, dataNodeNames :: ClassGraph -> [TypeName]
classNodeNames (ClassGraph cg) = [fromPartialName k | (CGClass{}, k, _) <- graphToNodes cg]
dataNodeNames  (ClassGraph cg) = [fromPartialName k | (n, k, _) <- graphToNodes cg, notClass n]
  where notClass CGClass{} = False
        notClass _         = True

-- | The number of type parameters and of fields a declaration spells, as 'genTypeFromExprWith'
-- reads them.
declShape :: Expr () -> (Int, Int)
declShape = go 0 0
  where
    go nv na (TupleApply _ (_, b) (EAppVar _ _)) = go (nv + 1) na b
    go nv na (TupleApply _ (_, b) (EAppArg _))   = go nv (na + 1) b
    go nv na _                                   = (nv, na)

-- | Whether a declaration has a usable (binder, payload-slot) placement for
-- 'genOwnVarBoundPair': at least one type parameter, and either a field or a second parameter.
--
-- Callers select declarations with this BEFORE drawing ('genPlacementWith'), so that a program
-- whose declarations are mostly nullary still yields a usable partial on every draw.
declHasPlacement :: Expr () -> Bool
declHasPlacement e = let (nv, na) = declShape e in nv >= 1 && (na >= 1 || nv >= 2)

-- | A placement whose payload slot is a FIELD: at least one parameter and one field. What
-- 'genParentVarBoundPair' places into.
declHasFieldPlacement :: Expr () -> Bool
declHasFieldPlacement e = let (nv, na) = declShape e in nv >= 1 && na >= 1

-- | A program carrying at least one placement declaration.
prgmHasPlacement :: Prgm Expr () -> Bool
prgmHasPlacement = prgmSatisfies placementNeed

-- | A program 'genPartialType' can draw from at all. With neither a data node nor an object
-- declaration there is nothing to build a positive leaf out of.
prgmHasPartial :: Prgm Expr () -> Bool
prgmHasPartial (Prgm om cg _) = not (null (dataNodeNames cg)) || not (nullObjectMap om)

-- | What a property needs the program it draws to contain, so that EVERY draw can state its
-- law. Both program sources satisfy a need by construction: 'genPrgmWith' forces the required
-- shape into the program it generates, and a fixture file is offered only when 'prgmSatisfies'
-- holds for it. Drawing a program and rejecting it instead (a @HG.filter@ over the program
-- generator, or a @discard@ in the property) charges the rejection to the property's discard
-- budget; roughly a third of generated programs and one of the fixture files carry no class at
-- all, so the class laws gave up once the iteration count was raised — the reason #40 was twice
-- triaged as a class\/negation gap when neither property had ever found a counterexample, and
-- why 'propExtensionKeepsProofs' gave up at 1000 discards while discarding every draw of
-- @containers.ct@.
data PrgmNeed = PrgmNeed
  { pnClass  :: Maybe ClassNeed -- ^ a class node of this shape
  , pnDecls  :: [(Int, Int)]    -- ^ alternatives @(min parameters, min fields)@, one of which
                                -- some declaration meets; empty for no requirement
  , pnObject :: Bool            -- ^ something a positive leaf can be built from ('prgmHasPartial')
  }

-- | A class need: its sealedness (or either) and the fewest members it has.
data ClassNeed = ClassNeed { cnSealed :: Maybe Bool, cnMinMembers :: Int }

noNeed, objectNeed, memberClassNeed, sealedClassNeed, openClassNeed, placementNeed, fieldPlacementNeed :: PrgmNeed
noNeed             = PrgmNeed Nothing [] False
objectNeed         = noNeed{pnObject = True}
memberClassNeed    = noNeed{pnClass = Just (ClassNeed Nothing 1)}
sealedClassNeed    = noNeed{pnClass = Just (ClassNeed (Just True) 1)}
openClassNeed      = noNeed{pnClass = Just (ClassNeed (Just False) 0)}
placementNeed      = noNeed{pnDecls = [(1, 1), (2, 0)]}
fieldPlacementNeed = noNeed{pnDecls = [(1, 1)]}

-- | Whether a program meets a need; the test for a fixture file, and what 'genPrgmWith' holds
-- a generated program to.
prgmSatisfies :: PrgmNeed -> Prgm Expr () -> Bool
prgmSatisfies PrgmNeed{pnClass, pnDecls, pnObject} prgm@(Prgm om (ClassGraph cg) _) =
  maybe True classOk pnClass && (null pnDecls || any declOk decls) && (not pnObject || prgmHasPartial prgm)
  where
    classOk ClassNeed{cnSealed, cnMinMembers} =
      or [ maybe True (== sealed) cnSealed && length cts >= cnMinMembers | (CGClass sealed _ cts _, _, _) <- graphToNodes cg ]
    decls = [ declShape oe | oa <- flatObjectMap om, Just oe <- [oaObj oa] ]
    declOk (nv, na) = any (\(mv, ma) -> nv >= mv && na >= ma) pnDecls

-- | A pair of types that MUST be indistinguishable to every operation: the same partial, binding
-- one of its own type variables to a ground bound, holding at one nested position either a
-- reference to that variable or the bound itself:
--
-- > B[/v=K](slot = $v)   ≡   B[/v=K](slot = K)
--
-- 'ptVars' is a BINDER — inside the partial that declares it, @$v@ denotes its binding (the rule
-- 'isSubPartialOfWithEnv'.'readOwnScope' implements). A traversal that descends into a partial
-- without extending its environment with that partial's own 'ptVars' reads @$v@ as ⊤ instead
-- ('vaenvLookup' defaults to ⊤), and because ⊤ is the lattice top, forgetting always WIDENS: such
-- a prover over-approximates membership and under-approximates difference.
--
-- Two axes are drawn independently, because the binder is consulted (or forgotten) separately on
-- each:
--
--   * the OUTER form the binding partial takes — a positive leaf, a @PredClass@ top, or a
--     @PredRel@ top. The predicate forms matter because complementing a partial turns it into a
--     predicate, and the same-name implication rules in 'predImpliesAtom' compare the two
--     partials' slots directly. Those rules are where a forgotten binder is invisible: a positive
--     leaf reaches the binding through 'intersectPartialsBase' instead.
--   * the POSITION of the reference within the slot — the whole slot, a nested positive partial's
--     dimension, or a class predicate's dimension.
--
-- Every key written is one the base's DECLARATION already has. A partial's 'ptVars' and 'ptArgs'
-- keys are its declaration's parameters and fields (declaration-boundedness), so a binder under an
-- invented name — @/Cons[/T=K, /extra=K](/head=$/extra)@ where @/Cons@ declares only @/T@ — is a
-- leaf no value carries, and the operations may legitimately disagree about it.
--
-- The sibling generator that places the nested reference as a 'TVParent' — reaching the enclosing
-- parameter THROUGH a shadowing inner scope rather than avoiding one — is 'genParentVarBoundPair'.
genOwnVarBoundPair :: Prgm Expr () -> Gen (Type, Type)
genOwnVarBoundPair prgm@(Prgm objMap _ _) = do
  -- 'genPlacement' forces every declared slot in, so a placement declaration always offers a
  -- binder placement.
  base    <- genPlacement prgm
  (varName, slot) <- HG.element (binderPlacements base)
  bound   <- genGroundBound prgm
  (loc, wrap) <- HG.choice (bareSlot : nestedPlacements varName slot)
  outer   <- HG.element [singletonType, classPartial, relPartial]
  let withSlot v = placeInSlot outer base varName bound slot (wrap v)
  return (withSlot (TypeVar (TVVar varName) loc PTopType), withSlot bound)
  where
    TypeEnv{teClassGraph=ClassGraph cg} = mkTypeEnv prgm
    -- The reference's SCOPE CLASS is decided by where it is placed, as the desugarer decides it
    -- ('tagParentVarRefs'): directly in the base's slot it names the base's own parameter and is
    -- 'TVInt'; inside a nested partial's (or class atom's) PARAMETER value it sits at that nested
    -- partial's level, one level inside the base, and is the 'TVParent' @$^v@. A nested placement
    -- is drawn only when the payload slot is a FIELD: held in a second PARAMETER of the base, the
    -- nested partial sits at the base's own level and a one-level parent reference would name the
    -- base's parent instead ('genParentVarBoundPair' explains the same register rule).
    --
    -- Each nested placement is offered only when the program has a declaration it can be built
    -- from, chosen BEFORE the draw: a declaration with a parameter not named like the binder
    -- for 'nestedDim', a class with such a parameter for 'classPredDim'. On a program whose
    -- parameters are all one name (@recursiveClassBounds.ct@ declares only @/T@) the payload
    -- can only be the whole slot — a fixture coverage limit, not a discard.
    nestedPlacements varName (Left _) = [nestedDim varName | not (null (nestedDims varName))] ++ [classPredDim varName | any ((/= [varName]) . snd) classDims]
    nestedPlacements _ (Right _)      = []
    -- The payload IS the slot.
    bareSlot = return (TVInt, id)
    -- The payload is a dimension of a nested positive partial, again one the nested partial
    -- declares, and not the binder's name: a nested partial declaring the SAME name is the
    -- shadowing shape 'genParentVarBoundPair' owns.
    nestedDims varName = [ (oe, dim) | oa <- flatObjectMap objMap, Just oe <- [oaObj oa], dim <- declaredVars oe, dim /= varName ]
    nestedDim varName = do
      (oe, dim) <- HG.element (nestedDims varName)
      p <- genWithTVExtOn prgm (genTypeFromExpr prgm oe)
      -- A coin-flipped slot named like the binder is dropped (fewer spelled slots is a
      -- legitimate partial); the chosen dimension is then written in.
      return (TVParent, \v -> singletonType p{ptVars = H.insert dim v (H.delete varName (ptVars p))})
    -- The payload is a dimension of a CLASS PREDICATE ('classPartial' builds the @∀C[..]@ form).
    -- CLASS nodes only — a 'PredClass' over a data type is not well-formed ('classNodeNames') —
    -- and the dimension must be one the CLASS declares.
    classDims = [ (fromPartialName k, ks)
                | (CGClass _ cpt _ _, k, _) <- graphToNodes cg
                , let ks = H.keys (ptVars cpt), not (null ks) ]
    classPredDim varName = do
      (className, ks) <- HG.element [c | c@(_, ks') <- classDims, any (/= varName) ks']
      dim <- HG.element (filter (/= varName) ks)
      return (TVParent, \v -> classPartial (partialVal className){ptVars = H.singleton dim v})

-- | Where a binder-and-payload pair can go in a partial: both DECLARED keys of the same partial
-- and necessarily distinct. The payload prefers a declared FIELD ('Left'); when the declaration
-- has none it takes a SECOND declared parameter ('Right'), so declarations without fields still
-- contribute.
binderPlacements :: PartialType -> [(TypeVarName, Either ArgName TypeVarName)]
binderPlacements b =
  [ (v, Left k)  | v <- H.keys (ptVars b), k <- H.keys (ptArgs b) ] ++
  [ (v, Right k) | v <- H.keys (ptVars b), k <- H.keys (ptVars b), k /= v ]

-- | The base with its binder bound and the payload written into the chosen slot, under one of the
-- three outer forms ('singletonType', 'classPartial', 'relPartial').
placeInSlot :: (PartialType -> Type) -> PartialType -> TypeVarName -> Type -> Either ArgName TypeVarName -> Type -> Type
placeInSlot outer base varName bound slot payload = outer base
  { ptVars = H.insert varName bound (case slot of
      Right k -> H.insert k payload (ptVars base)
      Left _  -> ptVars base)
  , ptArgs = case slot of
      Left k  -> H.insert k payload (ptArgs base)
      Right _ -> ptArgs base
  }

-- | The @∀r[..]@ relation-predicate form, the sibling of 'classPartial'.
relPartial :: PartialType -> Type
relPartial p = TopType H.empty (PredsOne (PredRel p))

-- | A GROUND bound for a binder: a bound that itself mentions a variable would make the two
-- members of a pair differ by more than the one reference under test.
genGroundBound :: Prgm Expr () -> Gen Type
genGroundBound prgm = HG.small (genUnionTypeWith defaultSpec{gsGround = True} prgm)

-- | A ground, constant-free bound: a positive leaf union or a predicate top (a class or
-- relation bound, possibly beside leafs). 'genParentVarBoundPair' binds its parent parameter
-- with one of these, and the law binds its decoy with one; see 'genParentVarBoundPair' for
-- the one shape left out.
genGroundLeafBound :: Prgm Expr () -> Gen Type
genGroundLeafBound prgm = HG.small (genUnionTypeWith defaultSpec{gsGround = True, gsConsts = False} prgm)

-- | The type parameters an object expression declares, in application order.
declaredVars :: Expr () -> [TypeVarName]
declaredVars (TupleApply _ (_, b) (EAppVar v _)) = v : declaredVars b
declaredVars (TupleApply _ (_, b) _)             = declaredVars b
declaredVars (EWhere _ b _)                      = declaredVars b
declaredVars _                                   = []

-- | The sibling of 'genOwnVarBoundPair' for the PARENT scope: a pair that must be indistinguishable
-- to every operation, where the reference reaches the enclosing partial's parameter THROUGH a
-- nested partial that declares the SAME name — the spelling a recursive declaration uses for its
-- recursive field, as in @/Cons@'s @/tail -> ∀/ConsList[/T=$^/T]@:
--
-- > B[/v=K](slot = N[/v=$^/v])   ≡   B[/v=K](slot = N[/v=K])
--
-- The nested partial's parameter VALUE is written at the level of the slot holding it (a
-- 'ptVars' value sits at its partial's own level — the register rule every scoped walker
-- implements, 'substitutePartialTVExtScoped'), so a 'TVParent' there names the ENCLOSING
-- partial's parameter: not the nested partial's like-named one (that is what the spelling is
-- for — a 'TVInt' at the same position is the self-entry convention @/T=$/T@), and not a
-- like-named ambient binding ('TypeVarLoc': a parent reference is never capturable by the
-- ambient env). The nested partial is drawn from the declarations that declare the name, the
-- base's own always among them, so the same declaration nested in itself (@Cons@ in @Cons@) is
-- in the distribution.
--
-- The nested partial is placed in a FIELD of the base only. Held in a second PARAMETER of the
-- base instead (@Pair[/A=Pair[/B=$^/B]]@), the same register rule puts the nested partial at
-- the base's own level, so its parent reference reaches past the base to the base's parent, which
-- at the top of a generated type does not exist. That shape is not the idiom and has no single
-- denotation to assert.
--
-- Returns a THIRD member too — the same shape with an unrelated ground bound in the nested
-- parameter — and the parameter's name. The identity alone cannot tell a reference resolved to
-- the parent's binding from one not resolved at all: an unbound variable slot is read as FLEXIBLE
-- (bindable per value — 'isFlexibleVar') and matches anything, so a reader that lost the parent
-- would still pass both directions. Against the third member the reference must answer exactly
-- as its binding does. It also checks the two bounds are compared as the sets they are: with
-- @K = Cons@ and an unrelated @K' = Nil@, @B[/v=Cons](slot = N[/v=Cons])@ (a list of lists) and
-- @B[/v=Cons](slot = N[/v=Nil])@ are different types and no operation may equate them. The name
-- lets the law bind it in the ambient env to a decoy and assert the reference is not captured.
--
-- The bound and the decoy are ground and constant-free ('genGroundLeafBound'): data leafs, bare
-- names included, and CLASS or RELATION bounds, whose membership questions must be decided
-- open-world on every route for the law to hold. A decoy that is a predicate top is what reaches
-- a reader resolving a partial's own parameter through the ambient env. One shape is not drawn:
-- a CONSTANT-only bound, where comparing a recursive declaration whose element is a constant
-- (@Cons[/T={c}](head=Cons[/T={c}](..))@) does not terminate.
--
-- A BARE top-level 'TVParent' is not generated anywhere: a fragment's top has no enclosing
-- partial, so a parent reference there is unbound. An unbound parent reference is an erroneous
-- state, not a value with a denotation for a law to assert.
genParentVarBoundPair :: Prgm Expr () -> Gen (Type, Type, Type, TypeVarName)
genParentVarBoundPair prgm@(Prgm objMap _ _) = do
  -- Drawn from the declarations that HAVE a field ('declHasFieldPlacement'); a two-parameter
  -- field-less declaration offers no field placement, so it is not drawn and rejected.
  base    <- genPlacementWith declHasFieldPlacement prgm
  (varName, slot) <- HG.element (fieldPlacements base)
  bound   <- genGroundLeafBound prgm
  other   <- genGroundLeafBound prgm
  -- Every declared slot of the nested partial is forced in ('genTypeFromExprWith' True), as for
  -- the base: the pair must differ by the one parameter value under test and nothing else.
  nested  <- HG.element [oe | (v, oe) <- decls, v == varName] >>= genTypeFromExprWith defaultSpec True prgm
  outer   <- HG.element [singletonType, classPartial, relPartial]
  let payload v = singletonType nested{ptVars = H.insert varName v (ptVars nested)}
      withSlot v = placeInSlot outer base varName bound slot (payload v)
  return (withSlot (TypeVar (TVVar varName) TVParent PTopType), withSlot bound, withSlot other, varName)
  where
    -- The declarations that declare the name and that 'genTypeFromExprWith' can read (the
    -- same spine shape 'genPlacement' accepts); the base's own declaration is always one.
    decls = [ (v, oe) | oa <- flatObjectMap objMap, Just oe <- [oaObj oa], declHasPlacement oe, v <- declaredVars oe ]
    fieldPlacements b = [ pl | pl@(_, Left _) <- binderPlacements b ]

-- | A small external variable environment ('TypeVarArgEnv') mapping a subset of 'varNamePool'
-- to concrete-ish types drawn from the program. Threading this through the @*WithEnv@ operations
-- exercises 'vaenvLookup' (a bound variable reads as its value rather than ⊤), 'substituteTVExt'\/
-- 'substituteKeep' (external variables inside partial slots — added by 'genWithTVExtOn' from the
-- same pool — resolve), and 'applyTypeVarOverrides'.
--
-- Each pool name is bound with ~2/3 probability, so unbound variables (read as ⊤) still occur,
-- under either a 'TVVar' or 'TVArg' key. The bound values are union types (never a bare
-- variable, which could loop the variable-rewriting clauses of the operations; never ⊤, which is
-- indistinguishable from an unbound name; never ∅, which no program's variable resolves to) and
-- acyclic, as the environment of a real program is.
genVaenv :: Prgm Expr () -> Gen TypeVarArgEnv
genVaenv prgm = go [] varNamePool
  where
    -- Acyclicity at every depth, not only at the top: a value may reference only pool names
    -- bound EARLIER, so by induction no binding reaches itself. A self-reference nested inside a
    -- slot (@b ↦ Pair[/A=$_b]@) is a recursive equation rather than a value, and resolving it
    -- unfolds without end. No program's environment holds one — the algebra keeps such an
    -- equation on the occurrence instead of installing it ('comparedPartialsEnv'). The value is
    -- drawn under a spec whose pool is exactly the names bound so far, so it cannot reference a
    -- later one. Only POOL names matter: a reference to a declaration's own parameter (@$/T@) is
    -- not an environment edge at all.
    go acc [] = return $ ambientEnv $ H.fromList acc
    go acc (n:ns) = do
      bind <- HG.frequency [(2, return True), (1, return False)]
      if not bind
        then go acc ns
        else do
          aux <- HG.element [TVVar (partialKey n), TVArg (partialKey n)]
          v   <- genUnionTypeWith defaultSpec{gsExtPool = boundNames (map fst acc)} prgm
          go ((aux, v) : acc) ns
    boundNames bound = [ n | n <- varNamePool, let k = partialKey n, TVVar k `elem` bound || TVArg k `elem` bound ]

-- | Substitute every nested reference to a recursive class @cName@ (its class-predicate
-- @∀cName@\/@~cName@ form, appearing as an argument or variable value) with @replacement@. Applied
-- to a class's depth-1 member expansion with the expansion itself as @replacement@, this unrolls
-- the recursion one level deeper — the shape a recursive class accretes per constraint epoch during
-- solving (see 'Semantics.Types.foldRecursiveType').
substRecRef :: TypeName -> Type -> Type -> Type
substRecRef cName replacement = go
  where
    matchesC p = makeAbsoluteName (ptName p) == makeAbsoluteName cName
    go (TopType _ (PredsOne (PredClass rp))) | matchesC rp = replacement
    go (TopType _ (PredsOne (PredRel rp)))   | matchesC rp = replacement
    go (UnionType m pos cs) = UnionType m (joinUnionType (map goLeaf (splitUnionType pos))) cs
    go t = t
    goLeaf p = p{ptArgs = fmap go (ptArgs p), ptVars = fmap go (ptVars p)}

genTypeFromExpr :: Prgm Expr () -> Expr () -> Gen PartialType
genTypeFromExpr = genTypeFromExprWith defaultSpec False

-- | 'genTypeFromExpr' under a 'GenSpec', with a switch that forces EVERY declared parameter and
-- field of the expression into the generated partial instead of deciding each with a coin flip.
--
-- The coin flips are what a general draw wants: a partial that spells only some of its
-- declaration's slots is a legitimate value (an unspelled parameter is ⊤), and generating those
-- is most of the point. But a caller that needs a partial of a PARTICULAR SHAPE — 'genPlacement'
-- needs a binder plus a payload slot — cannot get it from a coin flip: a declaration with one
-- parameter and one field yields the shape only when both flips land, so three draws in four
-- would be unusable.
--
-- A GROUND draw ('gsGround') leaves out every slot whose value carries a type variable: a
-- declared meta is what the slot is written with (@Cons(head -> $T, tail -> ConsList[$T -> $_T])@
-- carries one in each field), and an unspelled slot is ⊤, which under declaration-boundedness
-- is the slot's declared bound — so the partial is still one the declaration has.
genTypeFromExprWith :: GenSpec -> Bool -> Prgm Expr () -> Expr () -> Gen PartialType
genTypeFromExprWith spec full _ expr = ground <$> go expr
  where
    ground p | gsGround spec = p{ptVars = H.filter (not . typeHasTypeVars) (ptVars p), ptArgs = H.filter (not . typeHasTypeVars) (ptArgs p)}
             | otherwise     = p
    go (CExpr _ c) = return $ constantPartialType c
    go (Value _ n) = return $ partialVal n
    go (TupleApply _ (_, baseExpr) (EAppArg oa)) = do
      base@PartialType{ptArgs=baseArgs} <- go baseExpr
      shouldAddArg <- if full then return True else HG.bool
      if shouldAddArg
        then case oaArr oa of
               (Just (Just arrExpr, _)) -> do
                 arrExpr' <- go arrExpr
                 return base{ptArgs = H.insert (inExprSingleton $ oaObjExpr oa) (singletonType arrExpr') baseArgs}
               (Just (Nothing, oaM)) -> return base{ptArgs = H.insert (inExprSingleton $ oaObjExpr oa) (getMetaType oaM) baseArgs}
               Nothing -> return base
        else return base
    go (TupleApply _ (_, baseExpr) (EAppVar varName m)) = do
      base@PartialType{ptVars=baseVars} <- go baseExpr
      shouldAddVar <- if full then return True else HG.bool
      return $ if shouldAddVar
        then base{ptVars = H.insert varName (getMetaType m) baseVars}
        else base
    go (EWhere _ base cond) = do
      baseType <- go base
      condType <- go cond
      return $ partialAddPreds baseType (PredsOne $ PredExpr condType)
    go e = error $ printf "Unimplemented genTypeFromExpr for %s" (show e)

-- | Generates a 'Type' from the program.
--
-- 'Type' has two constructors, so this has two cases — the 'UnionType' product ('genUnionType')
-- and the 'TypeVar' occurrence ('genTypeVar') — and one more for the two EXTREMA. Every slot of
-- the product is drawn independently from shared sources, so any combination of slots can occur:
-- a predicate top whose exclusions carry a 'TVExt' slot, an unrolled recursive union inside a
-- subtraction, a leaf slot holding two differently-sourced leafs.
--
-- ⊤ and ⊥ are drawn explicitly rather than falling out of a degenerate 'genUnionType' draw. A
-- caller that must not receive a bare variable, ⊤ or ⊥ (a variable's inner, an environment
-- binding, a binder's bound) draws 'genUnionTypeWith' directly, whose non-degeneracy invariant
-- excludes exactly those three; keeping the extrema out of the product is what lets it hold that
-- invariant. Nothing is drawn here and then rejected.
--
-- A bare argument-less reference like @/Cons@ (exact mode, no fields) is a legitimate standalone
-- value — the partial-application form a @/Cons@ has before its args are applied (arg-set @{}@,
-- disjoint from the field forms) — and is generated as-is.
genType :: Prgm Expr () -> Gen Type
genType = genTypeWith defaultSpec

-- | 'genType' under a 'GenSpec': a bare variable is offered only when the spec allows one.
genTypeWith :: GenSpec -> Prgm Expr () -> Gen Type
genTypeWith spec prgm = HG.frequency $ concat
    [ [ (1,  genBasic) ]
    , [ (1,  genTypeVarWith spec prgm) | not (gsGround spec), not (null (gsExtPool spec)) ]
    , [ (11, genUnionTypeWith spec prgm) ]
    ]
  where
    genBasic = HG.element [PTopType, BottomType]

-- | The whole 'UnionType' space: the subtractive top (predicates, negative leafs, negative
-- constants) and the positive half (leafs, constants), each slot drawn independently from a
-- shared source.
--
-- NON-DEGENERACY is an invariant, not an accident: a top always populates at least one of its
-- three slots (an empty top IS ⊤) and a top-less draw always populates at least one positive (an
-- empty positive half IS ⊥). Both extrema belong to 'genType'.'genBasic', which is what keeps
-- their rate — and therefore every filter's acceptance — independent of the program and the size.
--
-- EVERY SHAPE DECISION IS SIZE-INDEPENDENT. The runner sweeps size 0..99 across the test count,
-- and @Range.linear 0 n@ has an upper bound of 0 for every size below @99\/n@: @HG.list (linear 0
-- 2)@ yields nothing at all below size 50, and @linear 1 3@ pins to exactly 1 below size 25. A
-- composite whose slot presence came from a size-parameterised range would therefore be a
-- different generator in the first half of the sweep. So presence and count are drawn with
-- 'HG.frequency' over explicit values; only constant MAGNITUDES keep a size-parameterised range.
--
-- What a program cannot supply is absent from the weight list rather than branched around, which
-- is how the empty program takes the same code path as every other: its only surviving rows are
-- the constant ones, so it yields constant unions and @⊤ − {c}@ tops. What the 'GenSpec'
-- forbids is absent the same way: a ground draw offers no variable-carrying source, a
-- constant-free draw no constant row. A spec that leaves NO row (a constant-free draw on a
-- program with neither leafs nor predicates) is a caller's error, not a discard: every caller
-- of a restricted spec holds a program with declarations ('placementNeed').
genUnionType :: Prgm Expr () -> Gen Type
genUnionType = genUnionTypeWith defaultSpec

-- | 'genUnionType' under a 'GenSpec'.
genUnionTypeWith :: GenSpec -> Prgm Expr () -> Gen Type
genUnionTypeWith spec prgm@(Prgm objMap _ _) = HG.frequency $ concat
    [ [ (2, withTop) | not (null topRows) ]
    , [ (5, plain)   | not (null positiveRows) ]
    , [ (1, error "genUnionTypeWith: the spec leaves no source to draw from") | null topRows && null positiveRows ]
    ]
  where
    ground = gsGround spec
    consts = gsConsts spec
    typeEnv@TypeEnv{teClassGraph=ClassGraph cg} = mkTypeEnv prgm
    canClassAtom = not (null (graphToNodes cg))
    canExprAtom = not (null exprAtomFns)
    canPred = canClassAtom || canExprAtom
    canLeaf = not (null leafSources)

    -- The program's functions, with their declared argument keys: what a value predicate names.
    exprAtomFns = [ (oaObjPath oa, [inExprSingleton (oaObjExpr a) | a <- exprAppliedArgs oe])
                  | oa@ObjArr{oaBasis=FunctionObj, oaObj=Just oe} <- flatObjectMap objMap ]

    -- | A value predicate ('PredExpr') in the two shapes the compiler produces, naming one of the
    -- program's functions: a type property's refinement @Base_prop@, which desugars to
    -- @/operator==(/r=True, /l=~prop(/this=~/$this))@, and a body condition's form @a | f(k= a)@,
    -- which is @f(/k=~/$this)@ ('TypeCheck.Encode.condPredForm'). The value the predicate is over
    -- is named 'macroThisKey', as everywhere a value predicate is decided. A generated program has
    -- no arms for the function and no @==@, so deciding the atom through the graph answers
    -- 'Nothing' and it is OPAQUE to the algebra — what every undecided refinement is to it. What
    -- the atom exercises is that an opaque predicate is carried, merged, decided-by-syntax and
    -- negated soundly wherever a predicate may sit.
    genPredExprAtom :: Gen PartialType
    genPredExprAtom = do
      (fname, argKeys) <- HG.element exprAtomFns
      let this = relTypeVal (mkName macroThisKey)
      let prop = (partialVal fname){ptArgs = H.singleton (partialKey (mkName thisKey)) this}
      HG.choice $
        [ return (partialVal (mkName operatorEq)){ptArgs = H.fromList [(partialKey "/r", typeVal (mkName truePrim)), (partialKey "/l", relPartial prop)]} ]
        ++ [ (\k -> (partialVal fname){ptArgs = H.singleton k this}) <$> HG.element argKeys | not (null argKeys) ]

    plain = uncurry (UnionType Nothing) <$> genPositives

    -- The canonical producer of a 'Just'-top is 'complementTypeEnv', which emits EMPTY positives,
    -- so a mixed type is the rarer shape.
    withTop = do
      top      <- genTopTriple
      (ls, cs) <- HG.frequency $ (3, return (H.empty, [])) : [(1, genPositives) | not (null positiveRows)]
      return $ UnionType (Just top) ls cs

    genTopTriple :: Gen (TypePredicates, PartialLeafs, [Constant])
    genTopTriple = HG.frequency topRows
    topRows = concat
      [ [ (12, (,,) <$> genPreds <*> pure H.empty <*> pure [])  | canPred            ]
      , [ (1, (,,) <$> genPreds <*> genLeafs     <*> pure [])   | canPred && canLeaf ]
      , [ (1, (,,) <$> genPreds <*> pure H.empty <*> genConsts) | canPred && consts  ]
      , [ (1, (,,) PredsNone     <$> genLeafs    <*> pure [])   | canLeaf            ]
      , [ (1, (,,) PredsNone H.empty <$> genConsts)             | consts             ]
      ]

    genPositives :: Gen (PartialLeafs, [Constant])
    genPositives = HG.frequency positiveRows
    positiveRows = concat
      [ [ (13, (,) <$> genLeafs <*> pure [])   | canLeaf           ]
      , [ (3,  (,) H.empty <$> genConsts)      | consts            ]
      , [ (1,  (,) <$> genLeafs <*> genConsts) | canLeaf && consts ]
      ]

    -- 1-3 constants, size-independently (see 'genUnionType').
    genConsts :: Gen [Constant]
    genConsts = nubConsts <$> (count >>= \n -> replicateM n genConstant)
      where count = HG.frequency [(6, return 1), (3, return 2), (1, return 3)]

    -- | Every positive-leaf source in one place. A source yields a GROUP, not a single partial:
    -- the unrolled expansion is only meaningful whole (its 'hasExplicitRecursion' gate needs the
    -- recursive member and its base member in the same union) and a stored meta may itself be a
    -- multi-leaf union. Every source returns a NON-EMPTY group, which is what makes
    -- 'genPositives' non-degenerate.
    leafSources :: [(Int, Gen [PartialType])]
    leafSources = concat
      -- 'genPartialType' is the one partial generator: a bare data name, a declaration partial
      -- with its slots drawn, or either carrying a 'TVExt' slot.
      [ [ (8, pure <$> genLeaf)   | prgmHasPartial prgm       ]
      , [ (4, genStoredMetaLeafs) | not (nullObjectMap objMap) ]
      -- Weighted low: an unrolled expansion is the deepest and slowest type drawn here, and a
      -- small share is enough to reach every leaf position, negative and mixed included. The
      -- expansion carries the class's parameter (@$_T@), so a ground draw never offers it.
      , [ (1, recLeafs)                     | not (null recExpansions), not ground ]
      -- A leaf refined by a value predicate (@Z | $this.isZ == True@, what a refined argument
      -- slot or a body condition's value is typed as). Weighted low, like the expansion: a
      -- small share reaches every leaf position.
      , [ (1, pure <$> genRefinedLeaf)      | prgmHasPartial prgm && canExprAtom ]
      ]

    genRefinedLeaf :: Gen PartialType
    genRefinedLeaf = do
      l <- genLeaf
      e <- genPredExprAtom
      return $ partialAddPreds l (PredsOne (PredExpr e))

    -- | One synthesized leaf, carrying an argument mode. Every leaf position fills from here, so
    -- a mode reaches a top's EXCLUSIONS and a mixed type's halves as well as a plain positive
    -- union.
    --
    -- Re-mode what we synthesize; keep what we borrow. 'genStoredMetaLeafs' holds the mode the
    -- DECLARATION has and 'recLeafs' the one 'expandClassPartial' produced; overwriting those
    -- makes a leaf the program does not have (declaration-boundedness, as in 'genWithTVExtOn').
    genLeaf :: Gen PartialType
    genLeaf = genPartialTypeWith spec prgm >>= remodePartial typeEnv

    -- A union of one to three source groups. Drawing more than one is the only way a leaf slot
    -- holds two DIFFERENTLY-SOURCED leafs — for example a bare data name beside a fully-slotted
    -- partial of the same name, which exercises the disjointness of the argument-less form from
    -- the field forms ('genType').
    genLeafs :: Gen PartialLeafs
    genLeafs = do
      n      <- HG.frequency [(7, return 1), (2, return 2), (1, return 3)]
      groups <- replicateM n (HG.frequency leafSources)
      return $ joinUnionType (concat groups)

    -- | The stored outer meta of a declaration — the one leaf source whose slot values come from
    -- the typechecker rather than from declared metas or a coin flip, so what it holds is by
    -- construction a type the compiler produces. It is NOT 'genTypeFromExprWith' @True@: on a
    -- 'genPrgm' program the two coincide exactly ('genInputExpr' stores @singletonType p'@ with
    -- every slot at ⊤), but on a typechecked fixture the meta is what inference concluded and may
    -- be a multi-leaf union or carry inferred rather than declared slot values.
    --
    -- A meta that is not a positive-leaf union is not forced into a leaf slot: a 'Just'-top
    -- belongs in the predicate slot, a ⊤ meta means the node was never checked, and a bare
    -- variable is 'genTypeVar's. Those fall back to a partial rather than to an empty group,
    -- which would break the non-degeneracy invariant. So does a meta carrying a type variable
    -- under a ground spec.
    genStoredMetaLeafs :: Gen [PartialType]
    genStoredMetaLeafs = do
      oa <- HG.element (flatObjectMap objMap)
      case getExprType <$> oaObj oa of
        Just t@(UnionType Nothing ls []) | not (H.null ls), not (ground && typeHasTypeVars t) -> return (splitUnionType ls)
        _                                                                                      -> pure <$> genLeaf

    recLeafs :: Gen [PartialType]
    recLeafs = do
      t <- genUnrolledRecursive
      case t of
        UnionType Nothing ls [] | not (H.null ls) -> return (splitUnionType ls)
        _                                         -> pure <$> genLeaf

    -- Over-unrolled recursive class expansions. A sealed recursive class (e.g. @ConsList@) expands
    -- to a depth-1 member union whose recursive field is the class-predicate form
    -- (@Cons(tail=∀ConsList) + Nil@). During constraint solving that field accretes one explicit
    -- member-union level per epoch, producing deeper unrollings
    -- (@Cons(tail = Nil + Cons(tail=∀ConsList)) + Nil@) that are semantically equal but syntactically
    -- distinct and never reach a fixpoint. 'Semantics.Types.compactType'\/'foldRecursiveType' fold
    -- these back to the finite predicate form; generating them directly exercises that fold (its
    -- 'hasExplicitRecursion' gate needs a leaf field to explicitly reference the class) and confirms
    -- every law holds across the unrolled and folded representations.
    recExpansions =
      [ (ptName cpt, exp1)
      | (CGClass sealed cpt cts _, _, _) <- graphToNodes cg
      , sealed, not (null cts)
      , let exp1 = expandClassPartial typeEnv H.empty cpt
      , recursesExplicitly (ptName cpt) exp1 ]
      where
        recursesExplicitly cName exp1 = case substRecRef cName exp1 exp1 of
          UnionType Nothing ls [] -> hasExplicitRecursion typeEnv (splitUnionType ls)
          _                       -> False

    genUnrolledRecursive :: Gen Type
    genUnrolledRecursive = do
      (cName, exp1) <- HG.element recExpansions
      depth <- HG.element [1, 2 :: Int] -- unroll 1-2 extra levels → over-unrolled depth 2-3
      return $ iterate (substRecRef cName exp1) exp1 !! depth

    -- Compound predicate formulae: conjunctions (and negated conjunctions) of PredClass/PredRel
    -- atoms drawn from the class graph. Exercises P∨¬P=⊤ / P∧¬P=∅ and the De Morgan handling of
    -- the union/intersection/complement paths for non-atomic predicates. A one-atom non-negated
    -- draw is the plain @∀C@ \/ @isa C@ shape.
    --
    -- Atom kinds mirror the compiler's name resolution ('resolveRelPartial'): a CLASS name
    -- resolves to a 'PredClass' (or stays a 'PredRel' while relative), while a data-TYPE name in
    -- type position resolves to a positive leaf - never a 'PredClass'. So 'PredClass' atoms are
    -- generated only for class nodes; any node may appear as a 'PredRel'. A VALUE predicate
    -- ('genPredExprAtom') is the third kind, the refinement a top carries (@⊤ | $this.isZ == True@).
    genPreds :: Gen TypePredicates
    genPreds = do
      n     <- HG.frequency [(4, return 1), (1, return 2)]
      atoms <- replicateM n genAtom
      let conj = foldr1 predsAnd (map PredsOne atoms)
      -- A quarter negated: positive class membership is the commoner shape in real programs.
      HG.frequency [(3, return conj), (1, return (predsNot conj))]
      where
        genAtom = HG.frequency $ concat
          [ [ (4, genClassAtom)                | canClassAtom ]
          , [ (1, PredExpr <$> genPredExprAtom) | canExprAtom  ]
          ]
        genClassAtom = do
          classNode <- HG.element $ graphToNodes cg
          case snd3 classNode of
            cls@PClassName{} -> do
              let nm = fromPartialName cls
              HG.element [PredClass (partialVal nm), PredRel (partialVal nm)]
            t -> return $ PredRel (partialVal (fromPartialName t))

-- | Generates a type structurally RELATED to a given one. When the input is a positive-partial
-- union, one leaf is mutated along a single dimension: its argument mode flipped among the three
-- set-exact modes, or one declared field widened to ⊤ \/ narrowed to another type. Otherwise it
-- falls back to an independent 'genType'.
--
-- Two independently generated types almost always carry different names and so are trivially
-- disjoint; the difference and intersection then short-circuit before touching the per-field
-- machinery. Drawing the second operand as a same-name single-field mutation instead forces the
-- pair through 'subtractArg'\/'subtractVar', 'looseArgModeRejects' and 'argModeCovers' — the
-- precise arg-set-interval reasoning that is otherwise never exercised.
--
-- The mutated leaf is returned AS-IS, exactly as 'genType' returns its own: a bare argument-less
-- reference is a legitimate standalone value (the partial-application form before its args are
-- applied), and restoring its declared fields onto it would map that form to a DIFFERENT type.
-- It would also overwrite the 'ptArgMode' a mutation had just chosen.
genRelatedType :: Prgm Expr () -> Type -> Gen Type
genRelatedType prgm a = case a of
  UnionType Nothing leafs [] | not (H.null leafs) -> do
    p  <- HG.element (splitUnionType leafs)
    p' <- mutate p
    return $ singletonType p'
  _ -> genType prgm
  where
    typeEnv = mkTypeEnv prgm
    mutate p = HG.choice $
      [ remodePartial typeEnv p ] ++ [ mutateArg p | not (H.null (ptArgs p)) ]
    mutateArg p = do
      k       <- HG.element (H.keys (ptArgs p))
      newVal  <- HG.choice [return PTopType, singletonType <$> genPartialType prgm]
      return p{ptArgs = H.insert k newVal (ptArgs p)}

-- | Wraps a base partial generator and randomly adds a 'TVExt'-tagged type variable to one of
-- its 'ptVars'. This exercises the external variable resolution path in
-- substitution\/intersection. The external variable name is drawn from 'varNamePool' (shared with
-- 'genTypeVar' and 'genVaenv') so a bound fraction of these slots actually resolve when a
-- generated 'TypeVarArgEnv' is threaded through the operations. Shared by 'genPartialType' and
-- 'genPlacement'.
genWithTVExtOn :: Prgm Expr () -> Gen PartialType -> Gen PartialType
genWithTVExtOn = genWithTVExtOnWith defaultSpec

-- | 'genWithTVExtOn' under a 'GenSpec': the name comes from the spec's pool, and a ground spec
-- or an empty pool injects nothing.
genWithTVExtOnWith :: GenSpec -> Prgm Expr () -> Gen PartialType -> Gen PartialType
genWithTVExtOnWith spec prgm baseGen = do
  base <- baseGen
  -- Fill a DECLARED type parameter; never invent one. A partial's 'ptVars' keys are the
  -- parameters its declaration has — @Cons[$T -> $_T]@ fills @Cons@'s own @/T@ — and a key no
  -- declaration mentions (@Dog[xyz=$_a]@) is not a slot any value carries
  -- (declaration-boundedness).
  if H.null (ptVars base) || gsGround spec || null (gsExtPool spec)
    then return base
    else do
      extName <- HG.element (gsExtPool spec)
      -- ~1/3 of injected slots carry occurrence-local knowledge, so the drop/substitute
      -- family's inner handling is exercised where it actually traverses (nested slots).
      inner <- HG.frequency [(2, return PTopType), (1, HG.small (genUnionTypeWith spec prgm))]
      let extVar = TypeVar (TVVar (partialKey extName)) TVExt inner
      varName <- HG.element (H.keys (ptVars base))
      return base{ptVars = H.insert varName extVar (ptVars base)}

-- | A partial that is GUARANTEED to offer a (binder, payload-slot) placement: drawn from the
-- declarations 'declHasPlacement' accepts, with every declared slot forced in. Choosing the
-- declaration first, rather than filtering arbitrary partials, keeps the draw from depending on
-- how many declarations are nullary and on every slot's coin flip landing.
genPlacement :: Prgm Expr () -> Gen PartialType
genPlacement = genPlacementWith declHasPlacement

-- | 'genPlacement' from the declarations a predicate accepts. The caller's program need
-- ('placementNeed', 'fieldPlacementNeed') guarantees one; none is a broken invariant and fails
-- loudly rather than shrinking the corpus with a discard.
genPlacementWith :: (Expr () -> Bool) -> Prgm Expr () -> Gen PartialType
genPlacementWith accept prgm@(Prgm objMap _ _) =
  case [oe | oa <- flatObjectMap objMap, Just oe <- [oaObj oa], accept oe] of
    []    -> error "genPlacementWith: the program has no declaration of the needed shape (its PrgmNeed was not applied)"
    decls -> do
      oe   <- HG.element decls
      base <- genTypeFromExprWith defaultSpec True prgm oe
      HG.choice [return base, genWithTVExtOn prgm (return base)]

genPartialType :: Prgm Expr () -> Gen PartialType
genPartialType = genPartialTypeWith defaultSpec

-- | 'genPartialType' under a 'GenSpec'. The 'TVExt'-carrying variants are offered only when the
-- spec can inject one. A program with neither a data node nor an object has nothing a positive
-- leaf can be built from; the callers' program need ('objectNeed') and the leaf-source guard in
-- 'genUnionType' keep such a program out, so reaching it is a broken invariant.
genPartialTypeWith :: GenSpec -> Prgm Expr () -> Gen PartialType
genPartialTypeWith spec prgm@(Prgm objMap _ _) = HG.choice $ concat
    [ [ genCG                | not (null dataNodes) ]
    , [ genObj               | not (nullObjectMap objMap) ]
    , [ genWithTVExt genCG   | not (null dataNodes), canExt ]
    , [ genWithTVExt genObj  | not (nullObjectMap objMap), canExt ]
    , [ error "genPartialTypeWith: the program has no data node and no object (its PrgmNeed was not applied)" | null dataNodes && nullObjectMap objMap ]
    ]
  where
    canExt = not (gsGround spec) && not (null (gsExtPool spec))
    TypeEnv{teClassGraph=ClassGraph cg} = mkTypeEnv prgm
    -- DATA-TYPE nodes only. A 'PartialType' IS a positive leaf, and a class name in type position
    -- resolves to a 'PredClass' top instead — a 'PartialType' cannot represent a class at all
    -- ('classNodeNames'\/'dataNodeNames').
    dataNodes = map (leafSpelling prgm) (dataNodeNames (ClassGraph cg))
    genCG :: Gen PartialType
    genCG = partialVal <$> HG.element dataNodes

    genObj :: Gen PartialType
    genObj = do
      oa <- HG.element $ flatObjectMap objMap
      let objExpr = fromJust $ oaObj oa
      genTypeFromExprWith spec False prgm objExpr

    genWithTVExt :: Gen PartialType -> Gen PartialType
    genWithTVExt = genWithTVExtOnWith spec prgm

genInputExpr :: Gen (Expr ())
genInputExpr = genInputExprWith (0, 0)

-- | 'genInputExpr' with at least the given numbers of type parameters and fields.
--
-- Every count is drawn with 'HG.frequency' over explicit values, size-independently (the rule
-- 'genUnionType' states): a size-parameterised range pins the count to its lower bound for the
-- first part of the runner's size sweep, so a declaration would have no parameter at all below
-- size 10 and the laws conditioned on one would only ever see the fixture files there. The
-- names are made distinct by construction (each carries its own index letter) rather than drawn
-- as a set, which would retry and discard on collisions.
genInputExprWith :: (Int, Int) -> Gen (Expr ())
genInputExprWith (minVars, minArgs) = do
  n <- makeAbsoluteName . mkName <$> genName
  nVars <- HG.element [minVars .. max minVars 3]
  nArgs <- HG.element [minArgs .. max minArgs 3]
  names <- zipWith (:) ['a' ..] <$> replicateM (nVars + nArgs) genName
  let (varNames, argNames) = splitAt nVars names
  let base = partialVal n
  let val = (base, Value (emptyMetaT $ singletonType base) n)
  withVars <- foldM genVar val (map (partialKey . mkName) varNames)
  snd <$> foldM genArg withVars (map mkName argNames)
  where
    genName = HG.string (linear 5 10) HG.lower
    -- Every application node PRESENTS the type built so far: the object's name plus the
    -- parameters and fields applied up to that point, each at its declared ⊤ bound. A
    -- typechecked program carries these metas; leaving them ⊤ makes the object's declared type
    -- ⊤ everywhere except the innermost 'Value', and 'Semantics.typeGraphData' — which reads
    -- the OUTER meta and keeps only leaf-union presentations — then reports the name as having
    -- NO declaration. Every declaration-dependent rule (a field's declared bound, a name's
    -- declared dimensions and arg-sets, the stamp-support check) silently reads
    -- no-information, so the laws exercise a weaker environment than the compiler's.
    --
    -- The arg's own object expression keeps its ⊤ meta on purpose: 'inExprSingleton' derives
    -- the arg KEY from a singleton type when one is present and from the expression's path
    -- otherwise, and the two spellings differ.
    genVar (p, b) varName = do
      let p' = p{ptVars = H.insert varName PTopType (ptVars p)}
      return (p', TupleApply (emptyMetaT $ singletonType p') (emptyMetaN, b) (EAppVar varName emptyMetaN))
    genArg (p, b) argName = do
      let oa = ObjArr (Just (Value emptyMetaN argName)) ArgObj Nothing [] (Just (Nothing, emptyMetaN))
          p' = p{ptArgs = H.insert (inExprSingleton $ oaObjExpr oa) PTopType (ptArgs p)}
      return (p', TupleApply (emptyMetaT $ singletonType p') (emptyMetaN, b) (EAppArg oa))

-- | Generates an output expression equivalent to an input expression
genOutputExpr :: Expr () -> Expr () -> Gen (Expr ())
genOutputExpr (Value _ n) _ = return $ Value emptyMetaN n
genOutputExpr (TupleApply _ (_, b) _) input = genOutputExpr b input
genOutputExpr e _ = error $ printf "Not yet implemented getOutputExpr for %s" (show e)


genPrgm :: Gen (Prgm Expr ())
genPrgm = genPrgmWith noNeed

-- | 'genPrgm' holding its program to a 'PrgmNeed' by construction: one declaration is drawn to
-- a required shape and one class to the required one, so no generated program has to be drawn
-- and rejected. The object need is met by every generated program (it always has a declaration).
genPrgmWith :: PrgmNeed -> Gen (Prgm Expr ())
genPrgmWith PrgmNeed{pnClass, pnDecls} = do
  nInputs <- HG.element [1 .. 5 :: Int]
  firstShape <- if null pnDecls then return (0, 0) else HG.element pnDecls
  let shapeAt i = if i == 1 then firstShape else (0, 0)
  inputExprs <- forM [1 .. nInputs] $ \i -> HG.either_ (genInputExprWith (shapeAt i)) (genInputExprWith (shapeAt i))
  let (dataTypes, funs) = partitionEithers inputExprs
  let dataObjs = map (\obj -> ObjArr (Just obj) TypeObj Nothing [] Nothing) dataTypes

  let allInputs = dataTypes ++ funs
  funObjs <- forM funs $ \obj -> do
                                let otherInputs = filter (/= obj) allInputs
                                if null otherInputs
                                  then return $ ObjArr (Just obj) FunctionObj Nothing [] (Just (Nothing, emptyMetaN))
                                  else do
                                    arrGoal <- HG.element otherInputs
                                    arr <- genOutputExpr arrGoal obj
                                    return $ ObjArr (Just obj) FunctionObj Nothing [] (Just (Just arr, emptyMetaN))

  (classMemberObjs, classNodes) <- genClassesWith pnClass

  let objMap = objectMapFromList (dataObjs ++ funObjs ++ classMemberObjs)

  return $ Prgm objMap (classGraphFromObjs objMap <> ClassGraph (graphFromEdges classNodes)) []

-- | Well-formed class-graph nodes, plus the nullary data objects that are their members.
--
-- 'classGraphFromObjs' emits only @CGType@ nodes, so these are the generated program's only
-- classes. They are what lets 'genUnionType'.'genPreds' build a 'PredClass' atom and draw a @∀C@
-- shape, and what the properties conditioned on a class being present draw from.
--
-- Well-formedness is by construction, mirroring what the desugarer emits:
--
--   * Members are FRESH NULLARY data types, so a member's stored type is exactly
--     @singletonType (partialVal name)@ — the @class(C, [A, B])@ shape. A member with declared
--     fields would have to carry them here (@Some[$T -> $_T](value -> $T)@), and inventing that
--     spelling is how malformed partials would get into the corpus.
--   * A SEALED class always has at least one member, because a sealed class can only be written
--     @class(C, [..])@; the memberless form @class(C)@ is unsealed ('desClassDecl' builds it with
--     @sealed=False@).
--   * Class names are drawn from a pool disjoint from the data names ('genInputExpr' draws
--     lowercase; these are capitalized), because the two are NOT interchangeable in a type: a
--     class name in type position is a 'PredClass' top and a data name is a positive leaf — the
--     distinction 'classNodeNames'\/'dataNodeNames' exists to keep.
--
-- Zero classes stays in the distribution, so class-free programs are still generated — unless
-- a 'ClassNeed' asks for one, which the first class is then drawn to. The counts are
-- size-independent ('genInputExprWith' says why).
genClasses :: Gen ([ObjArr Expr ()], [(CGNode, PartialName, [PartialName])])
genClasses = genClassesWith Nothing

genClassesWith :: Maybe ClassNeed -> Gen ([ObjArr Expr ()], [(CGNode, PartialName, [PartialName])])
genClassesWith need = do
  nClasses <- HG.element [maybe 0 (const 1) need .. 2]
  parts <- forM [1 .. nClasses] $ \ci -> do
    let forced = if ci == 1 then need else Nothing
    sealed <- maybe HG.bool return (forced >>= cnSealed)
    -- Sealed classes are written with their member list, so they are never memberless.
    let minMembers = maximum [if sealed then 1 else 0, maybe 0 cnMinMembers forced]
    nMembers <- HG.element [minMembers .. max minMembers 3]
    let className   = makeAbsoluteName $ mkName $ printf "Class%d" (ci :: Int)
    let memberNames = [makeAbsoluteName $ mkName $ printf "Mem%d_%d" ci mi | mi <- [1 .. nMembers]]
    let memberObjs  = [ObjArr (Just (Value (emptyMetaT $ typeVal n) n)) TypeObj Nothing [] Nothing | n <- memberNames]
    let node = ( CGClass sealed (partialVal className) (map (singletonType . partialVal) memberNames) Nothing
               , PClassName className
               , map PTypeName memberNames )
    return (memberObjs, node)
  return (concatMap fst parts, map snd parts)

genPrgms :: Gen [GraphNodes (Prgm Expr ()) FileImport]
genPrgms = do
  prgm <- genPrgm
  prgmName <- HG.string (linear 5 10) HG.lower
  return [(prgm, desFileImport $ mkRawFileImport $ RawCExpr emptyMetaN $ CStr prgmName, [])]

-- | Generate a random 'Val' for a concrete 'PartialType'.
-- Returns 'HG.discard' for unknown types.
--
-- A partial with fields (e.g. @Cons(head, tail)@) is generated recursively: each field's type is
-- expanded to its concrete leaves and a value generated for it via 'genValForType'. A field whose
-- type is a bare type variable (e.g. @Cons@'s polymorphic @head -> $T@) is defaulted to 'Integer',
-- since the structural derivations that consume these values (such as @length@) are
-- element-agnostic. Recursion is bounded by 'genValForType' (see there).
--
-- A spread (@Context..[$V -> Integer]@) lists none of its declared fields, so it is read through its
-- declared floor ('spreadFloor') as every partial-level operation reads it. Read as written, it was
-- a nullary type and generated the field-less @/Context@, which is not a value of the type. A field
-- typed by one of the partial's own parameters takes the parameter's type.
genVal :: (TypeGraph tg) => TypeEnv tg -> PartialType -> Gen Val
genVal typeEnv pt0
  | pt == intLeaf   = IntVal <$> HG.integral (linear (-1000) 1000)
  | pt == floatLeaf = FloatVal <$> HG.double (linearFrac (-1000) 1000)
  -- A string by its name, however its fields are spelled: a member read out of the @String@ class
  -- (@String\/Text..@) leaves its characters' type to the declaration, and generated field by field
  -- it would be a list of anything rather than of characters.
  | ptName pt == ptName strLeaf  = textVal <$> HG.string (linear 0 20) HG.unicode
  | ptName pt == ptName cstrLeaf = StrVal <$> HG.string (linear 0 20) HG.unicode
  | pt == charLeaf  = CharVal <$> HG.unicode
  | nameStr (ptName pt) == truePrim  = pure $ TupleVal truePrim H.empty
  | nameStr (ptName pt) == falsePrim = pure $ TupleVal falsePrim H.empty
  -- A nullary data type (no fields) has a single inhabitant: itself. This covers data-valued
  -- args such as a chosen typeclass instance (e.g. an 'Eqv' instance for a property test).
  | H.null (ptArgs pt)     = pure $ TupleVal (nameStr (ptName pt)) H.empty
  | otherwise = do
      argVals <- mapM genArg (H.toList (ptArgs pt))
      pure $ TupleVal (nameStr (ptName pt)) (H.fromList argVals)
  where
    TypeEnv{teNames} = typeEnv
    pt = spreadFloor typeEnv pt0
    genArg (k, at) = (nameStr (pkName k),) <$> genValForType typeEnv (fieldType at)
    -- A field carrying a class/relation (e.g. @tail -> ConsList@) expands to its member data leaves;
    -- a bare type variable has no members to expand to, so default it to 'Integer'.
    fieldType (TypeVar (TVVar v) _ _) | Just vt@(UnionType Nothing _ _) <- H.lookup v (ptVars pt) = fieldType vt
    fieldType TypeVar{} = intType
    fieldType at        = UnionType Nothing (expandTypeWithNames typeEnv H.empty at teNames) []

-- | Generate a random 'Val' for a 'Type'.
-- For union types, picks a random variant and delegates to 'genVal'.
--
-- Recursion through self-referential types (e.g. a @ConsList@ whose @tail@ is again a @ConsList@) is
-- bounded with 'HG.recursive': the nullary variants (e.g. @Nil@) are the non-recursive base cases
-- and the field-bearing variants (e.g. @Cons@) the recursive ones, so Hedgehog shrinks the size on
-- each recursive step and always terminates at a base case. When a union has no nullary variant
-- (e.g. an overlap domain narrowed to @Cons@ alone) every choice recurses, but each recursive field
-- still bottoms out through its own bounded 'genValForType'.
genValForType :: (TypeGraph tg) => TypeEnv tg -> Type -> Gen Val
genValForType typeEnv (UnionType Nothing leafs []) = case splitUnionType leafs of
  [] -> HG.discard
  ps -> case partition (H.null . ptArgs) ps of
    ([], recs)        -> HG.choice (map (genVal typeEnv) recs)
    (terminals, [])   -> HG.choice (map (genVal typeEnv) terminals)
    (terminals, recs) -> HG.recursive HG.choice (map (genVal typeEnv) terminals) (map (genVal typeEnv) recs)
genValForType _ _ = HG.discard
