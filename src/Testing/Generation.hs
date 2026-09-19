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
import           Data.Bifunctor          (bimap)
import           Data.Either             (partitionEithers)
import           Data.Graph              (graphFromEdges)
import qualified Data.HashMap.Strict     as H
import           Data.List               (partition)
import           Data.Maybe
import qualified Data.Set                as S
import           Eval.Common             (Val)
import           Hedgehog
import qualified Hedgehog.Gen            as HG
import           Hedgehog.Range          (linear, linearFrac, singleton)
import           Semantics               (classGraphFromObjs, mkTypeEnv)
import           Semantics.Prgm
import           Semantics.Types
import           Semantics.Val           hiding (Val)
import           Syntax.Ct.Desugarf.Expr (desFileImport)
import           Syntax.Ct.Prgm          (RawExpr (RawCExpr), mkRawFileImport)
import           Text.Printf
import           Utils

-- | A fixed pool of variable names shared by the type-var generators (and, later, a
-- 'TypeVarArgEnv' generator), so a bound fraction of generated variables actually collide.
varNamePool :: [Name]
varNamePool = map mkName ["a", "b", "c"]

-- | Bare type variables as direct inputs ('TVVar'\/'TVArg' × 'TVInt'\/'TVExt'). With no
-- external env a variable's standalone reading is unconstrained, i.e. ⊤ ('vaenvLookup').
genTypeVar :: Prgm Expr () -> Gen Type
genTypeVar prgm = do
  n   <- HG.element varNamePool
  aux <- HG.element [TVVar (partialKey n), TVArg (partialKey n)]
  loc <- HG.element [TVInt, TVExt]
  -- The occurrence-local INNER (read as var ∩ inner): ⊤ (knowledge-free) two thirds of the
  -- time, else a small concrete-ish type — never a bare var (closed-inner producer
  -- discipline), never ⊤ (that is the explicit branch) nor ⊥ (would trivialize every law).
  inner <- HG.frequency [(2, return PTopType), (1, HG.small (HG.filter concreteishInner (genType prgm)))]
  return $ TypeVar aux loc inner

-- | The inner-value filter shared by 'genTypeVar' and 'genWithTVExt' (mirrors 'genVaenv's
-- local rule): no bare variable, no ⊤, no ⊥.
concreteishInner :: Type -> Bool
concreteishInner TypeVar{}  = False
concreteishInner PTopType   = False
concreteishInner BottomType = False
concreteishInner _          = True

-- | Partition the class graph's nodes. 'graphToNodes' returns BOTH the classes ('CGClass') and the
-- data types that are instances of one ('CGType'), and the two are not interchangeable in a type:
--
--   * a CLASS name in type position resolves to a @PredClass@ top, never a positive leaf, and
--   * a DATA-TYPE name resolves to a positive leaf, never a @PredClass@
--
-- — the rule 'resolveRelPartial' implements and 'genType.genCG'\/'genType.genPreds' already follow.
-- A 'PredClass' over a data type errors out of 'expandPartial' (@Unknown class \/aaaaa@), and a
-- class drawn as a bare 'PartialType' is a leaf the compiler never produces. A 'PredRel' is
-- well-formed over either, which is why 'genCGRel' draws from all nodes.
-- | A class-graph node's name as a positive LEAF spells it. The desugarer keys some class-graph
-- nodes by the source's relative name (@Point@, @#noCore@ — 'desMultiTypeDef', 'desClassDecl')
-- while the object map holds the absolute one (@/Point@, @/#noCore@), and the compiler resolves
-- every type-position name before the algebra sees it. A relatively-spelled leaf is therefore a
-- form the algebra never receives, and it judges it as OUTSIDE the universe (the relation route,
-- 'relNameAdmits', admits only universe names): drawing one produced laws failing on spelling
-- alone (@#noCore ∩ ¬~#noCore@, @~Point ∩ Point@). A node name a single object resolves is drawn
-- under that object's spelling; relation and class atoms keep the node's own spelling, which
-- the compiler does produce.
leafSpelling :: Prgm Expr () -> TypeName -> TypeName
leafSpelling (Prgm objMap _ _) n = case relativeNameFilter n (map oaObjPath (flatObjectMap objMap)) of
  [absolute] -> absolute
  _          -> n

classNodeNames, dataNodeNames :: ClassGraph -> [TypeName]
classNodeNames (ClassGraph cg) = [fromPartialName k | (CGClass{}, k, _) <- graphToNodes cg]
dataNodeNames  (ClassGraph cg) = [fromPartialName k | (n, k, _) <- graphToNodes cg, notClass n]
  where notClass CGClass{} = False
        notClass _         = True

-- | A pair of types that MUST be indistinguishable to every operation: the same partial, binding
-- one of its own type variables to a ground bound, holding at one nested position either a
-- 'TVInt' reference to that variable or the bound itself.
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
-- | Whether a declaration has a usable (binder, payload-slot) placement for
-- 'genOwnVarBoundPair': at least one type parameter, and either a field or a second parameter.
--
-- Drawn through 'HG.filter' at the PROGRAM level this retries inside the generator instead of
-- spending the property's discard budget. Without it, a program whose declarations are all
-- nullary — common, and more so now that generated class members are nullary by construction —
-- rejects EVERY draw, and the property gave up on discards at ~37% acceptance while a real
-- counterexample sat past the cap.
declHasPlacement :: Expr () -> Bool
declHasPlacement = go (0 :: Int) (0 :: Int)
  where
    go nv na (TupleApply _ (_, b) (EAppVar _ _)) = go (nv + 1) na b
    go nv na (TupleApply _ (_, b) (EAppArg _))   = go nv (na + 1) b
    go nv na _                                   = nv >= 1 && (na >= 1 || nv >= 2)

-- | A program carrying at least one such declaration.
prgmHasPlacement :: Prgm Expr () -> Bool
prgmHasPlacement (Prgm om _ _) = any (maybe False declHasPlacement . oaObj) (flatObjectMap om)

-- | A program 'genPartialType' can draw from at all. With neither a data node nor an object
-- declaration it has nothing to build a positive leaf out of and discards every draw, spending
-- the property's budget instead of the generator's retries — the give-up FAIL
-- propSubtypeUnionVarSplit reported once the test count reached 10000.
prgmHasPartial :: Prgm Expr () -> Bool
prgmHasPartial (Prgm om cg _) = not (null (dataNodeNames cg)) || not (nullObjectMap om)

-- The sibling generator that places the nested reference as a 'TVParent' — reaching the enclosing
-- parameter THROUGH a shadowing inner scope rather than avoiding one — is 'genParentVarBoundPair'.
genOwnVarBoundPair :: Prgm Expr () -> Gen (Type, Type)
genOwnVarBoundPair prgm@(Prgm objMap _ _) = do
  when (nullObjectMap objMap && graphEmpty cg) HG.discard
  -- Every key written below is one the base's DECLARATION already has. A partial's 'ptVars' and
  -- 'ptArgs' keys ARE its declaration's parameters and fields (declaration-boundedness, the rule
  -- 'genWithTVExt' follows), so inventing a binder name — this generator used to insert its own
  -- @own1@\/@own2@ into whatever partial it drew — builds a leaf no value ever carries. The
  -- operations then legitimately disagree about it, and the law reports a compiler defect that is
  -- really a malformed input: @/Cons[/T=…, /own1=/Cons](/head=$/own1)@ (@/Cons@ declares only
  -- @/T@) failed @sub ⊆ ref@, while the identical shape keyed on the declared @/T@ passes both
  -- directions and both differences.
  base    <- HG.filter (not . null . binderPlacements) (genPlacement prgm)
  (varName, slot) <- HG.element (binderPlacements base)
  bound   <- genGroundBound prgm
  wrap    <- HG.choice (bareSlot : nestedDim varName : [classPredDim varName | any ((/= [varName]) . snd) classDims])
  outer   <- HG.element [singletonType, classPartial, relPartial]
  let withSlot v = placeInSlot outer base varName bound slot (wrap v)
  return (withSlot (TypeVar (TVVar varName) TVInt PTopType), withSlot bound)
  where
    TypeEnv{teClassGraph=ClassGraph cg} = mkTypeEnv prgm
    -- NO SHADOWING at the nested positions. A 'TVInt' name the enclosing partial declares and the
    -- NESTED one does not reads against the enclosing scope ('partialCoversSat'.'ambientRef'); but
    -- if the nested partial declares the SAME name it captures the reference, and the two members
    -- of the "identical" pair stop denoting the same thing —
    -- @∀/Cons[/T=#noCore](/tail=/Some[/T=$/T])@ reads @$/T@ as @/Some@'s @/T@, not @/Cons@'s.
    -- The old invented @own1@\/@own2@ names could never collide, so drawing DECLARED names (which
    -- fixed a worse problem) reintroduced capture; the nested generators below exclude the binder's
    -- name explicitly instead. Referring to an enclosing parameter from inside a nested partial
    -- through a captured name is what 'TVParent' (@$^T@) is for — 'genParentVarBoundPair'.
    -- The payload IS the slot.
    bareSlot = return id
    -- The payload is a dimension of a nested positive partial — again one the nested partial
    -- DECLARES, not an invented @T@\/@K@\/@V@.
    nestedDim varName = do
      p <- HG.filter (\q -> not (H.null (ptVars q)) && not (H.member varName (ptVars q))) (genPartialType prgm)
      dim <- HG.element (H.keys (ptVars p))
      return $ \v -> singletonType p{ptVars = H.insert dim v (ptVars p)}
    -- The payload is a dimension of a CLASS PREDICATE ('classPartial' builds the @∀C[..]@ form).
    -- CLASS nodes only — a 'PredClass' over a data type is not well-formed ('classNodeNames') —
    -- and the dimension must be one the CLASS declares.
    classDims = [ (fromPartialName k, ks)
                | (CGClass _ cpt _ _, k, _) <- graphToNodes cg
                , let ks = H.keys (ptVars cpt), not (null ks) ]
    classPredDim varName = do
      (className, ks) <- HG.element [c | c@(_, ks') <- classDims, any (/= varName) ks']
      dim <- HG.element (filter (/= varName) ks)
      return $ \v -> classPartial (partialVal className){ptVars = H.singleton dim v}

-- | Where a binder-and-payload pair can go in a partial: both DECLARED keys of the same partial
-- and necessarily distinct. The payload prefers a declared FIELD ('Left'); when the declaration
-- has none it takes a SECOND declared parameter ('Right'). Requiring a field outright rejected
-- most draws — plenty of declarations are nullary — and the property then gave up on discards
-- instead of testing anything.
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
genGroundBound prgm = HG.small (HG.filter (\t -> concreteishInner t && not (typeHasTypeVars t)) (genType prgm))

-- | A ground bound that is a positive LEAF union — no predicate or relation top, no constants.
-- 'genParentVarBoundPair' binds its parent parameter with one of these, and the law binds its
-- decoy with one: a relation-shaped bound (@~Nil@) under an env whose like-named entry is a
-- predicate top exposed a MEET gap unrelated to the reference (see 'genParentVarBoundPair').
genGroundLeafBound :: Prgm Expr () -> Gen Type
genGroundLeafBound prgm = HG.small (HG.filter groundLeaf (genType prgm))
  where
    groundLeaf t@(UnionType Nothing leafs []) = not (H.null leafs) && not (typeHasTypeVars t)
    groundLeaf _                               = False

-- | The type parameters an object expression declares, in application order.
declaredVars :: Expr () -> [TypeVarName]
declaredVars (TupleApply _ (_, b) (EAppVar v _)) = v : declaredVars b
declaredVars (TupleApply _ (_, b) _)             = declaredVars b
declaredVars (EWhere _ b _)                      = declaredVars b
declaredVars _                                   = []

-- | The sibling of 'genOwnVarBoundPair' for the PARENT scope: a pair that must be indistinguishable
-- to every operation, where the reference reaches the enclosing partial's parameter THROUGH a
-- nested partial that declares the SAME name — the spelling @/Cons@'s own declaration uses for
-- @/tail -> ∀/ConsList[/T=$^/T]@:
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
-- the base's own level, so its parent reference reaches past the base to the base's parent —
-- at a fragment's top, no definite scope — and the reference is read as a flexible slot:
-- @ref ⊆ alt@ held where @sub ⊆ alt@ did not (seed 16335697364997014974 15371164498158892649,
-- path 27:a13Da6Ba7, 2026-09-18). That shape is not the idiom and asserts nothing.
--
-- Returns the parameter's name too, so the law can bind it in the ambient env to a decoy and
-- assert the reference is not captured.
--
-- The identity alone cannot tell a reference resolved to the parent's binding from one not
-- resolved at all: an unbound variable slot is read as FLEXIBLE (bindable per value —
-- 'isFlexibleVar') and matches anything. A third member with an UNRELATED bound in the nested
-- parameter (@N[/v=K']@, against which the reference must answer exactly as @K@ does) was tried
-- and withdrawn, 2026-09-18: on the idiom shape the prover judges @Cons[/T=Cons](tail=Cons[/T=Cons])@
-- and @Cons[/T=Cons](tail=Cons[/T=#noCore])@ EQUAL — both directions, both differences ∅, even
-- with a bare nested partial — while at the top level the same two spellings are disjoint. The
-- nested spelled parameter is read through the enclosing declaration's pointer
-- (@tail -> ConsList[$T -> $_T]@) rather than as spelled, so the third member compares equal to
-- the second for a reason unrelated to the reference, and the reference — which the prover DOES
-- resolve (@ref − alt@ spells the nested parameter as @Cons@ and answers @ref ⊄ alt@) — was the
-- odd one out. Whether a contradicting nested spelling reads as the pointer or as ∅ is an open
-- ruling; the check returns once it is made.
--
-- The bound is a ground positive LEAF ('genGroundLeafBound'), not any ground type. With the
-- relation top @~Nil@ as the bound and the ambient env binding the same name to a predicate
-- top (@⊤ | ~#noCore ∧ ~Cons@), the pair @Some[/T=~Nil](value=Some[/T=$^/T](value=$/T))@
-- came apart (seed 15064677296543469091 6541914999501811919, path 2816:a15B3aBfDc2): the
-- nested slots compare fine (the reference resolves to @~Nil@), but the OUTER parameter is
-- read declaration-bounded through its @value@ field ('isSubPartialOfWithEnv'.'subVarEff'),
-- @~Nil ∩ Some[/T=$^/T](value=$/T)@, and under that env the meet is left as the unreduced
-- @Some[..] | ~Nil@ — an empty set a @Some@ can never satisfy — which the walk then cannot
-- prove a subtype of @~Nil@. Without the env, or with a data decoy, the same meet reduces.
-- A meet gap on relation-shaped bounds (fixed repro in build/scratch/parentrefrepro.hs), so
-- the law keeps to leaf bounds until it is closed.
--
-- A BARE top-level 'TVParent' is deliberately not generated anywhere: a fragment's top has no
-- enclosing partial, and which layer stands in for the missing parent depends on which walker
-- reads the fragment ('substituteKeep' seeds the ambient env, 'substituteTVExt' seeds nothing —
-- the two fragment kinds of 2d1d1b1e), so there is no one denotation for a law to assert.
genParentVarBoundPair :: Prgm Expr () -> Gen (Type, Type, TypeVarName)
genParentVarBoundPair prgm@(Prgm objMap _ _) = do
  when (nullObjectMap objMap) HG.discard
  base    <- HG.filter (not . null . fieldPlacements) (genPlacement prgm)
  (varName, slot) <- HG.element (fieldPlacements base)
  bound   <- genGroundLeafBound prgm
  -- Every declared slot of the nested partial is forced in ('genTypeFromExprWith' True), as for
  -- the base: the pair must differ by the one parameter value under test and nothing else.
  nested  <- HG.element [oe | (v, oe) <- decls, v == varName] >>= genTypeFromExprWith True prgm
  outer   <- HG.element [singletonType, classPartial, relPartial]
  let payload v = singletonType nested{ptVars = H.insert varName v (ptVars nested)}
      withSlot v = placeInSlot outer base varName bound slot (payload v)
  return (withSlot (TypeVar (TVVar varName) TVParent PTopType), withSlot bound, varName)
  where
    -- The declarations that declare the name and that 'genTypeFromExprWith' can read (the
    -- same spine shape 'genPlacement' accepts); the base's own declaration is always one.
    decls = [ (v, oe) | oa <- flatObjectMap objMap, Just oe <- [oaObj oa], declHasPlacement oe, v <- declaredVars oe ]
    fieldPlacements b = [ pl | pl@(_, Left _) <- binderPlacements b ]

-- | A small external variable environment ('TypeVarArgEnv') mapping a subset of 'varNamePool'
-- to concrete-ish types drawn from the program. Threading this through the @*WithEnv@ operations
-- exercises 'vaenvLookup' (a bound variable reads as its value rather than ⊤), 'substituteTVExt'\/
-- 'substituteKeep' (external variables inside partial slots — added by 'genWithTVExt' from the same
-- pool — resolve), and 'applyTypeVarOverrides'.
--
-- Each pool name is bound (with ~2/3 probability, so unbound variables still occur and read as ⊤)
-- under either a 'TVVar' or 'TVArg' key. The bound values are concrete-ish: drawn from 'genType' but
-- never another bare variable (so the environment is acyclic — 'vaenvLookup' does no recursive
-- resolution and a variable bound to another variable could loop the variable-rewriting clauses of
-- the operations) and never ⊤ or ∅ (a variable bound to ⊤ is indistinguishable from an unbound one,
-- and ∅ is not a value a program's type variable resolves to).
genVaenv :: Prgm Expr () -> Gen TypeVarArgEnv
genVaenv prgm = do
  entries <- forM varNamePool $ \n -> do
    bind <- HG.frequency [(2, return True), (1, return False)]
    if not bind
      then return Nothing
      else do
        aux <- HG.element [TVVar (partialKey n), TVArg (partialKey n)]
        v   <- HG.filter concreteish (genType prgm)
        return $ Just (aux, v)
  return $ H.fromList $ catMaybes entries
  where
    concreteish TypeVar{}  = False
    concreteish PTopType   = False
    concreteish BottomType = False
    concreteish _          = True

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
genTypeFromExpr = genTypeFromExprWith False

-- | 'genTypeFromExpr', with a switch that forces EVERY declared parameter and field of the
-- expression into the generated partial instead of deciding each with a coin flip.
--
-- The coin flips are what a general draw wants: a partial that spells only some of its
-- declaration's slots is a legitimate value (an unspelled parameter is ⊤), and generating those
-- is most of the point. But a caller that needs a partial of a PARTICULAR SHAPE — 'genPlacement'
-- below needs a binder plus a payload slot — cannot get it from a coin flip: a declaration with
-- one parameter and one field yields the shape only when both flips land, so three draws in four
-- are unusable. Filtering those out afterwards spends the property's discard budget rather than
-- the generator's own retries, which is how propOwnVarBindingRespected came to report
-- "gave up after 1000 discards" once the test count reached 10000 — a FAIL that says nothing
-- about the algebra.
genTypeFromExprWith :: Bool -> Prgm Expr () -> Expr () -> Gen PartialType
genTypeFromExprWith _ _ (CExpr _ c) = return $ constantPartialType c
genTypeFromExprWith _ _ (Value _ n) = return $ partialVal n
genTypeFromExprWith full prgm (TupleApply _ (_, baseExpr) (EAppArg oa)) = do
  base@PartialType{ptArgs=baseArgs} <- genTypeFromExprWith full prgm baseExpr
  shouldAddArg <- if full then return True else HG.bool
  if shouldAddArg
    then case oaArr oa of
           (Just (Just arrExpr, _)) -> do
             arrExpr' <- genTypeFromExprWith full prgm arrExpr
             return base{ptArgs = H.insert (inExprSingleton $ oaObjExpr oa) (singletonType arrExpr') baseArgs}
           (Just (Nothing, oaM)) -> return base{ptArgs = H.insert (inExprSingleton $ oaObjExpr oa) (getMetaType oaM) baseArgs}
           Nothing -> return base
    else return base
genTypeFromExprWith full prgm (TupleApply _ (_, baseExpr) (EAppVar varName m)) = do
  base@PartialType{ptVars=baseVars} <- genTypeFromExprWith full prgm baseExpr
  shouldAddVar <- if full then return True else HG.bool
  return $ if shouldAddVar
    then base{ptVars = H.insert varName (getMetaType m) baseVars}
    else base
genTypeFromExprWith full prgm (EWhere _ base cond) = do
  baseType <- genTypeFromExprWith full prgm base
  condType <- genTypeFromExprWith full prgm cond
  return $ partialAddPreds baseType (PredsOne $ PredExpr condType)
genTypeFromExprWith _ _ e = error $ printf "Unimplemented genTypeFromExpr for %s" (show e)

-- | Generates a 'Type' from the program. A bare argument-less reference like @/Cons@ (exact
-- mode, no fields) is a legitimate standalone value — the partial-application form a @/Cons@
-- has before its args are applied (arg-set @{}@, disjoint from the field forms) — and is
-- generated as-is.
genType :: Prgm Expr () -> Gen Type
genType prgm@(Prgm objMap _ _) = HG.choice gens >>= withRandomArgModes
  where
    typeEnv@TypeEnv{teClassGraph=ClassGraph cg} = mkTypeEnv prgm

    -- Randomly assign a 'PtArgMode' to the generated partials, exercising the new
    -- lower\/upper-bound modes through the (precise) set operations. Only plain
    -- positive-partial unions are re-moded.
    --
    -- The fully-unbounded mode (lb=False, ub=False) is excluded for partials with args:
    -- its intersection with a lower-bounded partial of the same name is "required x,
    -- optional typed y, extras allowed", which provably cannot be expressed as positive
    -- partials (it needs a negative partial). The other three modes are exact under all
    -- the set operations.
    withRandomArgModes :: Type -> Gen Type
    withRandomArgModes (UnionType Nothing leafs []) | not (H.null leafs) = do
      leafs' <- mapM remode $ splitUnionType leafs
      return $ UnionType Nothing (joinUnionType leafs') []
      where
        remode p = do
          mode <- HG.element [PtArgMode True True, PtArgMode True False, PtArgMode False True]
          -- the lower bound is vacuous with no args, so keep it set then (avoids
          -- producing a representation set-equivalent to a different one)
          let mode' = if H.null (ptArgs p) then mode{ptArgLb=True} else mode
          return p{ptArgMode = mode'}
    withRandomArgModes t = return t

    gens = if graphEmpty cg
      then [genBasic, genConst, genMultiConst, genTypeVar prgm]
      -- 'genPartialType' is its OWN family (its 'genCG'\/'genObj'\/'genWithTVExt' are distinct from
      -- the like-named ones here) and was reachable only from 'genRelatedType'.'mutateArg' and
      -- 'propParamClassCovariant' — so its shapes, including every 'TVExt'-carrying slot, never fed
      -- the set-algebra laws' operands. Drawing from it here is what puts them in the corpus.
      else [genBasic, genCGOld, genCG, genCGRel, genObjM, genObj, genConst, genMultiConst, genPredShape, genMixed, genTypeVar prgm, singletonType <$> genPartialType prgm]
             ++ [genUnrolledRecursive | not (null recExpansions)]

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
          UnionType Nothing ls [] -> hasExplicitRecursion (splitUnionType ls)
          _                       -> False

    genUnrolledRecursive :: Gen Type
    genUnrolledRecursive = do
      (cName, exp1) <- HG.element recExpansions
      depth <- HG.int (linear 1 2) -- unroll 1-2 extra levels → over-unrolled depth 2-3
      return $ iterate (substRecRef cName exp1) exp1 !! depth

    -- Compound predicate formulae: conjunctions (and negated conjunctions) of PredClass/PredRel
    -- atoms drawn from the class graph. Exercises P∨¬P=⊤ / P∧¬P=∅ and the De Morgan handling of
    -- the union/intersection/complement paths for non-atomic predicates.
    --
    -- Atom kinds mirror the compiler's name resolution ('resolveRelPartial'): a CLASS name
    -- resolves to a 'PredClass' (or stays a 'PredRel' while relative), while a data-TYPE name in
    -- type position resolves to a positive leaf - never a 'PredClass'. So 'PredClass' atoms are
    -- generated only for class nodes; any node may appear as a 'PredRel'.
    genPreds :: Gen TypePredicates
    genPreds = do
      atoms <- HG.list (linear 1 2) genAtom
      HG.choice [ return $ foldr1 predsAnd (map PredsOne atoms)
                , return $ predsNot $ foldr1 predsAnd (map PredsOne atoms) ]
      where
        genAtom = do
          classNode <- HG.element $ graphToNodes cg
          case snd3 classNode of
            cls@PClassName{} -> do
              let nm = fromPartialName cls
              HG.element [PredClass (partialVal nm), PredRel (partialVal nm)]
            t -> return $ PredRel (partialVal (fromPartialName t))

    genPredShape :: Gen Type
    genPredShape = TopType H.empty <$> genPreds

    -- Mixed types as direct inputs: a subtractive 'Just'-top component (predicates, negative
    -- leafs, negative constants) unioned with positive leafs and constants. These arise as
    -- intermediate results of the set operations; generating them directly exercises the
    -- mixed-distribute intersection, the mixed-left subtype branch, and the mixed
    -- compactType\/complement clauses as top-level inputs.
    genMixed :: Gen Type
    genMixed = do
      preds     <- HG.choice [genPreds, genPreds, return PredsNone]
      negLeafs  <- HG.choice [return H.empty, leafsOf genObj]
      negConsts <- HG.choice [return [], nubConsts <$> HG.list (linear 1 2) genConstant]
      posLeafs  <- HG.choice [return H.empty, leafsOf genObj]
      posConsts <- HG.choice [return [], nubConsts <$> HG.list (linear 1 2) genConstant]
      if H.null posLeafs && H.null negLeafs && preds == PredsNone && null negConsts && null posConsts
        then genCG
        else return $ UnionType (Just (preds, negLeafs, negConsts)) posLeafs posConsts
      where
        nubConsts = foldr (\c acc -> if c `elem` acc then acc else c : acc) []
        leafsOf g = do
          t <- g
          return $ case t of
            UnionType Nothing ls [] -> ls
            _                       -> H.empty

    -- Unions of several constants, and constants joined with a positive partial. Single-constant
    -- 'genConst' never exercises the multi-constant union paths or the constant-not-covered-by-top
    -- expansion in 'unionTypesWithEnv'\/'intersectTypesWithVarEnv'.
    genMultiConst :: Gen Type
    genMultiConst = do
      cs <- HG.list (linear 1 3) genConstant
      leafs <- if nullObjectMap objMap then return H.empty else HG.choice [return H.empty, someLeafs]
      return $ UnionType Nothing leafs (nubConsts cs)
      where
        nubConsts = foldr (\c acc -> if c `elem` acc then acc else c : acc) []
        someLeafs = do
          t <- genObj
          return $ case t of
            UnionType Nothing ls [] -> ls
            _                       -> H.empty

    genConstant :: Gen Constant
    genConstant = HG.choice [ CInt <$> HG.integral (linear (-100) 100)
                            , CFloat <$> HG.double (linearFrac (-100) 100)
                            , CStr <$> HG.string (linear 0 5) HG.alpha
                            , CChar <$> HG.unicode ]

    -- Find all functions with single argument having thisKey
    typePropFunctions = mapMaybe checkFunction $ flatObjectMap objMap
      where
        checkFunction ObjArr{oaBasis=FunctionObj, oaObj=Just objExpr} =
          case exprAppliedArgs objExpr of
            [arg] -> case oaObj arg of
              Just (Value _ n) | nameStr n == thisKey -> Just objExpr
              _                                       -> Nothing
            _ -> Nothing
        checkFunction _ = Nothing

    genBasic :: Gen Type
    genBasic = HG.element [PTopType, BottomType]

    genConst :: Gen Type
    genConst = do
      c <- HG.choice [ CInt <$> HG.integral (linear (-100) 100)
                     , CFloat <$> HG.double (linearFrac (-100) 100)
                     , CStr <$> HG.string (linear 0 5) HG.alpha
                     , CChar <$> HG.unicode ]
      return $ UnionType Nothing H.empty [c]

    genCGOld :: Gen Type
    genCGOld = do
      classNode <- HG.element $ graphToNodes cg
      return $ typeVal $ leafSpelling prgm $ fromPartialName $ snd3 classNode

    genCG :: Gen Type
    genCG = do
      classNode <- HG.element $ graphToNodes cg
      return $ case snd3 classNode of
        cls@PClassName{} -> TopType H.empty (PredsOne $ PredClass $ partialVal $ fromPartialName cls)
        t                -> typeVal $ leafSpelling prgm $ fromPartialName t

    genCGRel :: Gen Type
    genCGRel = do
      classNode <- HG.element $ graphToNodes cg
      return $ relTypeVal $ fromPartialName $ snd3 classNode

    genObjM :: Gen Type
    genObjM = do
      oa <- HG.element $ flatObjectMap objMap
      let objExpr = fromJust $ oaObj oa
      return $ getExprType objExpr

    genObj :: Gen Type
    genObj = do
      oa <- HG.element $ flatObjectMap objMap
      let objExpr = fromJust $ oaObj oa
      basePartial <- genTypeFromExpr prgm objExpr

      -- Optionally add type properties as refinement predicates
      if null typePropFunctions
        then return $ singletonType basePartial
        else do
          -- TODO: Fix support for shouldAddProps
          -- The current implementation of it doesn't actually make sense or produce valid PredExpr
          let shouldAddProps = False -- HG.bool
          if shouldAddProps
            then do
              -- Select a random subset of type properties
              numProps <- HG.int (linear 0 (min 3 $ length typePropFunctions))
              selectedProps <- take numProps <$> HG.shuffle typePropFunctions
              -- Add each as a predicate
              finalPartial <- foldM addPred basePartial selectedProps
              return $ singletonType finalPartial
            else return $ singletonType basePartial
      where
        addPred partial prop = do
          propType <- genTypeFromExpr prgm prop
          return $ partialAddPreds partial (PredsOne $ PredExpr propType)

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
-- applied), and 'completeDataPartialsTop' is not a canonicalization — it maps that form to a
-- DIFFERENT type. Completing here would also overwrite the very 'ptArgMode' a mutation had just
-- chosen (its 'completePartial' adopts the declaration's mode whenever the partial is left
-- argless and exact), erasing the arg-mode dimension this generator exists to exercise.
genRelatedType :: Prgm Expr () -> Type -> Gen Type
genRelatedType prgm a = case a of
  UnionType Nothing leafs [] | not (H.null leafs) -> do
    p  <- HG.element (splitUnionType leafs)
    p' <- mutate p
    return $ singletonType p'
  _ -> genType prgm
  where
    -- the three modes that are exact under all set operations (see 'genType.withRandomArgModes')
    safeModes = [PtArgExact, PtArgAny, PtArgMode False True]
    mutate p = HG.choice $
      [ do m <- HG.element safeModes
           return p{ptArgMode = if H.null (ptArgs p) then m{ptArgLb=True} else m}
      ] ++ [ mutateArg p | not (H.null (ptArgs p)) ]
    mutateArg p = do
      k       <- HG.element (H.keys (ptArgs p))
      newVal  <- HG.choice [return PTopType, singletonType <$> genPartialType prgm]
      return p{ptArgs = H.insert k newVal (ptArgs p)}

-- | Wraps a base partial generator and randomly adds a 'TVExt'-tagged type variable to one of
-- its 'ptVars'. This exercises the external variable resolution path in
-- substitution\/intersection. The external variable name is drawn from 'varNamePool' (shared with
-- 'genTypeVar' and 'genVaenv') so a bound fraction of these slots actually resolve when a
-- generated 'TypeVarArgEnv' is threaded through the operations.
--
-- Top-level so callers other than 'genPartialType' — 'genPlacement' — keep this coverage.
genWithTVExtOn :: Prgm Expr () -> Gen PartialType -> Gen PartialType
genWithTVExtOn prgm baseGen = do
  base <- baseGen
  extName <- HG.element varNamePool
  -- ~1/3 of injected slots carry occurrence-local knowledge, so the drop/substitute
  -- family's inner handling is exercised where it actually traverses (nested slots).
  inner <- HG.frequency [(2, return PTopType), (1, HG.small (HG.filter concreteishInner (genType prgm)))]
  let extVar = TypeVar (TVVar (partialKey extName)) TVExt inner
  -- Fill a DECLARED type parameter; never invent one. A partial's 'ptVars' keys are the
  -- parameters its declaration has — @Cons[$T -> $_T]@ fills @Cons@'s own @/T@ — and a key no
  -- declaration mentions is not a slot any value carries (declaration-boundedness). Inventing
  -- one produced leaves like @\/Dog[xyz=$_a]@; harmless while these partials only appeared
  -- nested, and 5 law failures the moment 'genType' drew from this generator.
  if H.null (ptVars base)
    then return base
    else do
      varName <- HG.element (H.keys (ptVars base))
      return base{ptVars = H.insert varName extVar (ptVars base)}

-- | A partial that is GUARANTEED to offer a (binder, payload-slot) placement: drawn from the
-- declarations 'declHasPlacement' accepts, with every declared slot forced in.
--
-- This replaces @HG.filter (not . null . placements) (genPartialType prgm)@. That filter drew
-- from ALL declarations — most are nullary — and then coin-flipped each slot, so a qualifying
-- shape needed a qualifying declaration AND every flip to land. Hedgehog\'s filter retries a
-- bounded number of times and then discards, which at 10000 tests exhausted the property\'s
-- 1000-discard budget and reported a give-up FAIL carrying no information about the algebra.
genPlacement :: Prgm Expr () -> Gen PartialType
genPlacement prgm@(Prgm objMap _ _) =
  case [oe | oa <- flatObjectMap objMap, Just oe <- [oaObj oa], declHasPlacement oe] of
    []    -> HG.discard -- caller filtered on 'prgmHasPlacement', so this is unreachable
    decls -> do
      oe   <- HG.element decls
      base <- genTypeFromExprWith True prgm oe
      HG.choice [return base, genWithTVExtOn prgm (return base)]

genPartialType :: Prgm Expr () -> Gen PartialType
genPartialType prgm@(Prgm objMap _ _) = do
  gen <- if null dataNodes
    then if nullObjectMap objMap
      then HG.discard
      else return [genObj, genWithTVExt genObj]
    else return [genCG, genObj, genWithTVExt genCG, genWithTVExt genObj]
  HG.choice gen
  where
    TypeEnv{teClassGraph=ClassGraph cg} = mkTypeEnv prgm
    -- DATA-TYPE nodes only. A 'PartialType' IS a positive leaf, and a class name in type position
    -- resolves to a 'PredClass' top instead — a 'PartialType' cannot represent a class at all
    -- ('classNodeNames'\/'dataNodeNames'). Drawing class nodes here produced leaves the compiler
    -- never emits, which is why they never reached the laws until 'genType' began drawing from
    -- this generator.
    dataNodes = map (leafSpelling prgm) (dataNodeNames (ClassGraph cg))
    genCG :: Gen PartialType
    genCG = partialVal <$> HG.element dataNodes

    genObj :: Gen PartialType
    genObj = do
      oa <- HG.element $ flatObjectMap objMap
      let objExpr = fromJust $ oaObj oa
      genTypeFromExpr prgm objExpr

    -- | Wraps a base partial generator and randomly adds a 'TVExt'-tagged
    -- type variable to one of its 'ptVars'.  This exercises the external
    -- variable resolution path in substitution/intersection. The external
    -- variable name is drawn from 'varNamePool' (shared with 'genTypeVar' and
    -- 'genVaenv') so a bound fraction of these slots actually resolve when a
    -- generated 'TypeVarArgEnv' is threaded through the operations.
    genWithTVExt :: Gen PartialType -> Gen PartialType
    genWithTVExt = genWithTVExtOn prgm

genInputExpr :: Gen (Expr ())
genInputExpr = do
  n <- makeAbsoluteName . mkName <$> genName
  varArgNames <- HG.set (linear 0 5) genName
  varOrArg <- HG.list (singleton $ S.size varArgNames) HG.bool
  let (varNames, argNames) = bimap (map fst) (map fst) $ partition snd $ zip (S.toList varArgNames) varOrArg
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
-- genOutputExpr (TupleApply _ (_, b) arg@ObjArr{oaArr}) input = do
--   b' <- genOutputExpr b input
--   oaArr' <- case oaArr of
--     (Just e, _) -> do
--       e' <- genOutputExpr e input
--       return (Just e', emptyMetaN)
--     (Nothing, _) -> return (Just (HoleExpr emptyMetaN (HoleActive Nothing)), emptyMetaN)
--   return $ TupleApply emptyMetaN (emptyMetaN, b') arg{oaArr=oaArr'}
-- genOutputExpr (VarApply _ b _ _) input = genOutputExpr b input
-- genOutputExpr (VarApply _ b varName _) input = do
--   b' <- genOutputExpr b input
--   return $ VarApply emptyMetaN b' varName emptyMetaN
genOutputExpr e _ = error $ printf "Not yet implemented getOutputExpr for %s" (show e)


genPrgm :: Gen (Prgm Expr ())
genPrgm = do
  inputExprs <- HG.list (linear 1 5) (HG.either_ genInputExpr genInputExpr)
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

  (classMemberObjs, classNodes) <- genClasses

  let objMap = objectMapFromList (dataObjs ++ funObjs ++ classMemberObjs)

  return $ Prgm objMap (classGraphFromObjs objMap <> ClassGraph (graphFromEdges classNodes)) []

-- | Well-formed class-graph nodes, plus the nullary data objects that are their members.
--
-- Without this, a generated program has NO classes at all: 'genPrgm' built its whole class graph
-- from 'classGraphFromObjs', which emits @CGType@ nodes exclusively (see @Semantics.hs@). That
-- silently emptied the class corpus for EVERY law, not just the class-specific ones —
-- 'genType'.'genAtom' can only build a 'PredClass' from a 'PClassName' node, so generated
-- programs produced 'PredRel' atoms exclusively, and 'classNodeNames' being empty meant no
-- @∀C@ shape was ever drawn. All class\/negation coverage came from the three hand-written
-- files in @test\/Semantics\/code\/@, and the properties CONDITIONED on a class being present
-- ('propClassExpansionLeafSubtype', 'propClassGraphMatchesPredClass') spent their entire
-- discard budget on draws that were structurally incapable of satisfying them.
--
-- Well-formedness is by construction, mirroring what the desugarer emits:
--
--   * Members are FRESH NULLARY data types, so a member's stored type is exactly
--     @singletonType (partialVal name)@ — the @class(C, [A, B])@ shape of @Animals.ct@. A member
--     with declared fields would have to carry them here (as @containers.ct@'s
--     @Some[$T -> $_T](value -> $T)@ does), and inventing that spelling is how malformed
--     partials get into the corpus.
--   * A SEALED class always has at least one member, because a sealed class can only be written
--     @class(C, [..])@; the memberless form @class(C)@ is unsealed ('desClassDecl' builds it with
--     @sealed=False@).
--   * Class names are drawn from a pool disjoint from the data names ('genInputExpr' draws
--     lowercase; these are capitalized), because the two are NOT interchangeable in a type: a
--     class name in type position is a 'PredClass' top and a data name is a positive leaf — the
--     distinction 'classNodeNames'\/'dataNodeNames' exists to keep.
--
-- Zero classes stays in the distribution, so the class-free programs that were previously the
-- only kind are still generated.
genClasses :: Gen ([ObjArr Expr ()], [(CGNode, PartialName, [PartialName])])
genClasses = do
  nClasses <- HG.int (linear 0 2)
  parts <- forM [1 .. nClasses] $ \ci -> do
    sealed <- HG.bool
    -- Sealed classes are written with their member list, so they are never memberless.
    nMembers <- HG.int (linear (if sealed then 1 else 0) 3)
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
genVal :: (TypeGraph tg) => TypeEnv tg -> PartialType -> Gen Val
genVal typeEnv pt
  | pt == intLeaf   = IntVal <$> HG.integral (linear (-1000) 1000)
  | pt == floatLeaf = FloatVal <$> HG.double (linearFrac (-1000) 1000)
  | pt == strLeaf   = StrVal <$> HG.string (linear 0 20) HG.unicode
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
    genArg (k, at) = (nameStr (pkName k),) <$> genValForType typeEnv (fieldType at)
    -- A field carrying a class/relation (e.g. @tail -> ConsList@) expands to its member data leaves;
    -- a bare type variable has no members to expand to, so default it to 'Integer'.
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
