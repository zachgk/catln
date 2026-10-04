--------------------------------------------------------------------
-- |
-- Module    :  TypeCheck.Proof
-- Copyright :  (c) Zach Kimberg 2026
-- License   :  MIT
-- Maintainer:  zachary@kimberg.com
-- Stability :  experimental
-- Portability: non-portable
--
-- This module proves refinement declarations marked @#proof@ with the type checker.
--
-- A refinement declaration (@#refine@) claims a narrower result for part of a function's domain.
-- To prove it, every definition whose input overlaps the refinement's is typechecked again, on the
-- overlap alone and against the already typechecked program, and its result must convert to the
-- claimed type. During that solve, calls back into the function being proven are read through the
-- claim itself (the induction hypothesis), which every overlapping definition establishes in turn.
--
-- Which definitions must prove it follows the function's quantification ('omiQuant'). Every
-- overlapping definition of a unique function must, since any of them may be the one result. An
-- existential function's definitions are alternatives, so its claim is proven when the definitions
-- which prove it together cover the input the refinement and its definitions share.
--------------------------------------------------------------------

module TypeCheck.Proof (
  RefinementProof(..),
  RefinementObligation(..),
  refinementProofs,
  proveRefinement,
  proveAnyRefinement,
  ) where

import           CRes
import           CtConstants         (proofAnnot)
import qualified Data.HashMap.Strict as H
import qualified Data.HashSet        as S
import           Data.List           (intercalate)
import           Data.Maybe
import           MapMeta             (clearMetaDat, mapMetaPrgm)
import           Semantics
import           Semantics.Annots    (getRuntimeAnnot)
import           Semantics.Prgm
import           Semantics.Types
import           Text.Printf
import           TypeCheck           (typecheckPrgms)
import           TypeCheck.Common

-- | One obligation of a @#proof@ refinement: the refinement and one definition (or runtime
-- declaration) of the same function whose input overlaps it.
data RefinementProof = RefinementProof
  { rpName    :: TypeName
  , rpRefine  :: TObjArr
  , rpDef     :: TObjArr
    -- | The inputs both accept, which are the inputs the definition is proven on.
  , rpOverlap :: Type
  }

-- | What a @#proof@ refinement asks to be proven, by the quantification of its function.
data RefinementObligation
  -- | A unique function: the refinement holds of this one definition. There is one per overlapping
  -- definition, and each is a test of its own.
  = ProveEach RefinementProof
  -- | An existential function: the refinement holds when the definitions proving it cover the
  -- 'Type', which is the refinement's input as far as the definitions accept it. One per refinement.
  | ProveAny TypeName TObjArr Type [RefinementProof]

-- | Every obligation of the program's @#proof@ refinements. A definition the refinement does not
-- overlap makes no claim about it.
refinementProofs :: TPrgm -> [RefinementObligation]
refinementProofs prgm@(Prgm (ObjectMap om) _ _) = concat
  [ case omiQuant omi of
      QuantUnique -> map ProveEach proofs
      QuantExist  -> [ProveAny name refOa required proofs]
  | (name, omi) <- H.toList om
  , refOa <- omiRefineDecls omi
  , oaHasAnnotRel proofAnnot refOa
  , isJust (oaArr refOa)
  , let defOas = omiDefs omi ++ omiRuntimeDecls omi
  , let proofs = [ RefinementProof name refOa defOa dom
                 | defOa <- defOas
                 , let dom = intersectTypes typeEnv (inputType refOa) (inputType defOa)
                 , not (isBottomType dom)
                 ]
  , let required = intersectTypes typeEnv (inputType refOa) (unionAllTypes typeEnv (map inputType defOas))
  ]
  where
    typeEnv = mkTypeEnv prgm

inputType :: TObjArr -> Type
inputType = getExprType . oaObjExpr

-- | The name the definition is typechecked again under. It is a fresh name so that the definition
-- does not answer its own recursive calls (they must read the claim), and its last segment matches
-- no name a body writes, since a body's names are resolved relatively.
proofSubjectName :: TypeName
proofSubjectName = "/Catln/RefinementProof/proofSubject"

-- | Prove one obligation. The typechecked program holds every page the definition was solved with;
-- the pre-typed definition is that same definition as desugared, which is what a solve starts from.
proveRefinement :: TPrgm -> PObjArr -> RefinementProof -> CRes ()
proveRefinement tprgm preDef RefinementProof{rpName, rpRefine, rpDef, rpOverlap} = case oaArr preDef of
  Just (Just _, _) -> do
    claim <- case oaArr rpRefine of
      Just (_, m) -> return $ getMetaType m
      Nothing     -> proofErr "the refinement declares no result to prove"
    subject <- proofSubject rpName rpOverlap preDef
    let deps = opaqueDeps rpName rpRefine tprgm
    solved <- typeCheckToRes $ typecheckPrgms [(Prgm (singletonObjectMap subject) mempty [], [], [deps])]
    solvedSubject <- case [ oa | Prgm (ObjectMap om) _ _ <- solved
                               , Just omi <- [H.lookup proofSubjectName om]
                               , oa <- omiDefs omi ] of
      [oa] -> return oa
      oas  -> proofErr $ printf "typechecking the definition again produced %d definitions rather than one" (length oas)
    let typeEnv = mkTypeEnv (mapMetaPrgm clearMetaDat (deps <> mconcat solved))
    -- The result as a call of the definition on the overlap reads it: its body's type with the
    -- arguments it refers to substituted by what the call gives them.
    let result = unionAllTypes typeEnv
          [ dest
          | UnionType Nothing leafs [] <- [inputType solvedSubject]
          , src <- splitUnionType leafs
          , dest <- arrowDestType typeEnv H.empty src solvedSubject ]
    let unproven = unprovenLeaves typeEnv claim result
    if null unproven
      then return ()
      else proofErr $ intercalate "\n\t" $
        [ "the result can not be shown to convert to the claimed type"
        , printf "Inferred result: %s" (show result)
        , printf "Unproven part: %s" (intercalate ", " $ map show unproven)
        , printf "Unproven difference: %s" (show $ differenceTypeEnv typeEnv result claim)
        ] ++ predNote claim
  Just (Nothing, _) | isJust (getRuntimeAnnot (oaAnnots rpDef)) -> proofErr "it is a #runtime primitive, which has no body to prove"
  _ -> proofErr "it has no body to prove"
  where
    proofErr :: String -> CRes a
    proofErr msg = CErr [MkCNote $ GenCErr defPos $ printf "Could not prove %s for the definition at %s on the input %s, claimed to give %s, because %s"
                           (nameStr rpName) (maybe "an unknown location" showCodeRange defPos) (show rpOverlap) (maybe "nothing" (show . getMetaType . snd) (oaArr rpRefine)) msg]
    -- An operator's object is written around its symbol and carries no position of its own, so
    -- such a definition is placed by its body.
    defPos = case getMetaPos . getExprMeta =<< oaObj rpDef of
      Just pos -> Just pos
      Nothing  -> getMetaPos . getExprMeta =<< (fst =<< oaArr rpDef)
    predNote claim = ["The claim states value predicates, which the type checker can not decide yet" | hasPredExpr claim]
    hasPredExpr (UnionType (Just (preds, _, _)) _ _) = predsHasExpr preds
    hasPredExpr _                                    = False
    predsHasExpr (PredsOne PredExpr{}) = True
    predsHasExpr (PredsOne _)          = False
    predsHasExpr (PredsAnd ps)         = any predsHasExpr ps
    predsHasExpr (PredsNot p)          = predsHasExpr p

-- | Prove the refinement of an existential function: each overlapping definition is attempted as
-- 'proveRefinement' proves it, and the claim holds when the overlaps of those that prove it cover
-- the required input. A definition which does not prove it is no failure in itself — another may be
-- the result on that input — so its reason is reported only when the cover falls short.
--
-- The induction hypothesis each attempt reads is the claim over the refinement's whole input, now
-- an existence claim: for each input some arrow gives a result within it. That is what the cover
-- establishes, by induction over the proven definitions together (and with the same termination
-- caveat as 'opaqueDeps').
proveAnyRefinement :: TPrgm -> TypeName -> TObjArr -> Type -> [(CRes PObjArr, RefinementProof)] -> CRes ()
proveAnyRefinement tprgm name refine required attempts =
  if isSubtypeOf typeEnv required proven
    then return ()
    else CErr [MkCNote $ GenCErr refinePos $ intercalate "\n\t" $
      [ printf "Could not prove %s on the input %s, claimed to give %s, because the definitions which prove it do not cover the input" (nameStr name) (show required) claim
      , printf "Proven input: %s" (show proven)
      , printf "Uncovered input: %s" (show $ differenceTypeEnv typeEnv required proven)
      ] ++ map prettyCNotes failures]
  where
    typeEnv = mkTypeEnv tprgm
    results = [ (proof, preDef >>= \d -> proveRefinement tprgm d proof) | (preDef, proof) <- attempts ]
    proven = unionAllTypes typeEnv [ rpOverlap proof | (proof, CRes _ ()) <- results ]
    failures = [ ns | (_, CErr ns) <- results ]
    claim = maybe "nothing" (show . getMetaType . snd) (oaArr refine)
    refinePos = getMetaPos . getExprMeta =<< oaObj refine

-- | The definition to typecheck again: under the fresh 'proofSubjectName', with its input narrowed
-- to the overlap. The input of an object is the type at the top of its expression, which the solve
-- reads as the actual of the object and carries into each argument.
--
-- Its annotations are dropped (they are checked with the definition itself) and so is its declared
-- result: it is the declaration's, read in the declaration's scope, and the claim is checked on the
-- inferred result rather than assumed from it.
proofSubject :: TypeName -> Type -> PObjArr -> CRes PObjArr
proofSubject name overlap oa@ObjArr{oaObj=Just obj, oaArr=Just (body, arrM)} = do
  overlap' <- case renameType overlap of
    Just t  -> return t
    Nothing -> CErr [MkCNote $ GenCErr (getMetaPos $ getExprMeta obj) $ printf "Could not narrow %s to the overlap %s, which is not a union of objects" (nameStr name) (show overlap)]
  let renameMeta m = mWithType (fromMaybe (getMetaType m) $ renameType (getMetaType m)) m
  let obj' = renameSpine renameMeta obj
  return oa{ oaObj = Just $ exprWithMeta (mWithType overlap' (getExprMeta obj')) obj'
           , oaAnnots = []
           , oaArr = Just (body, mWithType PTopType arrM)
           }
  where
    renameType (UnionType Nothing leafs []) = Just $ UnionType Nothing (joinUnionType $ map renamePartial $ splitUnionType leafs) []
    renameType _ = Nothing
    renamePartial p | ptName p == name = p{ptName = proofSubjectName}
    renamePartial p = p
    renameSpine f (Value m _) = Value (f m) proofSubjectName
    renameSpine f (EWhere m b c) = EWhere (f m) (renameSpine f b) c
    renameSpine f (TupleApply m (bm, b) a) = TupleApply (f m) (f bm, renameSpine f b) a
    renameSpine f (AliasExpr b a) = AliasExpr (renameSpine f b) a
    renameSpine _ e = e
proofSubject name _ _ = CErr [MkCNote $ GenCErr Nothing $ printf "Could not prove %s for a definition without an input and a body" (nameStr name)]

-- | The typechecked program with the function being proven made OPAQUE on the refinement's input:
-- its definitions and runtime declarations overlapping the refinement are removed, and its only
-- refinement is the one being proven. A call into the function on that input then finds only its
-- declarations and the claim, so the call's result is the claim: the induction hypothesis.
--
-- That is sound because every removed arm is itself an obligation of the same refinement, so the
-- claim holds over the refinement's whole input by induction over all of them together.
--
-- TODO(termination): the induction needs the recursion to be well founded. Nothing checks that
-- yet, so a definition such as @loop(n)= loop(n= n)@ is proven of any claim.
opaqueDeps :: TypeName -> TObjArr -> TPrgm -> TPrgm
opaqueDeps name refine prgm@(Prgm (ObjectMap om) cg annots) = Prgm (ObjectMap $ H.adjust opaque name om) cg annots
  where
    typeEnv = mkTypeEnv prgm
    disjoint oa = isBottomType $ intersectTypes typeEnv (inputType refine) (inputType oa)
    opaque omi = omi{ omiRefineDecls = [refine]
                    , omiDefs = filter disjoint (omiDefs omi)
                    , omiRuntimeDecls = filter disjoint (omiRuntimeDecls omi)
                    }

-- | The parts of a result that are not shown to convert to the claim. A leaf converts when it is
-- already within the claim, or when it is a form the program's arrows rewrite and every value of it
-- is accepted by some arrow whose result converts in turn — the arrows are followed the way
-- dispatch follows them, so one arrow's result vouches only for the inputs that arrow accepts.
--
-- A form already visited on the way down is not a proof of itself, so a cycle of rewrites converts
-- nothing.
unprovenLeaves :: (TypeGraph tg) => TypeEnv tg -> Type -> Type -> [Type]
unprovenLeaves typeEnv claim = typeUnproven S.empty
  where
    typeUnproven visited t
      | isSubtypeOf typeEnv t claim = []
      | otherwise = case t of
          UnionType Nothing leafs consts -> filter (not . converts visited) (map singletonType (splitUnionType leafs ++ map constantPartialType consts))
          _ -> [t]
    converts visited leafT
      | isSubtypeOf typeEnv leafT claim = True
      | otherwise = case leafT of
          UnionType Nothing leafs [] | [p] <- splitUnionType leafs, not (S.member p visited) ->
            let visited' = S.insert p visited
                arrows = typeGraphQueryWithReason typeEnv H.empty p
                vouched = [ accepted | (_, accepted, dest) <- arrows, null (typeUnproven visited' dest) ]
            in not (null vouched) && isSubtypeOf typeEnv leafT (unionAllTypes typeEnv vouched)
          _ -> False
