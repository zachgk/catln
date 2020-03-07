--------------------------------------------------------------------
-- |
-- Module    :  Desugarf
-- Copyright :  (c) Zach Kimberg 2019
-- License   :  MIT
-- Maintainer:  zachary@kimberg.com
-- Stability :  experimental
-- Portability: non-portable
--
--------------------------------------------------------------------

module Desugarf where


import           Data.Hashable
import qualified Data.HashMap.Strict as H
import qualified Data.HashSet        as S
import Data.Graph

import           Syntax
import CallGraph

data SemiDecl m = SemiDecl (DeclLHS m) (Maybe (Expr m))
  deriving (Eq, Ord, Show)

setMeta :: m -> Expr m -> Expr m
setMeta m (CExpr _ c)   = CExpr m c
setMeta m (Tuple _ n es) = Tuple m n es

scopeSubDeclFunNamesInExpr :: Name -> S.HashSet Name -> Expr m -> Expr m
scopeSubDeclFunNamesInExpr _ _ e@CExpr{} = e
scopeSubDeclFunNamesInExpr prefix replaceNames (Tuple m name args) = Tuple m name' args'
  where
    addPrefix n = prefix ++ "." ++ n
    name' = if S.member name replaceNames then addPrefix name else name
    args' = fmap (scopeSubDeclFunNamesInExpr prefix replaceNames) args

-- Renames sub functions by applying the parent names as a prefix to avoid name collisions
scopeSubDeclFunNames :: Name -> [SemiDecl m] -> Maybe (Expr m) -> ([SemiDecl m], Maybe (Expr m))
scopeSubDeclFunNames prefix decls maybeExpr = (decls', expr')
  where
    declNames = S.fromList $ map (\(SemiDecl (DeclLHS _ name _) _) -> name) decls
    addPrefix n = prefix ++ "." ++ n
    decls' = map (\(SemiDecl (DeclLHS m name args) subExpr) -> SemiDecl (DeclLHS m (addPrefix name) args) (fmap (scopeSubDeclFunNamesInExpr prefix declNames) subExpr)) decls
    expr' = fmap (scopeSubDeclFunNamesInExpr prefix declNames) maybeExpr

currySubFunctionSignature :: H.HashMap Name m -> CallGraph -> SemiDecl m -> (SemiDecl m, (Name, H.HashMap Name m))
currySubFunctionSignature parentArgMap (graph, nodeFromVertex, vertexFromKey) (SemiDecl (DeclLHS m name args) expr) = (SemiDecl (DeclLHS m name args') expr, (name, curryArgs))
  where
    getContained n = S.fromList $ map ((\(_, calledName, _) -> calledName) . nodeFromVertex) $ reachable graph $ (\(Just n') -> n') $ vertexFromKey n
    contained = getContained name
    curryArgs = H.filterWithKey (\k _ -> S.member k contained) parentArgMap
    args' = H.union args curryArgs


buildCallGraph :: [SemiDecl m] -> CallGraph
buildCallGraph decls = graphFromEdges $ map fromDecl decls
  where
    fromDecl (SemiDecl (DeclLHS _ name _) Nothing) = ((), name, [])
    fromDecl (SemiDecl (DeclLHS _ name _) (Just expr)) = ((), name, S.toList $ tupleNamesInExpr expr)

currySubFunctions :: H.HashMap Name m -> [SemiDecl m] -> Maybe (Expr m) -> ([SemiDecl m], Maybe (Expr m))
currySubFunctions parentArgMap decls expr = (decls', expr')
  where
    callGraph = buildCallGraph decls
    (decls2, exprUpdateSource) = unzip $ map (currySubFunctionSignature parentArgMap callGraph) decls
    exprUpdateMap = H.fromList exprUpdateSource
    updateExpr c@CExpr{} = c
    updateExpr (Tuple tm tn te) = Tuple tm tn (H.union (fmap updateExpr te) (H.mapWithKey (\argName argM -> Tuple argM argName H.empty) $ H.lookupDefault H.empty tn exprUpdateMap))
    expr' = fmap updateExpr expr
    decls' = map (\(SemiDecl lhs e) -> SemiDecl lhs (fmap updateExpr e)) decls2

removeSubDeclarations :: RawDecl m -> [SemiDecl m]
removeSubDeclarations (RawDecl (DeclLHS m declName args) subDecls expr) = decl':subDecls4
  where
    subDecls2 = concatMap removeSubDeclarations subDecls
    (subDecls3, expr2) = scopeSubDeclFunNames declName subDecls2 expr
    (subDecls4, expr3) = currySubFunctions args subDecls3 expr2
    decl' = SemiDecl (DeclLHS m declName args) expr3

declToObjArrow :: SemiDecl m -> (Object m, [Arrow m])
declToObjArrow (SemiDecl (DeclLHS m name args) expr) = (object, [arrow])
  where
    object = Object m name args
    arrow = Arrow m expr

desDecl :: (Eq m, Hashable m) => RawDecl m -> Prgm m
desDecl decl = H.fromList $ map declToObjArrow $ removeSubDeclarations decl

unionsWith :: (Ord k, Hashable k) => (a->a->a) -> [H.HashMap k a] -> H.HashMap k a
unionsWith f = foldl (H.unionWith f) H.empty

desDecls :: (Eq m, Ord m, Hashable m) => [RawDecl m] -> Prgm m
desDecls decls = unionsWith (++) $ map desDecl decls

desPrgm :: (Eq m, Ord m, Hashable m) => RawPrgm m -> Prgm m
desPrgm = desDecls
