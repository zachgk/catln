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

{-# LANGUAGE LambdaCase #-}

module Desugarf where


import qualified Data.HashMap.Strict as H
import qualified Data.HashSet        as S

import           Syntax

data SemiDecl m = SemiDecl (DeclLHS m) (Expr m)
  deriving (Eq, Ord, Show)

desExpr :: RawExpr m -> Expr m
desExpr = id

setMeta :: m -> Expr m -> Expr m
setMeta m (CExpr _ c)   = CExpr m c
setMeta m (Var _ n)     = Var m n
setMeta m (Tuple _ n es) = Tuple m n es

replaceVars :: H.HashMap Name (Expr m) -> Expr m -> Expr m
replaceVars _ e@(CExpr _ _) = e
replaceVars reps e@(Var m name) = case H.lookup name reps of
  Just e' -> setMeta m e'
  Nothing -> e
replaceVars reps (Tuple m n es) = Tuple m n (fmap (replaceVars reps) es)

-- Removes all subVariable declarations by replacing with their expressions
-- removeSubDeclVal :: RawDecl m -> RawDecl m
-- removeSubDeclVal (RawDecl lhs decls expr) = RawDecl lhs funs' expr'
--   where (vars, funs) =  unzip $ map (\case
--                              RawDecl (DeclLHS m name []) [] subE -> ([(name, setMeta m subE)], [])
--                              f@RawDecl{} -> ([], [f])) decls
--         replaceList = H.fromList $ concat vars
--         expr' = replaceVars replaceList expr
--         replaceInFun (RawDecl l [] e) = RawDecl l (replaceVars replaceList e)
--         funs' = map replaceInFun $ concat funs

-- scopeSubDeclFunNamesE :: H.HashMap Name Name -> Expr m -> Expr m
-- scopeSubDeclFunNamesE nameMap (Tuple m n es) = Tuple m n' es'
--   where n' = H.lookupDefault n n nameMap
--         es' = fmap (scopeSubDeclFunNamesE nameMap) es
-- scopeSubDeclFunNamesE _ e = e

-- -- Renames sub functions by applying the parent names as a prefix to avoid name collisions
-- scopeSubDeclFunNamesD :: Name -> H.HashMap Name Name -> [SemiDecl] m -> [SemiDecl] m
-- scopeSubDeclFunNamesD prefix nameMap (SemiDecl lhs@(DeclLHS _ dn _) subDecls expr) = SemiDecl lhs subDecls' expr'
--   where prefix' = prefix ++ '.' : dn
--         getSubDeclName (Decl (DeclLHS _ n _) _) = n
--         subDeclNames = map getSubDeclName subDecls
--         nameMap' = foldl (\curMap name -> H.insert name (prefix' ++ name) curMap) nameMap subDeclNames
--         subDecls' = map (\(Decl l e) -> Decl l (scopeSubDeclFunNamesE nameMap' e)) subDecls
--         expr' = scopeSubDeclFunNamesE nameMap' expr

-- curryFun :: H.HashMap Name [Expr m] -> Expr m -> Expr m
-- curryFun replaceMap (Call m name args) = Call m name (newArgs ++ args)
--   where newArgs = H.lookupDefault [] name replaceMap
-- curryFun _ e = e

-- Desugars declaration sub functions by currying the lhs terms and scoping the name
-- curryFind :: DeclLHS m -> Decl m -> (Name, [Expr m])
-- curryFind (DeclLHS _ _ []) _ = error "Should call curryFind after desDeclVals"
-- curryFind (DeclLHS _ _ parentTerms) (Decl (DeclLHS _ name _) expr) = (name, valTermsToCurry)
--   where parentSet = S.fromList (map fst parentTerms)
--         parentMap = H.fromList parentTerms
--         notUsedIn potentials (CExpr _ _) = potentials
--         notUsedIn potentials (Var _ n) = if S.member n potentials then S.delete n potentials else potentials
--         notUsedIn potentials (Call _ _ ces) = foldl S.intersection potentials $ map (notUsedIn potentials) ces
--         termsToCurry = notUsedIn parentSet expr
--         valTermsToCurry = map (\(t, m) -> Var m t)$ H.toList $ H.filterWithKey (\t _ -> S.member t termsToCurry) parentMap

-- Removes all subFunction declarations by currying the function and lifting into top scope
-- curryDeclD :: SemiDecl m -> [Decl m]
-- curryDeclD (SemiDecl lhs subFunctions expr) = newParent:subFunctions'
--   where newSubSignatures = map (curryFind lhs) subFunctions
--         replaceMap = H.fromList newSubSignatures
--         newParent = Decl lhs (curryFun replaceMap expr)
--         subFunctions' = map (\(Decl l e) -> Decl l (curryFun replaceMap e)) subFunctions


scopeSubDeclFunNamesInExpr :: Name -> S.HashSet Name -> Expr m -> Expr m
scopeSubDeclFunNamesInExpr _ _ e@CExpr{} = e
-- scopeSubDeclFunNamesE nameMap (Tuple m n es) = Tuple m n' es'
--   where n' = H.lookupDefault n n nameMap
--         es' = fmap (scopeSubDeclFunNamesE nameMap) es
-- scopeSubDeclFunNamesE _ e = e

-- Renames sub functions by applying the parent names as a prefix to avoid name collisions
scopeSubDeclFunNames :: Name -> [SemiDecl m] -> [SemiDecl m]
scopeSubDeclFunNames prefix decls = decls'
  where
    declNames = S.fromList $ map (\(SemiDecl (DeclLHS _ name _) _) -> name) decls
    addPrefix n = prefix ++ "." ++ n
    decls' = map (\(SemiDecl (DeclLHS m name args) expr) -> SemiDecl (DeclLHS m (addPrefix name) args) (scopeSubDeclFunNamesInExpr prefix declNames expr)) decls

removeSubDeclarations :: RawDecl m -> [SemiDecl m]
removeSubDeclarations (RawDecl (DeclLHS m declName args) subDecls expr) = undefined
  where
    subDecls2 = concatMap removeSubDeclarations subDecls
    subDecls3 = scopeSubDeclFunNames declName subDecls2

desDecl :: RawDecl m -> ([Object m], [Arrow m])
desDecl = undefined
-- desDecl (RawDecl lhs rsubDecls rexpr) = decls
--   where (subObjects, subArrows) = desDecls rsubDecls
--         expr = desExpr rexpr
--         sdecl1 = SemiDecl lhs expr
--         sdecl2 = removeSubDeclVal sdecl1
--         sdecl3 = scopeSubDeclFunNamesD "" H.empty sdecl2
--         decls = curryDeclD sdecl3

desDecls :: [RawDecl m] -> ([Object m], [Arrow m])
desDecls decls = let (objects', arrows') = unzip $ map desDecl decls
                  in (concat objects', concat arrows')

desPrgm :: RawPrgm m -> Prgm m
desPrgm = desDecls
