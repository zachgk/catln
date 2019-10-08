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

data SemiDecl m = SemiDecl (DeclLHS m) [Decl m] (Expr m)
  deriving (Eq, Ord, Show)

desExpr :: RawExpr m -> Expr m
desExpr = id

setMeta :: m -> Expr m -> Expr m
setMeta m (CExpr _ c)   = CExpr m c
setMeta m (Var _ n)     = Var m n
setMeta m (Call _ n es) = Call m n es

replaceVars :: H.HashMap Name (Expr m) -> Expr m -> Expr m
replaceVars _ e@(CExpr _ _) = e
replaceVars reps e@(Var m name) = case H.lookup name reps of
  Just e' -> setMeta m e'
  Nothing -> e
replaceVars reps (Call m n es) = Call m n (map (replaceVars reps) es)

-- Removes all subVariable declarations by replacing with their expressions
removeSubDeclVal :: SemiDecl m -> SemiDecl m
removeSubDeclVal (SemiDecl lhs decls expr) = SemiDecl lhs funs' expr'
  where (vars, funs) =  unzip $ map (\case
                             Decl (DeclLHS m name []) subE -> ([(name, setMeta m subE)], [])
                             f@(Decl _ _) -> ([], [f])) decls
        replaceList = H.fromList $ concat vars
        expr' = replaceVars replaceList expr
        replaceInFun (Decl l e) = Decl l (replaceVars replaceList e)
        funs' = map replaceInFun $ concat funs

scopeSubDeclFunNamesE :: H.HashMap Name Name -> Expr m -> Expr m
scopeSubDeclFunNamesE nameMap (Call m n es) = Call m n' es'
  where n' = H.lookupDefault n n nameMap
        es' = map (scopeSubDeclFunNamesE nameMap) es
scopeSubDeclFunNamesE _ e = e

-- Renames sub functions by applying the parent names as a prefix to avoid name collisions
scopeSubDeclFunNamesD :: Name -> H.HashMap Name Name -> SemiDecl m -> SemiDecl m
scopeSubDeclFunNamesD prefix nameMap (SemiDecl lhs@(DeclLHS _ dn _) subDecls expr) = SemiDecl lhs subDecls' expr'
  where prefix' = prefix ++ '.' : dn
        getSubDeclName (Decl (DeclLHS _ n _) _) = n
        subDeclNames = map getSubDeclName subDecls
        nameMap' = foldl (\curMap name -> H.insert name (prefix' ++ name) curMap) nameMap subDeclNames
        subDecls' = map (\(Decl l e) -> Decl l (scopeSubDeclFunNamesE nameMap' e)) subDecls
        expr' = scopeSubDeclFunNamesE nameMap' expr

curryFun :: H.HashMap Name [Expr m] -> Expr m -> Expr m
curryFun replaceMap (Call m name args) = Call m name (newArgs ++ args)
  where newArgs = H.lookupDefault [] name replaceMap
curryFun _ e = e

-- Desugars declaration sub functions by currying the lhs terms and scoping the name
curryFind :: DeclLHS m -> Decl m -> (Name, [Expr m])
curryFind (DeclLHS _ _ []) _ = error "Should call curryFind after desDeclVals"
curryFind (DeclLHS _ _ parentTerms) (Decl (DeclLHS _ name _) expr) = (name, valTermsToCurry)
  where parentSet = S.fromList (map fst parentTerms)
        parentMap = H.fromList parentTerms
        notUsedIn potentials (CExpr _ _) = potentials
        notUsedIn potentials (Var _ n) = if S.member n potentials then S.delete n potentials else potentials
        notUsedIn potentials (Call _ _ ces) = foldl S.intersection potentials $ map (notUsedIn potentials) ces
        termsToCurry = notUsedIn parentSet expr
        valTermsToCurry = map (\(t, m) -> Var m t)$ H.toList $ H.filterWithKey (\t _ -> S.member t termsToCurry) parentMap

-- Removes all subFunction declarations by currying the function and lifting into top scope
curryDeclD :: SemiDecl m -> [Decl m]
curryDeclD (SemiDecl lhs subFunctions expr) = newParent:subFunctions'
  where newSubSignatures = map (curryFind lhs) subFunctions
        replaceMap = H.fromList newSubSignatures
        newParent = Decl lhs (curryFun replaceMap expr)
        subFunctions' = map (\(Decl l e) -> Decl l (curryFun replaceMap e)) subFunctions

desDecl :: RawDecl m -> [Decl m]
desDecl (RawDecl lhs rsubDecls rexpr) = decls
  where subDecls = concatMap desDecl rsubDecls
        expr = desExpr rexpr
        sdecl1 = SemiDecl lhs subDecls expr
        sdecl2 = removeSubDeclVal sdecl1
        sdecl3 = scopeSubDeclFunNamesD "" H.empty sdecl2
        decls = curryDeclD sdecl3

desPrgm :: RawPrgm m -> Prgm m
desPrgm = concatMap desDecl
