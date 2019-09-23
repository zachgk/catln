{-# LANGUAGE TupleSections #-}
--------------------------------------------------------------------
-- |
-- Module    :  TypeCheck
-- Copyright :  (c) Zach Kimberg 2019
-- License   :  MIT
-- Maintainer:  zachary@kimberg.com
-- Stability :  experimental
-- Portability: non-portable
--
--------------------------------------------------------------------

module TypeCheck where

import           Control.Monad
import           Control.Monad.ST
import           Data.Either
import           Data.Functor
import qualified Data.HashMap.Strict as H
import           Data.UnionFind.ST
import           Data.Void

import           Syntax

type TypeCheckError = String

data Scheme
  = SKnown Type
  | SUnknown
  | SCheckError String
  deriving (Eq, Ord, Show)

type Pnt s = Point s Scheme

data PntDef s
  = PntVal (Pnt s)
  | PntFun (Pnt s) [Pnt s]

data FEnv s = FEnv [Constraint s] (H.HashMap String (PntDef s)) [TypeCheckError]

data Constraint s
  = EqType (Pnt s) Type
  | EqPoints (Pnt s) (Pnt s)

type TypeCheckResult r = Either [TypeCheckError] r

type PreMeta = PreTyped
type PExpr = Expr PreMeta
type PDecl = Decl PreMeta
type PDeclLHS = DeclLHS PreMeta
type PPrgm = Prgm PreMeta
type PReplRes = ReplRes PreMeta

type VarMeta s = Pnt s
type VExpr s = Expr (VarMeta s)
type VDecl s = Decl (VarMeta s)
type VDeclLHS s = DeclLHS (VarMeta s)
type VPrgm s = Prgm (VarMeta s)
type VReplRes s = ReplRes (VarMeta s)

type TypedMeta = Typed
type TExpr = Expr TypedMeta
type TDecl = Decl TypedMeta
type TDeclLHS = DeclLHS TypedMeta
type TPrgm = Prgm TypedMeta
type TReplRes = ReplRes TypedMeta

baseFEnv :: FEnv s
baseFEnv = FEnv [] H.empty []

getPnt :: VarMeta s -> Pnt s
getPnt x = x

addConstraints :: FEnv s -> [Constraint s] -> FEnv s
addConstraints (FEnv oldCons defMap errs) newCons = FEnv (newCons ++ oldCons) defMap errs

fLookup :: FEnv s -> String -> (Maybe (PntDef s), FEnv s)
fLookup env@(FEnv cons map errs) k = case H.lookup k map of
  Just v  -> (Just v, env)
  Nothing -> (Nothing, FEnv cons map (("Failed to lookup " ++ k):errs))

fromMetaP :: FEnv s -> PreMeta -> ST s (VarMeta s, Pnt s, FEnv s)
fromMetaP env (PreTyped mt) = do
  let scheme = case mt of
        Nothing -> SUnknown
        Just t  -> SKnown t
  p <- fresh scheme
  return (p, p, env)

fromMeta :: FEnv s -> PreMeta -> ST s (VarMeta s, FEnv s)
fromMeta env m = do
  (m', p, env') <- fromMetaP env m
  return (m', env')

fromExpr :: FEnv s -> PExpr -> ST s (VExpr s, FEnv s)
fromExpr env (CExpr m (CInt i)) = do
  (m', p, env') <- fromMetaP env m
  return (CExpr m' (CInt i), addConstraints env' [EqType p intType])
fromExpr env (CExpr m (CFloat f)) = do
  (m', p, env') <- fromMetaP env m
  return (CExpr m' (CFloat f), addConstraints env' [EqType p floatType])
fromExpr env (CExpr m (CStr s)) = do
  (m', p, env') <- fromMetaP env m
  return (CExpr m' (CStr s), addConstraints env' [EqType p strType])
fromExpr env (Var m name) = do
  (m', p, env') <- fromMetaP env m
  let env'' = case fLookup env' name of
              (Nothing, e)          -> e
              (Just (PntVal pp), e) -> addConstraints e [EqPoints p pp]
   in return (Var m' name, env'')
fromExpr env1 (Call m name expressions) = topLevelCall
  where
    topLevelCall = do
          (m', p, env2) <- fromMetaP env1 m
          let (env3, pargs) = case fLookup env2 name of
                (Just (PntFun pp pargs_), e) -> (addConstraints e [EqPoints p pp], pargs_)
            in do
                (vexpressions, env4) <- recurse env3 expressions pargs
                return (Call m' name vexpressions, env4)
    recurse :: FEnv s -> [PExpr] -> [Pnt s] -> ST s ([VExpr s], FEnv s)
    recurse e [] [] = return ([], e)
    recurse e (expr:exprs) (parg:pargs) = do
      (vexpr, e') <- fromExpr e expr
      (vexprs, e'') <- recurse e' exprs pargs
      return (vexpr:vexprs, addConstraints e'' [EqPoints parg (getPnt $ getExprMeta vexpr)])

fromDeclLHS :: FEnv s -> PDeclLHS -> ST s (VDeclLHS s, FEnv s)
fromDeclLHS env (DeclVal name) = return (DeclVal name, env)
fromDeclLHS env (DeclFun name []) = return (DeclFun name [], env)
fromDeclLHS env (DeclFun name ((n, m):args)) = do
  (m', env') <- fromMeta env m
  (DeclFun _ vargs, env'') <- fromDeclLHS env' (DeclFun name args)
  return (DeclFun name ((n, m'):vargs), env'')

fromDecl :: FEnv s -> PDecl -> ST s (VDecl s, FEnv s)
fromDecl env1 (Decl lhs subDecls expr) = do
  (vSubDecls, env2) <- fromDecls env1 subDecls
  (vlhs, env3) <- fromDeclLHS env2 lhs
  (vExpr, env4) <- fromExpr env3 expr
  let vdecl = Decl vlhs vSubDecls vExpr
  return (vdecl, env4)

fromDecls :: FEnv s -> [PDecl] -> ST s ([VDecl s], FEnv s)
fromDecls env [] = return ([], env)
fromDecls env (decl:decls) = do
  (vdecl, env') <- fromDecl env decl
  (vdecls, env'') <- fromDecls env' decls
  return (vdecl:vdecls, env'')

fromPrgm :: FEnv s -> PPrgm -> ST s (VPrgm s, FEnv s)
fromPrgm env ([], [], decls) = fromDecls env decls >>= (\(vdecls, env') -> return (([], [], vdecls), env'))

executeConstraint :: Constraint s -> ST s ()
executeConstraint (EqType pnt tp) = modifyDescriptor pnt (\oldTp ->
                                                            case oldTp of
                                                              SUnknown -> SKnown tp
                                                              SKnown t | t == tp -> oldTp
                                                              SKnown badTp -> SCheckError $ concat ["Mismatched types: ", show tp, " and ", show badTp]
                                                              SCheckError s -> SCheckError s
                                                         )
executeConstraint (EqPoints p1 p2) = union' p1 p2 (\s1 s2 -> return $ case (s1, s2) of
                                                      (SUnknown, SUnknown) -> SUnknown
                                                      (SKnown t, SUnknown) -> SKnown t
                                                      (SUnknown, SKnown t) -> SKnown t
                                                      (SKnown t1, SKnown t2) -> if t1 == t2 then SKnown t1 else SCheckError $ concat ["Mismatched types: ", show t1, " and ", show t2]
                                                      (_, SCheckError s) -> SCheckError s
                                                      (SCheckError s, _) -> SCheckError s
                                                  )

mergeTypeCheckResults :: [TypeCheckResult r] -> TypeCheckResult [r]
mergeTypeCheckResults = foldl addToTotal (Right [])
  where addToTotal (Right accRes) (Right newRes) = Right (newRes:accRes)
        addToTotal a b = Left $ fromLeft [] a ++ fromLeft [] b

toMeta :: VarMeta s -> ST s (TypeCheckResult Typed)
toMeta p = do
  scheme <- descriptor p
  return $ case scheme of
    SKnown tp     -> return $ Typed tp
    SUnknown      -> Left []
    SCheckError s -> Left [s]

toExpr :: VExpr s -> ST s (TypeCheckResult TExpr)
toExpr (CExpr m c) = do
  res <- toMeta m
  return $ res <&> (`CExpr` c)
toExpr (Var m name) = do
  res <- toMeta m
  return $ case res of
    Right m' -> Right $ Var m' name
    Left _   -> Left ["Could not find type for " ++ name]
toExpr (Call m name exprs) = do
  res1 <- toMeta m
  res2 <- mapM toExpr exprs
  return $ case (res1, mergeTypeCheckResults res2) of
    (Right m', Right exprs') -> Right $ Call m' name exprs'
    (a, b) -> Left $ ["Could not find type for " ++ name | isLeft a] ++ fromLeft [] b

toDeclLHS :: VDeclLHS s -> ST s (TypeCheckResult TDeclLHS)
toDeclLHS (DeclVal name ) = return $ return $ DeclVal name
toDeclLHS (DeclFun name [] ) = return $ return $ DeclFun name []
toDeclLHS (DeclFun name ((n, m):args) ) = do
  res1 <- toMeta m
  res2 <- toDeclLHS (DeclFun name args)
  return $ case (res1, res2) of
    (Right m', Right (DeclFun _ targs)) -> Right (DeclFun name ((n, m'):targs))
    (a, b) -> Left $ fromLeft [] a ++ fromLeft [] b


toDecl :: VDecl s -> ST s (TypeCheckResult TDecl)
toDecl (Decl lhs subDecls expr) = do
  res1 <- toDeclLHS lhs
  res2 <- mapM toDecl subDecls
  res3 <- toExpr expr
  return $ case (res1, mergeTypeCheckResults res2, res3) of
    (Right lhs', Right subDecls', Right expr') -> Right $ Decl lhs' subDecls' expr'
    (a, b, c) -> Left $ concat [fromLeft [] a, fromLeft [] b, fromLeft [] c]

toPrgm :: VPrgm s -> ST s (TypeCheckResult TPrgm)
toPrgm ([], [], decls) = do
  res <- mapM toDecl decls
  return $ mergeTypeCheckResults res <&> ([], [],)

typecheckPrgm :: PPrgm -> TypeCheckResult TPrgm
typecheckPrgm ppgrm = runST $ do
  (vpgrm, (FEnv cons _ errs)) <- fromPrgm baseFEnv ppgrm
  mapM_ executeConstraint cons
  toPrgm vpgrm
