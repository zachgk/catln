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

data SemiPDecl s = SemiPDecl (DeclLHS (VarMeta s)) PExpr
  deriving (Eq)

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

makeBaseFEnv :: ST s (FEnv s)
makeBaseFEnv = do
  let env1 = FEnv [] H.empty []
  let ops = [ ("+", intType, [intType, intType])
            , ("-", intType, [intType, intType])
            , ("*", intType, [intType, intType])
            , (">", boolType, [intType, intType])
            , ("<", boolType, [intType, intType])
            , (">=", boolType, [intType, intType])
            , ("<=", boolType, [intType, intType])
            , ("==", boolType, [intType, intType])
            , ("!=", boolType, [intType, intType])
            , ("&", boolType, [boolType, boolType])
            , ("~", boolType, [boolType])
            ]
  foldM f env1 ops
  where f e (opName, retType, args) = do
          p <- fresh (SKnown retType)
          pargs <- forM args (fresh . SKnown )
          return $ fInsert e opName (PntFun p pargs)

getPnt :: VarMeta s -> Pnt s
getPnt x = x

addErr :: FEnv s -> String -> FEnv s
addErr (FEnv cons pmap errs) newErr = FEnv cons pmap (newErr:errs)

addConstraints :: FEnv s -> [Constraint s] -> FEnv s
addConstraints (FEnv oldCons defMap errs) newCons = FEnv (newCons ++ oldCons) defMap errs

fLookup :: FEnv s -> String -> (Maybe (PntDef s), FEnv s)
fLookup env@(FEnv _ pmap _) k = case H.lookup k pmap of
  Just v  -> (Just v, env)
  Nothing -> (Nothing, addErr env ("Failed to lookup " ++ k))

fInsert :: FEnv s -> String -> PntDef s -> FEnv s
fInsert (FEnv cons pmap errs) k v = FEnv cons (H.insert k v pmap) errs

fReplaceMap :: FEnv s -> FEnv s -> FEnv s
fReplaceMap (FEnv cons _ errs1) (FEnv _ pmap errs2) = FEnv cons pmap (errs1 ++ errs2)

fromMetaP :: FEnv s -> PreMeta -> ST s (VarMeta s, Pnt s, FEnv s)
fromMetaP env (PreTyped mt) = do
  let scheme = case mt of
        Nothing -> SUnknown
        Just t  -> SKnown t
  p <- fresh scheme
  return (p, p, env)

fromMeta :: FEnv s -> PreMeta -> ST s (VarMeta s, FEnv s)
fromMeta env m = do
  (m', _, env') <- fromMetaP env m
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
              _                     -> error "Failed to find variable"
   in return (Var m' name, env'')
fromExpr env1 (Call m name expressions) = topLevelCall
  where
    topLevelCall = do
          (m', p, env2) <- fromMetaP env1 m
          let (env3, pargs) = case fLookup env2 name of
                (Just (PntFun pp pargs_), e) -> (addConstraints e [EqPoints p pp], pargs_)
                (Nothing, _) -> error $ "Invalid state - Could not find " ++ name
                _ -> error "Failed to find function"
            in do
                (vexpressions, env4) <- recurse env3 expressions pargs
                return (Call m' name vexpressions, env4)
    recurse :: FEnv s -> [PExpr] -> [Pnt s] -> ST s ([VExpr s], FEnv s)
    recurse e [] [] = return ([], e)
    recurse e (expr:exprs) (parg:pargs) = do
      (vexpr, e') <- fromExpr e expr
      (vexprs, e'') <- recurse e' exprs pargs
      return (vexpr:vexprs, addConstraints e'' [EqPoints parg (getPnt $ getExprMeta vexpr)])
    recurse _ _ _ = error "Invalid state"

fromDeclAddScope :: FEnv s -> VDeclLHS s -> ST s (FEnv s)
fromDeclAddScope env DeclVal{} = return env
fromDeclAddScope env (DeclFun _ _ []) = return env
fromDeclAddScope env (DeclFun _ _ args) = foldM aux env args
  where aux e (n, m) = return $ fInsert e n (PntVal $ getPnt m)

fromDecl :: FEnv s -> SemiPDecl s -> ST s (VDecl s, FEnv s)
fromDecl env1 (SemiPDecl lhs expr) = do
  env2 <- fromDeclAddScope env1 lhs
  (vExpr, env3) <- fromExpr env2 expr
  let env4 = addConstraints env3 [EqPoints (getPnt $ getDeclLHSMeta lhs) (getPnt $ getExprMeta vExpr)]
  let vdecl = Decl lhs vExpr
  return (vdecl, fReplaceMap env4 env1)

fromDecls :: FEnv s -> [SemiPDecl s] -> ST s ([VDecl s], FEnv s)
fromDecls env [] = return ([], env)
fromDecls env (sdecl:sdecls) = do
  (vdecl, env') <- fromDecl env sdecl
  (vdecls, env'') <- fromDecls env' sdecls
  return (vdecl:vdecls, env'')

addCallableArgs :: FEnv s -> [(Name, PreMeta)] -> ST s ([(Name, VarMeta s)], FEnv s)
addCallableArgs env [] = return ([], env)
addCallableArgs env ((n, m):args) = do
  (m', env2) <- fromMeta env m
  (args', env3) <- addCallableArgs env2 args
  return ((n, m'):args', env3)

addCallables :: FEnv s -> [PDecl] -> ST s ([SemiPDecl s], FEnv s)
addCallables env [] = return ([], env)
addCallables env (Decl (DeclVal m name) e:decls) = do
  (m', p, env1) <- fromMetaP env m
  let env2 = fInsert env1 name (PntVal p)
  let sdecl = SemiPDecl (DeclVal m' name) e
  (sdecls, env3) <- addCallables env2 decls
  return (sdecl:sdecls, env3)
addCallables env (Decl (DeclFun m name args) e:decls) = do
  (m', p, env1) <- fromMetaP env m
  let env2 = fInsert env1 name (PntVal p)
  (args', env3) <- addCallableArgs env2 args
  let sdecl = SemiPDecl (DeclFun m' name args') e
  (sdecls, env4) <- addCallables env3 decls
  return (sdecl:sdecls, env4)


fromPrgm :: FEnv s -> PPrgm -> ST s (VPrgm s, FEnv s)
fromPrgm env decls = do
  (sdecls, env') <- addCallables env decls
  (vdecls, env'') <- fromDecls env' sdecls
  return (vdecls, env'')

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

merge2TypeCheckResults :: TypeCheckResult a -> TypeCheckResult b -> TypeCheckResult (a, b)
merge2TypeCheckResults a b = case (a, b) of
  (Right a', Right b') -> Right (a', b')
  (a', b')             -> Left $ fromLeft [] a' ++ fromLeft [] b'

mergeTypeCheckResults :: [TypeCheckResult r] -> TypeCheckResult [r]
mergeTypeCheckResults res = case partitionEithers res of
  ([], rs) -> Right rs
  (ls, _)  -> Left $ concat ls

toMeta :: VarMeta s -> String -> ST s (TypeCheckResult Typed)
toMeta p name = do
  scheme <- descriptor p
  return $ case scheme of
    SKnown tp     -> return $ Typed tp
    SUnknown      -> Left ["Unknown " ++ name]
    SCheckError s -> Left [s]

toExpr :: VExpr s -> ST s (TypeCheckResult TExpr)
toExpr (CExpr m c) = do
  res <- toMeta m $ "Constant " ++ show c
  return $ res <&> (`CExpr` c)
toExpr (Var m name) = do
  res <- toMeta m $ "variable " ++ name
  return $ res >>= (\m' -> Right $ Var m' name)
toExpr (Call m name exprs) = do
  res1 <- toMeta m $ "Function call " ++ name
  res2 <- mapM toExpr exprs
  return $ case (res1, mergeTypeCheckResults res2) of
    (Right m', Right exprs') -> Right $ Call m' name exprs'
    (a, b)                   -> Left $ fromLeft [] a ++ fromLeft [] b

toDeclLHS :: VDeclLHS s -> ST s (TypeCheckResult TDeclLHS)
toDeclLHS (DeclVal m name ) = do
  res1 <- toMeta m $ "Value Declaration " ++ name
  return $ res1 >>= (\m' -> Right $ DeclVal m' name)
toDeclLHS (DeclFun m name [] ) = do
  res1 <- toMeta m $ "Function Declaration Return Type " ++ name
  return $ res1 >>= (\m' -> Right $ DeclFun m' name [])
toDeclLHS (DeclFun m name ((an, am):args) ) = do
  res1 <- toMeta am $ "Function Declaration Argument: " ++ name ++ "." ++ an
  res2 <- toDeclLHS (DeclFun m name args)
  return $ merge2TypeCheckResults res1 res2 >>= (\(am', DeclFun m' _ args') -> Right $ DeclFun m' name ((an, am'):args'))


toDecl :: VDecl s -> ST s (TypeCheckResult TDecl)
toDecl (Decl lhs expr) = do
  res1 <- toDeclLHS lhs
  res2 <- toExpr expr
  return $ merge2TypeCheckResults res1 res2 >>= (\(lhs', expr') -> Right $ Decl lhs' expr')

toPrgm :: VPrgm s -> ST s (TypeCheckResult TPrgm)
toPrgm decls = do
  res <- mapM toDecl decls
  return $ mergeTypeCheckResults res

typecheckPrgm :: PPrgm -> TypeCheckResult TPrgm
typecheckPrgm ppgrm = runST $ do
  baseFEnv <- makeBaseFEnv
  (vpgrm, FEnv cons _ errs) <- fromPrgm baseFEnv ppgrm
  case errs of
    [] -> do
      mapM_ executeConstraint cons
      toPrgm vpgrm
    _ -> return $ Left errs
