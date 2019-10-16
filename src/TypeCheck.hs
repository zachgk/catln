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
  = SType RawType RawType
  | SCheckError String
  deriving (Eq, Ord, Show)

type Pnt s = Point s Scheme

data PntDef s
  = PntVal (Pnt s)
  | PntFun (Pnt s) [Pnt s]

data FEnv s = FEnv [Constraint s] (H.HashMap String (PntDef s)) [TypeCheckError]

data Constraint s
  = EqType (Pnt s) RawType
  | EqPoints (Pnt s) (Pnt s)
  | HasType (Pnt s) (Pnt s) -- May need variations that affect only the super or sub type
  deriving (Eq)

type TypeCheckResult r = Either [TypeCheckError] r

type PreMeta = PreTyped
type PExpr = Expr PreMeta
type PDeclLHS = DeclLHS PreMeta
type PPrgm = Prgm PreMeta
type PReplRes = ReplRes PreMeta

data SemiPDecl s = SemiPDecl (DeclLHS (VarMeta s)) PExpr
  deriving (Eq)

type VarMeta s = Pnt s
type VExpr s = Expr (VarMeta s)
type VDeclLHS s = DeclLHS (VarMeta s)
type VPrgm s = Prgm (VarMeta s)
type VReplRes s = ReplRes (VarMeta s)

type TypedMeta = Typed
type TExpr = Expr TypedMeta
type TDeclLHS = DeclLHS TypedMeta
type TPrgm = Prgm TypedMeta
type TReplRes = ReplRes TypedMeta

makeBaseFEnv :: ST s (FEnv s)
makeBaseFEnv = do
  let env1 = FEnv [] H.empty []
  let ops = [ ("+", rintType, [rintType, rintType])
            , ("-", rintType, [rintType, rintType])
            , ("*", rintType, [rintType, rintType])
            , (">", rboolType, [rintType, rintType])
            , ("<", rboolType, [rintType, rintType])
            , (">=", rboolType, [rintType, rintType])
            , ("<=", rboolType, [rintType, rintType])
            , ("==", rboolType, [rintType, rintType])
            , ("!=", rboolType, [rintType, rintType])
            , ("&", rboolType, [rboolType, rboolType])
            , ("~", rboolType, [rboolType])
            ]
  foldM f env1 ops
  where f e (opName, retType, args) = do
          p <- fresh (SType RawBottomType retType)
          pargs <- forM args (fresh . SType RawBottomType )
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
  p <- fresh (SType RawBottomType mt)
  return (p, p, env)

fromMeta :: FEnv s -> PreMeta -> ST s (VarMeta s, FEnv s)
fromMeta env m = do
  (m', _, env') <- fromMetaP env m
  return (m', env')

fromExpr :: FEnv s -> PExpr -> ST s (VExpr s, FEnv s)
fromExpr env (CExpr m (CInt i)) = do
  (m', p, env') <- fromMetaP env m
  return (CExpr m' (CInt i), addConstraints env' [EqType p rintType])
fromExpr env (CExpr m (CFloat f)) = do
  (m', p, env') <- fromMetaP env m
  return (CExpr m' (CFloat f), addConstraints env' [EqType p rfloatType])
fromExpr env (CExpr m (CStr s)) = do
  (m', p, env') <- fromMetaP env m
  return (CExpr m' (CStr s), addConstraints env' [EqType p rstrType])
fromExpr env (Var m name) = do
  (m', p, env') <- fromMetaP env m
  let env'' = case fLookup env' name of
              (Nothing, e)          -> e
              (Just (PntVal pp), e) -> addConstraints e [EqPoints p pp]
              _                     -> error "Failed to find variable"
   in return (Var m' name, env'')
-- fromExpr env1 (Call m name expressions) = topLevelCall
--   where
--     topLevelCall = do
--           (m', p, env2) <- fromMetaP env1 m
--           let (env3, pargs) = case fLookup env2 name of
--                 (Just (PntFun pp pargs_), e) -> (addConstraints e [EqPoints p pp], pargs_)
--                 (Nothing, _) -> error $ "Invalid state - Could not find " ++ name
--                 _ -> error "Failed to find function"
--             in do
--                 (vexpressions, env4) <- recurse env3 expressions pargs
--                 return (Call m' name vexpressions, env4)
--     recurse :: FEnv s -> [PExpr] -> [Pnt s] -> ST s ([VExpr s], FEnv s)
--     recurse e [] [] = return ([], e)
--     recurse e (expr:exprs) (parg:pargs) = do
--       (vexpr, e') <- fromExpr e expr
--       (vexprs, e'') <- recurse e' exprs pargs
--       return (vexpr:vexprs, addConstraints e'' [EqPoints parg (getPnt $ getExprMeta vexpr)])
--     recurse _ _ _ = error "Invalid state"

fromDeclAddScope :: FEnv s -> VDeclLHS s -> ST s (FEnv s)
fromDeclAddScope env (DeclLHS _ _ []) = return env
fromDeclAddScope env (DeclLHS _ _ args) = foldM aux env args
  where aux e (n, m) = return $ fInsert e n (PntVal $ getPnt m)

-- fromDecl :: FEnv s -> SemiPDecl s -> ST s (VDecl s, FEnv s)
-- fromDecl env1 (SemiPDecl lhs@(DeclLHS m' _ _) expr) = do
--   env2 <- fromDeclAddScope env1 lhs
--   (vExpr, env3) <- fromExpr env2 expr
--   let env4 = addConstraints env3 [EqPoints (getPnt $ getExprMeta vExpr) (getPnt m')]
--   let vdecl = Decl lhs vExpr
--   return (vdecl, fReplaceMap env4 env1)

-- fromDecls :: FEnv s -> [SemiPDecl s] -> ST s ([VDecl s], FEnv s)
-- fromDecls env [] = return ([], env)
-- fromDecls env (sdecl:sdecls) = do
--   (vdecl, env') <- fromDecl env sdecl
--   (vdecls, env'') <- fromDecls env' sdecls
--   return (vdecl:vdecls, env'')

addCallableArgs :: FEnv s -> [(Name, PreMeta)] -> ST s ([(Name, VarMeta s)], FEnv s)
addCallableArgs env [] = return ([], env)
addCallableArgs env ((n, m):args) = do
  (m', env2) <- fromMeta env m
  (args', env3) <- addCallableArgs env2 args
  return ((n, m'):args', env3)

-- addCallables :: FEnv s -> [PDecl] -> ST s ([SemiPDecl s], FEnv s)
-- addCallables env [] = return ([], env)
-- addCallables env (Decl (DeclLHS m name args) e:decls) = do
--   (m', p, env1) <- fromMetaP env m
--   let env2 = fInsert env1 name (PntVal p)
--   (args', env3) <- addCallableArgs env2 args
--   let sdecl = SemiPDecl (DeclLHS m' name args') e
--   (sdecls, env4) <- addCallables env3 decls
--   return (sdecl:sdecls, env4)


fromPrgm :: FEnv s -> PPrgm -> ST s (VPrgm s, FEnv s)
fromPrgm = undefined
-- fromPrgm env decls = do
--   (sdecls, env') <- addCallables env decls
--   (vdecls, env'') <- fromDecls env' sdecls
--   return (vdecls, env'')

equalizeSchemes :: (Scheme, Scheme) -> Scheme
equalizeSchemes (_, SCheckError s) = SCheckError s
equalizeSchemes (SCheckError s, _) = SCheckError s
equalizeSchemes (SType lb1 ub1, SType lb2 ub2) = let lbBoth = unionRawTypes lb1 lb2
                                                     ubBoth = intersectRawTypes ub1 ub2
                                                  in if hasRawType lbBoth ubBoth
                                                        then SType lbBoth ubBoth
                                                        else SCheckError $ concat ["Type Mismatched: ", show lbBoth, " is not a subtype of ", show ubBoth]

executeConstraint :: Constraint s -> ST s [Constraint s]
executeConstraint (EqType pnt tp) = modifyDescriptor pnt (\oldScheme -> equalizeSchemes (oldScheme, SType tp tp)) >> return []
executeConstraint (EqPoints p1 p2) = union' p1 p2 (\s1 s2 -> return (equalizeSchemes (s1, s2))) >> return []
executeConstraint cons@(HasType subPnt parentPnt) = do
  subScheme <- descriptor subPnt
  parentScheme <- descriptor parentPnt
  case (subScheme, parentScheme) of
    (_, SCheckError _) -> return []
    (SCheckError _, _) -> return []
    (SType lb1 ub1, SType lb2 ub2) -> do
      setDescriptor subPnt (SType lb1 (intersectRawTypes ub1 lb2))
      setDescriptor parentPnt (SType (unionRawTypes lb2 ub1) ub2)
      return [cons | not (hasRawType ub1 lb2)]

merge2TypeCheckResults :: TypeCheckResult a -> TypeCheckResult b -> TypeCheckResult (a, b)
merge2TypeCheckResults a b = case (a, b) of
  (Right a', Right b') -> Right (a', b')
  (a', b')             -> Left $ fromLeft [] a' ++ fromLeft [] b'

mergeTypeCheckResults :: [TypeCheckResult r] -> TypeCheckResult [r]
mergeTypeCheckResults res = case partitionEithers res of
  ([], rs) -> Right rs
  (ls, _)  -> Left $ concat ls

fromRawType :: RawType -> Maybe Type
fromRawType RawTopType = Nothing
fromRawType RawBottomType = Nothing
fromRawType (RawLeafType s) = Just $ LeafType s
fromRawType (RawSumType [t]) = fromRawType t
fromRawType (RawSumType ts) = do
  ts' <- mapM fromRawType ts
  Just $ SumType ts'
fromRawType (RawProdType ts) = do
  ts' <- mapM fromRawType ts
  Just $ ProdType ts'


toMeta :: VarMeta s -> String -> ST s (TypeCheckResult Typed)
toMeta p name = do
  scheme <- descriptor p
  return $ case scheme of
    SCheckError s -> Left ["CheckError on " ++ name ++ ": " ++ s]
    SType lb ub -> case fromRawType ub of
      Nothing -> Left ["CheckError on " ++ show name ++ ": \ntLower Bound: " ++ show lb ++ "\n\tUpper Bound: " ++ show ub]
      Just t -> Right $ Typed t

toExpr :: VExpr s -> ST s (TypeCheckResult TExpr)
toExpr (CExpr m c) = do
  res <- toMeta m $ "Constant " ++ show c
  return $ res <&> (`CExpr` c)
toExpr (Var m name) = do
  res <- toMeta m $ "variable " ++ name
  return $ res >>= (\m' -> Right $ Var m' name)
-- toExpr (Call m name exprs) = do
--   res1 <- toMeta m $ "Function call " ++ name
--   res2 <- mapM toExpr exprs
--   return $ case (res1, mergeTypeCheckResults res2) of
--     (Right m', Right exprs') -> Right $ Call m' name exprs'
--     (a, b)                   -> Left $ fromLeft [] a ++ fromLeft [] b

toDeclLHS :: VDeclLHS s -> ST s (TypeCheckResult TDeclLHS)
toDeclLHS (DeclLHS m name [] ) = do
  res1 <- toMeta m $ "Declaration Return Type " ++ name
  return $ res1 >>= (\m' -> Right $ DeclLHS m' name [])
toDeclLHS (DeclLHS m name ((an, am):args) ) = do
  res1 <- toMeta am $ "Declaration Argument: " ++ name ++ "." ++ an
  res2 <- toDeclLHS (DeclLHS m name args)
  return $ merge2TypeCheckResults res1 res2 >>= (\(am', DeclLHS m' _ args') -> Right $ DeclLHS m' name ((an, am'):args'))


-- toDecl :: VDecl s -> ST s (TypeCheckResult TDecl)
-- toDecl (Decl lhs expr) = do
--   res1 <- toDeclLHS lhs
--   res2 <- toExpr expr
--   return $ merge2TypeCheckResults res1 res2 >>= (\(lhs', expr') -> Right $ Decl lhs' expr')

toPrgm :: VPrgm s -> ST s (TypeCheckResult TPrgm)
toPrgm = undefined
-- toPrgm decls = do
--   res <- mapM toDecl decls
--   return $ mergeTypeCheckResults res

abandonConstraints :: Constraint s -> ST s ()
abandonConstraints EqType{} = error "Bad Type equality"
abandonConstraints EqPoints{} = error "Bad point equality"
abandonConstraints (HasType subPnt parentPnt) = do
  subScheme <- descriptor subPnt
  parentScheme <- descriptor parentPnt
  case (subScheme, parentScheme) of
    -- (SKnown _, SUnknown) -> setDescriptor parentPnt $ SCheckError "Failed to unify hasType"
    (_, _) -> error "Uknown abandon constraint failure"

runConstraints :: [Constraint s] -> ST s ()
runConstraints [] = return ()
runConstraints cons = do
  res <- mapM executeConstraint cons
  let cons' = concat res
  if cons == cons'
    then mapM_ abandonConstraints cons
    else runConstraints cons'

typecheckPrgm :: PPrgm -> TypeCheckResult TPrgm
typecheckPrgm ppgrm = runST $ do
  baseFEnv <- makeBaseFEnv
  (vpgrm, FEnv cons _ errs) <- fromPrgm baseFEnv ppgrm
  case errs of
    [] -> do
      runConstraints cons
      toPrgm vpgrm
    _ -> return $ Left errs


testPrgm :: PPrgm
testPrgm = ([
               Global (PreTyped RawTopType) "add" (CExpr (PreTyped RawTopType) (CInt 1))
            ], [], [])
