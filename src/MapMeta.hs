--------------------------------------------------------------------
-- |
-- Module    :  MapMeta
-- Copyright :  (c) Zach Kimberg 2019
-- License   :  MIT
-- Maintainer:  zachary@kimberg.com
-- Stability :  experimental
-- Portability: non-portable
--
-- MapMeta is a utility to update the metadata across a program.
--------------------------------------------------------------------

{-# LANGUAGE FlexibleInstances #-}

module MapMeta where

import           Control.Monad
import           Control.Monad.Identity
import           Control.Monad.State        (State, evalState, get, put)
import           Control.Monad.Trans.Writer (execWriter, tell)
import qualified Data.ByteString            as B
import qualified Data.ByteString.UTF8       as BSU
import qualified Data.HashMap.Strict        as H
import qualified Data.List                  as L
import           Data.Maybe                 (fromMaybe)
import qualified Data.UUID                  as UUID
import qualified Data.UUID.V5               as UUID5
import           Data.Word                  (Word64, Word8)
import           Semantics.Prgm
import           Semantics.Types            (Constant (..))
import           Text.Printf

data MetaLocation
  = OutputMeta
  | InputMeta
  | AnnotMeta
  | ApplyMeta
  | GuardMeta
  | ImportMeta
  deriving (Eq, Ord, Show)

data MetaType
  = ExprMeta MetaLocation ExprMetaType
  | ArrMeta
  deriving (Eq, Ord, Show)

data ExprMetaType
  = ExprMetaConstant
  | ExprMetaVal
  | ExprMetaHole
  | ExprMetaMacroVal
  | ExprMetaApplyArg
  | ExprMetaApplyArgBase
  | ExprMetaTupleArg
  | ExprMetaApplyVar
  | ExprMetaApplyVarVal
  | ExprMetaWhere
  | ExprMetaTypeProp
  deriving (Eq, Ord, Show)

type MetaFun n a b = MetaType -> Meta a -> n (Meta b)

class MapMeta m where
  mapMetaM :: Monad n => MetaFun n a b -> MetaLocation -> m a -> n (m b)


clearMetaDat :: (Monad n) => MetaFun n a ()
clearMetaDat _ (Meta p l mid _) = return $ Meta p l mid ()

interleaveMeta :: (Monad n) => H.HashMap UUID.UUID a -> MetaFun n () (Maybe a)
interleaveMeta dat _ (Meta t p mid _) = return $ Meta t p mid (H.lookup mid dat)

interleavePrgm :: Prgm Expr m -> H.HashMap UUID.UUID (Meta m)
interleavePrgm prgm = H.fromList $ execWriter $ mapMetaPrgmM f prgm
  where
    f _ m = do
        tell [(getMetaId m, m)]
        return m

-- | The namespace every compiler-assigned meta id is generated in.
metaIDNamespace :: UUID.UUID
metaIDNamespace = UUID5.generateNamed UUID5.namespaceURL $ utf8Bytes "https://catln.dev/metaID"

-- | What desugar synthesizes an arrow for on behalf of a written statement: the continuation of a
-- bind, an arm of an @if@ / @match@ / @case@, or the fall through a guarded @case@ arm leaves to the
-- next one.
data DerivedMeta
  = DerivedBindCont
  | DerivedBranchArm
  | DerivedCaseFallthrough
  deriving (Eq, Ord, Show, Enum, Bounded)

-- | The id of a meta desugar synthesizes for a written statement, from the id of the statement it
-- stands for.
--
-- Derived rather than numbered in 'desugarMetaIDScope', so the arrow it names can be traced back to
-- the statement it was written as: a debugger showing a run by the page that was written finds the
-- statement a synthesized arrow belongs to by computing this for each statement. Derived rather than
-- copied, because an id names one meta ('interleavePrgm' keys metas by it, and an arrow's object id
-- is the id of the test it declares) and the statement's own meta is still in the program.
derivedMetaID :: DerivedMeta -> UUID.UUID -> UUID.UUID
derivedMetaID kind origin = UUID5.generateNamed metaIDNamespace $ utf8Bytes $ printf "derived:%s:%s" (show kind) (UUID.toString origin)

utf8Bytes :: String -> [Word8]
utf8Bytes = B.unpack . BSU.fromString

-- | Where a meta id comes from: the name of the pass handing ids out, and how many it has handed
-- out so far.
--
-- Ids are derived from the scope rather than drawn at random so that reading a file which has not
-- changed gives back a program /equal/ to the one read before it. 'Meta' compares by id, so a
-- random id per read made every re-read of every page look like an edit, and rebuilt the whole
-- program (see the @oldRaw == rawPrgm@ guard in 'CtService.ctssRead').
--
-- Two scopes must never share a name. Ids are compared across every page of a program
-- ('CtService.pageOwners' keys the page which wrote each test by one), so a shared name would hand
-- two pages the same ids and attribute one page's tests to the other.
data MetaIDScope = MetaIDScope !String !Word64

-- | The directories a page is named from in its meta-id scopes ('pageIDName'), each under a name of
-- its own: the standard library's, and the one the compiler runs in.
--
-- A page's ids are the same wherever the files sit. An id named from the page's absolute path made
-- every id of a checkout differ from another checkout's, so a golden holding one (an annotation's
-- value naming the expression it came from) held the path it was written under, though it is
-- written with the path itself spelled as the fixed @/repo/dir@.
newtype MetaIDRoots = MetaIDRoots [(String, FilePath)]

-- | No directory to name pages from: for pages which name no file ('Syntax.Ct.Prgm.mkRawFileImport'
-- of a bare name), whose import holds no path to be named.
noMetaIDRoots :: MetaIDRoots
noMetaIDRoots = MetaIDRoots []

-- | What a page's meta-id scopes are named by: its absolute import expression, which is what
-- 'AFileImport' compares by, with each path in it spelled from the root it sits under — the root's
-- name, then the path within it — the longest root first. A path under no root stays as it is.
--
-- Two distinct files cannot share a name, as no two can share an absolute import: every 'impAbs' is
-- rebuilt by 'Syntax.Parsers.canonicalImport' from an absolute path with position-less metas (an
-- import form which kept its parsed positions would break that, giving two files written alike the
-- same ids), and a path is spelled from one root, the same one, wherever the file was named from.
pageIDName :: MetaIDRoots -> FileImport -> String
pageIDName (MetaIDRoots roots) imp = show $ mapExprStrings fromRoot $ impAbs imp
  where
    longestFirst = L.sortOn (negate . length . snd) roots
    fromRoot s = case [ name ++ ":" ++ rest | (name, root) <- longestFirst, Just rest <- [within root s] ] of
      (named:_) -> named
      []        -> s
    within root s
      | s == root = Just ""
      | (root ++ "/") `L.isPrefixOf` s = Just (drop (length root + 1) s)
      | otherwise = Nothing

-- | An expression with each string constant in it rewritten.
mapExprStrings :: (String -> String) -> Expr m -> Expr m
mapExprStrings f = expr
  where
    expr (CExpr m (CStr s))       = CExpr m (CStr (f s))
    expr e@CExpr{}                = e
    expr e@Value{}                = e
    expr e@HoleExpr{}             = e
    expr (AliasExpr b a)          = AliasExpr (expr b) (expr a)
    expr (EWhere m b c)           = EWhere m (expr b) (expr c)
    expr (TupleApply m (bm, b) a) = TupleApply m (bm, expr b) (app a)
    app (EAppArg oa)   = EAppArg (objArr oa)
    app a@EAppVar{}    = a
    app (EAppSpread e) = EAppSpread (expr e)
    objArr oa@ObjArr{oaObj, oaArr} = oa{oaObj = fmap expr oaObj, oaArr = fmap (\(e, m) -> (fmap expr e, m)) oaArr}

-- | The scope a file's parse numbers its metas in, named by 'pageIDName'.
parseMetaIDScope :: MetaIDRoots -> FileImport -> MetaIDScope
parseMetaIDScope roots imp = MetaIDScope ("parse:" ++ pageIDName roots imp) 0

-- | The scope a file's desugar numbers its metas in: the ones desugar synthesizes, as those
-- carried over from the parse already hold an id. Distinct from 'parseMetaIDScope' so the two
-- passes over one file cannot hand out the same id twice.
desugarMetaIDScope :: MetaIDRoots -> FileImport -> MetaIDScope
desugarMetaIDScope roots imp = MetaIDScope ("des:" ++ pageIDName roots imp) 0

-- | Gives an id to a meta which has none, leaving one which already has an id alone. Both the
-- parse and the desugar pass run over metas the other has already numbered.
addMetaID :: MetaFun (State MetaIDScope) m m
addMetaID _ m | not (UUID.null $ getMetaId m) = return m
addMetaID _ m = do
  MetaIDScope scope n <- get
  put $ MetaIDScope scope (n + 1)
  return m{getMetaId=UUID5.generateNamed metaIDNamespace $ utf8Bytes $ printf "%s#%d" scope n}

zipMetaFun :: (Monad n) => MetaFun n a b -> MetaFun n a c -> MetaFun n a (b, c)
zipMetaFun f1 f2 tp m@(Meta t p mid _) = do
  (Meta _ _ _ db) <- f1 tp m
  (Meta _ _ _ dc) <- f2 tp m
  return $ Meta t p mid (db, dc)

zip3MetaFun :: (Monad n) => MetaFun n a b -> MetaFun n a c -> MetaFun n a d -> MetaFun n a (b, c, d)
zip3MetaFun f1 f2 f3 tp m@(Meta t p mid _) = do
  (Meta _ _ _ db) <- f1 tp m
  (Meta _ _ _ dc) <- f2 tp m
  (Meta _ _ _ dd) <- f3 tp m
  return $ Meta t p mid (db, dc, dd)

zip4MetaFun :: (Monad n) => MetaFun n a b -> MetaFun n a c -> MetaFun n a d -> MetaFun n a e -> MetaFun n a (b, c, d, e)
zip4MetaFun f1 f2 f3 f4 tp m@(Meta t p mid _) = do
  (Meta _ _ _ db) <- f1 tp m
  (Meta _ _ _ dc) <- f2 tp m
  (Meta _ _ _ dd) <- f3 tp m
  (Meta _ _ _ de) <- f4 tp m
  return $ Meta t p mid (db, dc, dd, de)

mapMetaAppliedExprM :: (Monad n, MetaDat m, Show m) => MetaFun n m m -> MetaLocation -> Expr m -> n (Expr m)
mapMetaAppliedExprM f loc (CExpr m c) = do
  m' <- f (ExprMeta loc ExprMetaConstant) m
  return $ CExpr m' c
mapMetaAppliedExprM f loc (Value m n) = do
  m' <- f (ExprMeta loc ExprMetaVal) m
  return $ Value m' n
mapMetaAppliedExprM f loc (HoleExpr m h) = do
  m' <- f (ExprMeta loc ExprMetaHole) m
  return $ HoleExpr m' h
mapMetaAppliedExprM f loc (AliasExpr b a) = do
  b' <- mapMetaAppliedExprM f loc b
  a' <- mapMetaAppliedExprM f loc a
  return $ AliasExpr b' a'
mapMetaAppliedExprM f loc (EWhere m b a) = do
  m' <- f (ExprMeta loc ExprMetaWhere) m
  b' <- mapMetaAppliedExprM f loc b
  a' <- mapMetaAppliedExprM f loc a
  return $ EWhere m' b' a'
mapMetaAppliedExprM f loc (TupleApply m (bm, be) arg) = do
  m' <- f (ExprMeta loc ExprMetaApplyArg) m
  bm' <- f (ExprMeta loc ExprMetaApplyArgBase) bm
  be' <- mapMetaAppliedExprM f loc be
  arg' <- mapArg arg
  return $ TupleApply m' (bm', be') arg'
    where
      mapArg (EAppArg a)    = do
        obj' <- case oaObj a of
          Just o -> do
            o' <- mapMetaM f loc o
            return $ Just o'
          Nothing -> return Nothing
        let a' = a{oaObj=obj'}
        return $ EAppArg a'
      mapArg (EAppVar vn vm)    = do
        vm' <- f (ExprMeta loc ExprMetaApplyVarVal) vm
        return $ EAppVar vn vm'
      mapArg (EAppSpread a) = do
        a' <- mapMetaAppliedExprM f loc a
        return $ EAppSpread a'

instance MapMeta Expr where
  mapMetaM f loc (CExpr m c) = do
    m' <- f (ExprMeta loc ExprMetaConstant) m
    return $ CExpr m' c
  mapMetaM f loc (Value m n) = do
    m' <- f (ExprMeta loc ExprMetaVal) m
    return $ Value m' n
  mapMetaM f loc (HoleExpr m h) = do
    m' <- f (ExprMeta loc ExprMetaHole) m
    return $ HoleExpr m' h
  mapMetaM f loc (AliasExpr b a) = do
    b' <- mapMetaM f loc b
    a' <- mapMetaM f loc a
    return $ AliasExpr b' a'
  mapMetaM f loc (EWhere m b a) = do
    m' <- f (ExprMeta loc ExprMetaWhere) m
    b' <- mapMetaM f loc b
    a' <- mapMetaM f loc a
    return $ EWhere m' b' a'
  mapMetaM f loc (TupleApply m (bm, be) arg) = do
    m' <- f (ExprMeta loc ExprMetaApplyArg) m
    bm' <- f (ExprMeta loc ExprMetaApplyArgBase) bm
    be' <- mapMetaM f loc be
    arg' <- mapArg f arg
    return $ TupleApply m' (bm', be') arg'
    where
      mapArg :: (Monad n, MapMeta e) => MetaFun n a b -> EApp e a -> n (EApp e b)
      mapArg f' (EAppArg a)    = do
        a' <- mapMetaObjArrM f' (Just loc) a
        return $ EAppArg a'
      mapArg f' (EAppVar vn vm)    = do
        vm' <- f' (ExprMeta loc ExprMetaApplyVarVal) vm
        return $ EAppVar vn vm'
      mapArg f' (EAppSpread a) = do
        a' <- mapMetaM f' loc a
        return $ EAppSpread a'

mapMetaObjArrM :: (Monad n, MapMeta e) => MetaFun n a b -> Maybe MetaLocation -> ObjArr e a -> n (ObjArr e b)
mapMetaObjArrM f mloc oa@ObjArr{oaObj, oaAnnots, oaArr} = do
  oaObj' <- mapM (mapMetaM f (fromMaybe InputMeta mloc)) oaObj
  oaAnnots' <- mapM (mapMetaM f (fromMaybe AnnotMeta mloc)) oaAnnots
  oaArr' <- forM  oaArr $ \(arrE, arrM) -> do
    arrE' <- mapM (mapMetaM f (fromMaybe OutputMeta mloc)) arrE
    arrM' <- f ArrMeta arrM
    return (arrE', arrM')
  return oa{oaObj=oaObj', oaAnnots=oaAnnots', oaArr=oaArr'}

mapMetaObjectMapM :: (Monad n, MapMeta e) => MetaFun n a b -> ObjectMap e a -> n (ObjectMap e b)
mapMetaObjectMapM f = mapMObjectMap (mapMetaObjArrM f Nothing)

mapMetaPrgmM :: (Monad n, MapMeta e) => MetaFun n a b -> Prgm e a -> n (Prgm e b)
mapMetaPrgmM f (Prgm objMap classGraph annots) = do
  objMap' <- mapMetaObjectMapM f objMap
  annots' <- mapM (mapMetaM f AnnotMeta) annots
  return $ Prgm objMap' classGraph annots'

-- | Numbers every id-less meta of a program within one scope.
addPrgmMetaIDs :: (MapMeta e) => MetaIDScope -> Prgm e m -> Prgm e m
addPrgmMetaIDs scope prgm = evalState (mapMetaPrgmM addMetaID prgm) scope

mapMeta :: (MapMeta m) => MetaFun Identity a b -> MetaLocation -> m a -> m b
mapMeta f loc a = runIdentity $ mapMetaM f loc a

mapMetaAppliedExpr :: (MetaDat m, Show m) => MetaFun Identity m m -> MetaLocation -> Expr m -> Expr m
mapMetaAppliedExpr f loc e = runIdentity $ mapMetaAppliedExprM f loc e

mapMetaObjArr :: (MapMeta e) => MetaFun Identity a b -> Maybe MetaLocation -> ObjArr e a -> ObjArr e b
mapMetaObjArr f loc oa = runIdentity $ mapMetaObjArrM f loc oa

mapMetaObjectMap :: (MapMeta e) => MetaFun Identity a b -> ObjectMap e a -> ObjectMap e b
mapMetaObjectMap f om = runIdentity $ mapMetaObjectMapM f om

mapMetaPrgm :: (MapMeta e) => MetaFun Identity a b -> Prgm e a -> Prgm e b
mapMetaPrgm f p = runIdentity $ mapMetaPrgmM f p
