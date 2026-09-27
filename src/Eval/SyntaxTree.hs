--------------------------------------------------------------------
-- |
-- Module    :  Eval.SyntaxTree
-- Copyright :  (c) Zach Kimberg 2026
-- License   :  MIT
-- Maintainer:  zachary@kimberg.com
-- Stability :  experimental
-- Portability: non-portable
--
-- This module reads a debugger run by the functions its pages write
-- rather than by the arrows they were compiled to ('SyntaxRun').
--------------------------------------------------------------------

module Eval.SyntaxTree (withRunSyntax, runSyntax) where

import           Control.Applicative              ((<|>))
import           Control.Monad.State
import           CtConstants
import qualified Data.HashMap.Strict              as H
import qualified Data.HashSet                     as S
import           Data.List                        (foldl')
import           Data.Maybe                       (listToMaybe, mapMaybe)
import           Data.UUID                        (UUID)
import           Eval.Common
import           MapMeta                          (DerivedMeta (..),
                                                   derivedMetaID)
import           Semantics.Prgm
import           Semantics.Types
import           Syntax.Ct.Desugarf.Preprocessors (bindContArgName,
                                                   branchDeclNames)
import           Syntax.Ct.Prgm

-- | What a statement is within the function it is written in.
data StmtKind
  -- | A function: every statement written at the top of a page, and a nested one taking parameters
  -- of its own. Each call of one is a 'SyntaxCall'.
  = StmtFunction
  -- | A line of the function it is written in: a nested declaration taking no parameters.
  | StmtLine
  -- | An arm of the @if@, @match@ or @case@ the statement above it is written as.
  | StmtArm
  | StmtBind
  | StmtAnnot
  deriving (Eq, Show)

data StmtInfo = StmtInfo { siId     :: UUID
                         , siFile   :: String
                         , siTree   :: RawStatementTree RawExpr ()
                         -- | The function whose calls this statement is evaluated in: the nearest
                         -- 'StmtFunction' above it, or itself for a function.
                         , siOwner  :: Maybe UUID
                         -- | The statement it is written under.
                         , siParent :: Maybe UUID
                         , siKind   :: StmtKind
                         }

-- | What an arrow of the run was compiled from.
data ArrowOf
  = ArrowFunction StmtInfo
  | ArrowLine StmtInfo
  | ArrowArm StmtInfo
  -- | The continuation of a bind: the rest of the statements after it, given the bound value.
  | ArrowBindCont StmtInfo
  -- | The fall through a @case@ arm leaves to the next when it does not apply. It stands for no
  -- line of its own.
  | ArrowFallthrough StmtInfo

data Index = Index { ixStmts  :: H.HashMap UUID StmtInfo
                   , ixArrows :: H.HashMap UUID ArrowOf
                   }

-- | The id a statement is named by: that of its object's meta, or the annotation's
-- ('Syntax.Ct.MapRawMeta.rawStatementExprIds').
statementId :: RawStatement RawExpr () -> Maybe UUID
statementId (RawDeclStatement RawObjArr{roaObj=Just o}) = Just $ getMetaId $ getExprMeta o
statementId (RawBindStatement RawObjArr{roaObj=Just o}) = Just $ getMetaId $ getExprMeta o
statementId (RawAnnot a)                                = Just $ getMetaId $ getExprMeta a
statementId _                                           = Nothing

-- | The name of the body a declaration is written as, if it has one.
bodyName :: RawStatement RawExpr () -> Maybe String
bodyName (RawDeclStatement RawObjArr{roaArr=Just (Just body, _)}) = nameStr <$> maybeExprPath body
bodyName _ = Nothing

-- | Whether a declaration's object gives it parameters of its own. A line is written as a bare name
-- (@sum= a + b@), and a guard on an arm's name does not make it a function either. Read only of a
-- declaration with an arrow: the result a function is written over several lines to reach is a bare
-- expression, which applies arguments without declaring anything.
takesParams :: RawExpr () -> Bool
takesParams RawValue{}         = False
takesParams (RawWhere _ b _)   = takesParams b
takesParams (RawParen e)       = takesParams e
takesParams (RawAliasExpr b _) = takesParams b
takesParams _                  = True

indexPrgms :: [(String, RawPrgm ())] -> Index
indexPrgms prgms = Index (H.fromList [(siId si, si) | si <- stmts]) (H.fromList $ concatMap arrowKeys stmts)
  where
    stmts = concat [concatMap (indexTree file Nothing Nothing False) trees | (file, RawPrgm _ trees) <- prgms]

    indexTree file owner parent inBranch tree@(RawStatementTree s children) = case statementId s of
      Nothing -> []
      Just sid -> si : concatMap (indexTree file owner' (Just sid) (bodyName s `elem` map Just branchDeclNames)) children
        where
          kind = case s of
            RawAnnot{}                                         -> StmtAnnot
            RawBindStatement{}                                 -> StmtBind
            _ | inBranch                                       -> StmtArm
            _ | Nothing <- owner                               -> StmtFunction
            RawDeclStatement RawObjArr{roaObj=Just o, roaArr=Just _} | takesParams o -> StmtFunction
            _                                                  -> StmtLine
          owner' = if kind == StmtFunction then Just sid else owner
          si = StmtInfo sid file tree owner' parent kind

    arrowKeys si@StmtInfo{siId, siTree=RawStatementTree s _, siKind} = case siKind of
      StmtFunction -> pathKey ArrowFunction
      StmtLine -> pathKey ArrowLine
      StmtArm -> [(derivedMetaID DerivedBranchArm siId, ArrowArm si), (derivedMetaID DerivedCaseFallthrough siId, ArrowFallthrough si)]
      StmtBind -> [(derivedMetaID DerivedBindCont siId, ArrowBindCont si)]
      StmtAnnot -> []
      where
        pathKey f = case s of
          RawDeclStatement RawObjArr{roaObj=Just o} | Just (_, m) <- maybeExprPathM o -> [(getMetaId m, f si)]
          _ -> []

-- | The key an arrow of the program is found in the 'Index' by: the id of its object's name. A
-- function declared with a context (@main{io -> IO}@) is compiled to a @ContextIn@ whose @value@
-- is the function, as 'Eval.evalAllTargetModes' reads it.
arrowKey :: AnyObjArr -> Maybe UUID
arrowKey (Right _) = Nothing
arrowKey (Left oa) = do
  obj <- oaObj oa
  (n, m) <- maybeExprPathM obj
  if nameStr n == ContextInStr
    then case H.lookupDefault Nothing (partialKey (mkName contextValStr)) (exprAppliedArgsMap obj) of
      Just (_, Just valE) -> getMetaId . snd <$> maybeExprPathM valE
      _                   -> Nothing
    else Just (getMetaId m)

-- | The statement a segment's result is written as: the last statement under a declaration written
-- over several lines, or the declaration itself when written on one.
resultStmt :: StmtInfo -> Maybe UUID
resultStmt StmtInfo{siId, siTree=RawStatementTree s children} = case bodyName s of
  Just n | n == nestedDeclaration -> case reverse children of
    (RawStatementTree last'@(RawDeclStatement RawObjArr{roaArr=Nothing}) [] : _) -> statementId last'
    _ -> Nothing
  _ -> Just siId

-- | Whether a statement has a bind written directly under it. The statements after a bind are the
-- bind's continuation, so the result of such a statement is reached in the continuation of its last
-- bind rather than in the statement's own arrow.
hasBind :: StmtInfo -> Bool
hasBind StmtInfo{siTree=RawStatementTree _ children} = any (isBind . rstStatement) children
  where
    isBind RawBindStatement{} = True
    isBind _                  = False

lastBind :: StmtInfo -> Maybe UUID
lastBind StmtInfo{siTree=RawStatementTree _ children} = listToMaybe $ reverse $ mapMaybe bindId children
  where
    bindId (RawStatementTree s@RawBindStatement{} _) = statementId s
    bindId _                                         = Nothing

-- | A call of a function being gathered: its statement, its runs by statement, most recent first,
-- and the calls of functions made in it, most recent first.
data CallAcc = CallAcc { caStmt  :: UUID
                       , caRuns  :: [(UUID, LineRun)]
                       , caCalls :: [String]
                       }

data Acc = Acc { acCalls     :: H.HashMap String CallAcc
               , acRoots     :: [String]
               -- | The call each step of a line's rewriting was made for: the call the line is a line
               -- of, rather than whichever call the chain it was made in happened to be inside.
               , acRewriting :: H.HashMap String String
               }

-- | A run read by the functions the given pages write ('SyntaxRun').
runSyntax :: [(String, RawPrgm ())] -> DebugRun -> SyntaxRun
runSyntax prgms DebugRun{drResult=EvalResult{erTreebug, erExprs, erAnnots}} = SyntaxRun calls statements
  where
    ix@Index{ixStmts} = indexPrgms prgms

    -- The expression each node was a step of rewriting, and so what the call it made came to.
    exprOf = H.fromList [(step, e) | e@EvalExprRun{exSteps} <- erExprs, step <- exSteps]
    resolved nid = exValue <$> H.lookup nid exprOf
    -- The call an expression was written as, for a node that only continued its rewriting: a
    -- function returns a call of one of its own arms, which the caller then makes.
    chainHead nid = H.lookup nid exprOf >>= listToMaybe . exSteps
    -- The calls that went on rewriting what a line's arrow returned. A line is an arrow whose result is
    -- the term its body rewrites to, and what that term is rewritten through is made by whoever used
    -- the line, as the next steps of the expression that used it: @shifted= fmap(…)@ returns the fmap
    -- unapplied, and the fmap's calls follow the line's in the chain. They are the rest of how the line
    -- came to its value, up to the next step that is a line of its own, which has its own run.
    continuation nid = case H.lookup nid exprOf of
      Just EvalExprRun{exSteps} -> takeWhile (not . isLineArrow) $ drop 1 $ dropWhile (/= nid) exSteps
      Nothing                   -> []
    isLineArrow step = case H.lookup step nodesById >>= arrowKeyOfNode >>= (`H.lookup` ixArrows ix) of
      Just ArrowLine{}        -> True
      Just ArrowArm{}         -> True
      Just ArrowBindCont{}    -> True
      Just ArrowFallthrough{} -> True
      _                       -> False
    arrowKeyOfNode (EvalTreebugClosed oa _ _ _ _ _) = arrowKey oa
    nodesById = H.fromList $ concatMap nodeEntries erTreebug
    nodeEntries n@(EvalTreebugClosed _ _ _ children nid _) = (nid, n) : concatMap nodeEntries children
    annotsOf = H.fromListWith (flip (++)) [(eaNode a, [a]) | a <- erAnnots]

    Acc{acCalls=finalCalls, acRoots=finalRoots} = execState (mapM_ (visit ix []) erTreebug) (Acc H.empty [] H.empty)

    calls = map buildCall (reverse finalRoots)
    buildCall nid = SyntaxCall caStmt nid (groupRuns $ reverse caRuns) (map buildCall $ reverse caCalls)
      where CallAcc{caStmt, caRuns, caCalls} = finalCalls H.! nid
    groupRuns runs = [SyntaxLine sid [r | (sid', r) <- runs, sid' == sid] | sid <- ordNub (map fst runs)]

    statements = [SyntaxStatement ssId siFile siTree (ownedLines ssId siTree) | ssId <- ordNub (map caStmt $ H.elems finalCalls), Just StmtInfo{siFile, siTree} <- [H.lookup ssId ixStmts]]
    -- In the order they are written, which is the order a reader of the function reads them in.
    ownedLines fun (RawStatementTree _ children) = concatMap ownedIn children
      where
        ownedIn (RawStatementTree s cs) = [sid | Just sid <- [statementId s], Just StmtInfo{siOwner=Just owner} <- [H.lookup sid ixStmts], owner == fun, sid /= fun] ++ concatMap ownedIn cs

    visit :: Index -> [(String, UUID)] -> EvalTreebugClosed -> State Acc ()
    visit Index{ixArrows} stack (EvalTreebugClosed oa input _ children nid _) =
      case arrowKey oa >>= (`H.lookup` ixArrows) of
        Just (ArrowFunction si@StmtInfo{siId}) -> do
          -- A call made rewriting a line is a call of the function the line is in, however far out
          -- the chain that made it runs.
          rewritingFor <- gets (H.lookup nid . acRewriting)
          addCall (rewritingFor <|> fst <$> listToMaybe stack) nid siId
          segmentRuns nid si nid Nothing body
          annotRuns nid annots
          continue ((nid, siId) : stack)
        Just (ArrowLine si) -> owned stack si $ \c -> segmentRuns c si nid (Just nid) body >> annotRuns c annots
        Just (ArrowArm si) -> owned stack si $ \c -> segmentRuns c si nid (Just nid) body >> annotRuns c annots
        Just (ArrowBindCont si@StmtInfo{siId, siParent}) -> owned stack si $ \c -> do
          addRun c siId (LineRun (boundValue oa input) [] (Just nid) Nothing)
          -- The rest of the statement the bind is written under is this continuation, so the
          -- statement's result is reached here when no later bind continues it further.
          case siParent >>= (`H.lookup` ixStmts) of
            Just parent | lastBind parent == Just siId, Just r <- resultStmt parent -> addRun c r (LineRun (resolved nid) body Nothing (Just nid))
            _ -> return ()
          annotRuns c annots
        Just (ArrowFallthrough si) -> owned stack si $ \c -> annotRuns c annots
        Nothing -> continue stack
      where
        annots = H.lookupDefault [] nid annotsOf
        annotCalls = S.fromList $ concatMap eaCalls annots
        -- The calls made evaluating the node's own body, rather than its annotations.
        body = filter (not . (`S.member` annotCalls)) [cid | EvalTreebugClosed _ _ _ _ cid _ <- children]
        continue stack' = mapM_ (visit ix stack') children

        -- A statement is run in the call of the function it is written in: the nearest one open,
        -- or, for an arm or continuation made by the caller of a call that returned it, that call.
        -- One in neither is not a line of any call the run shows, and only its calls are read.
        owned stack' StmtInfo{siOwner} f = do
          Acc{acCalls} <- get
          let open = listToMaybe [c | (c, s) <- stack', Just s == siOwner]
              returned = do
                h <- chainHead nid
                CallAcc{caStmt} <- H.lookup h acCalls
                if Just caStmt == siOwner then Just h else Nothing
          forM_ (open <|> returned) f
          continue stack'

    -- The runs of a statement evaluated by one node: what it came to, and, where its result is written
    -- as a statement of its own, what that came to. The node's own calls break down whichever of the
    -- two is its result. A statement with a bind has its result in the bind's continuation, and its
    -- own calls are the bind's: the monad bound over, and the continuation run through it.
    segmentRuns :: String -> StmtInfo -> String -> Maybe String -> [String] -> State Acc ()
    segmentRuns c si@StmtInfo{siId} nid lineNode body = do
      let result = if hasBind si then Nothing else resultStmt si
      -- A line's own arrow is followed by the rest of its rewriting; a function's call is not, as what
      -- follows a call is its caller's.
      let rewriting = maybe [] (const $ continuation nid) lineNode
      modify $ \acc@Acc{acRewriting} -> acc{acRewriting = foldr (`H.insert` c) acRewriting rewriting}
      addRun c siId (LineRun (resolved nid) ((if result == Just siId || hasBind si then body else []) ++ rewriting) lineNode (Just nid))
      case result of
        Just r | r /= siId -> addRun c r (LineRun (resolved nid) body Nothing (Just nid))
        _                  -> return ()

    annotRuns c annots = forM_ annots $ \EvalAnnotRun{eaNode, eaAnnot, eaValue, eaCalls} -> addRun c eaAnnot (LineRun (Just eaValue) eaCalls Nothing (Just eaNode))

    addRun :: String -> UUID -> LineRun -> State Acc ()
    addRun c sid r = modify $ \acc@Acc{acCalls} -> acc{acCalls = H.adjust (\ca@CallAcc{caRuns} -> ca{caRuns = (sid, r) : caRuns}) c acCalls}

    addCall :: Maybe String -> String -> UUID -> State Acc ()
    addCall parent nid sid = modify $ \acc@Acc{acCalls, acRoots} -> case parent of
      Just p -> acc{acCalls = H.insert nid (CallAcc sid [] []) $ H.adjust (\ca@CallAcc{caCalls} -> ca{caCalls = nid : caCalls}) p acCalls}
      Nothing -> acc{acCalls = H.insert nid (CallAcc sid [] []) acCalls, acRoots = nid : acRoots}

-- | The value a bind bound, which its continuation is given as its own argument
-- ('bindContArgName').
boundValue :: AnyObjArr -> Val -> Maybe Val
boundValue (Left oa) (TupleVal _ args) | Just obj <- oaObj oa, Just n <- maybeExprPath obj =
  H.lookup ('/' : bindContArgName (contName $ nameStr n)) args
  where contName = reverse . takeWhile (/= '.') . reverse
boundValue _ _ = Nothing

ordNub :: [UUID] -> [UUID]
ordNub = reverse . snd . foldl' (\(seen, out) x -> if S.member x seen then (seen, out) else (S.insert x seen, x : out)) (S.empty, [])

-- | A run with its reading by the functions written in the pages it was run in ('runSyntax').
withRunSyntax :: [(String, RawPrgm ())] -> DebugRun -> DebugRun
withRunSyntax prgms run = run{drSyntax = runSyntax prgms run}
