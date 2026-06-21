--------------------------------------------------------------------
-- |
-- Module    :  TypeCheck.TraceWrite
-- Copyright :  (c) Zach Kimberg 2019
-- License   :  MIT
-- Maintainer:  zachary@kimberg.com
-- Stability :  experimental
-- Portability: non-portable
--
-- This module renders the type-inference 'TraceConstrain' into a set
-- of plain text files (one per point, plus a program overview). It is
-- the text-file equivalent of the webdocs TypeInfer component, used to
-- inspect how each inference point evolves across solver epochs along
-- with its neighboring points.
--------------------------------------------------------------------

module TypeCheck.TraceWrite
  ( renderTypeTrace
  ) where

import           Control.Monad.Identity (runIdentity)
import           Control.Monad.Writer   (Writer, execWriter, tell)
import qualified Data.HashMap.Strict    as H
import           Data.List              (intercalate, nub, sort)
import           Data.Maybe             (mapMaybe)
import           MapMeta                (MetaType, mapMetaObjArrM)
import           Semantics.Prgm
import           Semantics.Types        (Type)
import           Text.Printf            (printf)
import           TypeCheck.Common

-- | A 'VarMeta' rendered by its point number rather than its type. Used
-- to display a constraint's structure in terms of the points it relates.
newtype PntRef = PntRef (Maybe Pnt)

instance Show PntRef where
  show (PntRef (Just p)) = printf "p%d" p
  show (PntRef Nothing)  = "p?"

-- | Render a constraint showing each meta as its point (e.g. @p5 -> p7@).
showConPnts :: VConstraint -> String
showConPnts (Constraint _ _ d) = show $ runIdentity $ mapMConDat (return . PntRef . getPnt) d

-- | The points referenced by a constraint.
conPnts :: VConstraint -> [Pnt]
conPnts = mapMaybe getPnt . constraintMetas

-- | Collect every (point, position, type) appearing in a list of objArrs.
collectMetas :: [VObjArr] -> [(Pnt, CodeRange, Type)]
collectMetas oas = execWriter $ mapM_ (mapMetaObjArrM collect Nothing) oas
  where
    collect :: MetaType -> VarMeta -> Writer [(Pnt, CodeRange, Type)] VarMeta
    collect _ m@Meta{getMetaType, getMetaPos, getMetaDat=VarMetaDat mp _} = do
      case mp of
        Just p  -> tell [(p, getMetaPos, getMetaType)]
        Nothing -> return ()
      return m

-- | All points referenced anywhere in the trace (initial, epoch updates,
-- and constraints).
allTracePnts :: TraceConstrain -> [Pnt]
allTracePnts TraceConstrain{tcEpochs, tcInitial, tcCons} = nub $ sort $
     H.keys tcInitial
  ++ [p | epoch <- tcEpochs, (_, pnts) <- epoch, (p, _) <- pnts]
  ++ concatMap conPnts tcCons

-- | The final scheme of a point: its last update across all epochs, falling
-- back to its initial scheme. Assumes epochs are in chronological order.
finalScheme :: TraceConstrain -> Pnt -> Maybe Scheme
finalScheme TraceConstrain{tcEpochs, tcInitial} p =
  case reverse [s | epoch <- tcEpochs, (_, pnts) <- epoch, (p', s) <- pnts, p' == p] of
    (s:_) -> Just s
    []    -> H.lookup p tcInitial

showPos :: CodeRange -> String
showPos = maybe "?" showCodeRange

-- | Render the whole trace into a list of (relative path, contents) files.
-- Produces a @program.txt@ overview, a @points-index.txt@ table, and a
-- @pnt/<N>.txt@ file for every point in the trace.
renderTypeTrace :: VPrgm -> TraceConstrain -> [(FilePath, String)]
renderTypeTrace (objMap, _, _) tc =
  ("program.txt", programOverview)
  : ("points-index.txt", pointsIndex)
  : [(printf "pnt/%d.txt" p, renderPnt p) | p <- pnts]
  where
    objArrs = flatObjectMap objMap
    pnts = allTracePnts tc
    posMap = H.fromList [(p, (pos, t)) | (p, pos, t) <- collectMetas objArrs]

    finalStr p = maybe "(no value)" show (finalScheme tc p)

    -- program.txt: each definition printed structurally, with every point and
    -- its final converged type shown inline, then a compact point->position footer.
    programOverview = intercalate "\n\n" $ map renderObjArr objArrs
    renderObjArr oa = intercalate "\n" $
      showObjArrWithConfig traceCfg oa
      : [printf "    p%d @ %s" p (showPos pos) | (p, pos, _) <- sortMetas (collectMetas [oa])]
    sortMetas = nub . sort

    -- The final converged ACT type of a meta's point, if it has one.
    finalActType :: VarMeta -> Maybe Type
    finalActType m = do
      p <- getPnt m
      s <- finalScheme tc p
      tcreToMaybe (fmap stypeAct s)

    -- Render a meta as "pN:<final type>", falling back to the static type when
    -- the point never converged (and to just the type when there is no point).
    showFinalMeta :: VarMeta -> String
    showFinalMeta m = case getPnt m of
      Nothing -> show m
      Just p  -> printf "p%d:%s" p (maybe (show m) show (finalActType m))

    traceCfg :: ShowConfig VarMetaDat
    traceCfg = ShowConfig
      { scShowMeta = showFinalMeta
      , scAnnot    = \m -> case getPnt m of
          Nothing -> ""
          Just _  -> printf "‹%s›" (showFinalMeta m)
      }

    -- points-index.txt: global point -> position / final type table
    pointsIndex = intercalate "\n"
      [ printf "p%d @ %s :: %s" p (showPos (maybe Nothing fst (H.lookup p posMap))) (finalStr p)
      | p <- pnts ]

    -- pnt/<N>.txt: initial, per-epoch updates, constraints, and neighbors
    renderPnt p = intercalate "\n" $ concat
      [ [ printf "pnt %d" p
        , printf "position: %s" (showPos (maybe Nothing fst (H.lookup p posMap)))
        , printf "final:    %s" (finalStr p)
        , "" ]
      , initialSection
      , epochSections
      , constraintSection
      , neighborSection ]
      where
        initialSection = case H.lookup p (tcInitial tc) of
          Just s  -> ["Initial:", printf "  %s" (show s), ""]
          Nothing -> []

        epochSections = concat
          [ printf "Epoch %d:" i
            : [printf "  %s  =>  %s" (showConPnts con) (show s) | (con, s) <- updates]
            ++ [""]
          | (i, epoch) <- zip [(0 :: Int)..] (tcEpochs tc)
          , let updates = [(con, s) | (con, pnts') <- epoch, (p', s) <- pnts', p' == p]
          , not (null updates) ]

        consTouching = filter (elem p . conPnts) (tcCons tc)
        constraintSection =
          "Constraints touching this point:"
          : [printf "  %s" (showConPnts con) | con <- consTouching]
          ++ [""]

        neighbors = nub $ sort $ filter (/= p) $ concatMap conPnts consTouching
        neighborSection =
          "Neighbors:"
          : [printf "  p%d :: %s" n (finalStr n) | n <- neighbors]
