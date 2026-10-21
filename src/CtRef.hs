{-# LANGUAGE PatternSynonyms #-}
--------------------------------------------------------------------
-- |
-- Module    :  CtRef
-- Copyright :  (c) Zach Kimberg 2024
-- License   :  MIT
-- Maintainer:  zachary@kimberg.com
-- Stability :  experimental
-- Portability: non-portable
--
-- This module implements the @catln ref@ reference queries, currently the object index.
--------------------------------------------------------------------

module CtRef
  ( printRefIndex
  ) where

import           Control.Monad           (forM_)
import           Control.Monad.Writer    (execWriter, tell)
import           CtConstants             (exampleAnnot, testAnnot)
import qualified Data.HashMap.Strict     as H
import           Data.List               (groupBy, sortOn)
import           Data.Maybe              (catMaybes, mapMaybe)
import           MapMeta                 (mapMetaObjArrM)
import           Semantics.Prgm          (exprPath, flatObjectMapItem,
                                          getMetaPos, getOaAnnots, oaBasis,
                                          pattern ObjectMap, prgmObjMap,
                                          showCodeRange)
import           Semantics.Types         (NPath (npathPath), getPath, mkName,
                                          nameStr, relativeNameMatches)
import           Syntax.Ct.Parser.Syntax (DesPrgm)
import           Text.Printf             (printf)
import           Utils                   (treeBranch)

-- | An object in the reference index: its namespace path components (last is the leaf name),
-- its source location, and its kind (the 'Semantics.Prgm.ObjectBasis').
data RefEntry = RefEntry
  { reComps :: [String]
  , reLoc   :: String
  , reKind  :: String
  }

-- | Print all objects of a desugared program as a namespace tree (@/Data@ contains everything
-- under it), one line per object with its name, source location and kind. Objects carrying a
-- @#test@ or @#example@ annotation are excluded.
printRefIndex :: DesPrgm -> IO ()
printRefIndex prgm = do
  let ObjectMap objMap = prgmObjMap prgm
      entries = mapMaybe toEntry (H.toList objMap)
  printRefLevel "" entries
  putStrLn $ printf "%d objects" (length entries)
  where
    -- Annotation paths are still relative (@#test@, not @/Catln/#test@) at the desugar stage,
    -- so match them with 'relativeNameMatches' rather than an exact compare.
    isExcludedAnnot a = relativeNameMatches (exprPath a) (mkName testAnnot)
                     || relativeNameMatches (exprPath a) (mkName exampleAnnot)
    toEntry (name, item) =
      case filter (not . any isExcludedAnnot . getOaAnnots) (flatObjectMapItem item) of
        []          -> Nothing
        oas@(rep:_) -> case npathPath (getPath (nameStr name)) of
          []    -> Nothing
          comps -> Just RefEntry
            { reComps = comps
            -- The object-expr's own top meta often lacks a position (desugar synthesizes it);
            -- collect every position in the object's arrows and take the earliest as its location.
            , reLoc   = case concatMap oaPositions oas of
                []  -> ""
                crs -> showCodeRange (minimum crs)
            , reKind  = show (oaBasis rep)
            }
    oaPositions oa = catMaybes $ execWriter $
      mapMetaObjArrM (\_ m -> tell [getMetaPos m] >> return m) Nothing oa

-- | Print objects as a namespace tree using 'treeBranch' connectors. At each level, entries with
-- a single remaining component are leaves; the rest are grouped by their head component into child
-- namespaces and recursed into.
printRefLevel :: String -> [RefEntry] -> IO ()
printRefLevel prefix entries = do
  let leaves = [(head (reComps e), reLoc e, reKind e) | e <- entries, length (reComps e) == 1]
      children = [e | e <- entries, length (reComps e) > 1]
      items = sortOn itemName (map Left (groupChildren children) ++ map Right leaves)
      total = length items
  forM_ (zip [1 ..] items) $ \(idx, item) -> do
    let (connector, extension) = treeBranch (idx == total)
    case item of
      Left (nsName, subs) -> do
        putStrLn $ prefix ++ connector ++ nsName ++ "/"
        printRefLevel (prefix ++ extension) subs
      Right (nm, loc, kind) ->
        -- The anonymous/unit type has the empty name; show it as () rather than a blank.
        putStrLn $ prefix ++ connector ++ printf "%s  %s  [%s]" (if null nm then "()" else nm) loc kind
  where
    itemName (Left (nsName, _)) = nsName
    itemName (Right (nm, _, _)) = nm

-- | Group child entries by their head path component, dropping that component so each group can be
-- recursed into as a sub-namespace.
groupChildren :: [RefEntry] -> [(String, [RefEntry])]
groupChildren es =
  map (\grp -> (head (reComps (head grp)), map dropHead grp))
  $ groupBy (\a b -> head (reComps a) == head (reComps b))
  $ sortOn (head . reComps) es
  where
    dropHead e = e {reComps = tail (reComps e)}
