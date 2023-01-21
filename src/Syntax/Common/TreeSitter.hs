--------------------------------------------------------------------
-- |
-- Module    :  Syntax.Common.TreeSitter
-- Copyright :  (c) Zach Kimberg 2023
-- License   :  MIT
-- Maintainer:  zachary@kimberg.com
-- Stability :  experimental
-- Portability: non-portable
--
-- This module has utilities for working with TreeSitter
--------------------------------------------------------------------

module Syntax.Common.TreeSitter where
import           Control.Monad
import qualified Data.ByteString     as BS
import qualified Data.HashMap.Strict as H
import           Data.List
import qualified Data.Text           as T
import qualified Data.Text.Encoding  as T
import           Foreign
import           Foreign.C           (peekCString)
import           Text.Printf
import           TreeSitter.Node

data ENode = ENode {
        eType        :: !String
        , eFieldName :: !(Maybe String)
        , eIsNamed   :: !Bool
        , eIsExtra   :: !Bool
           } deriving (Eq, Ord, Show)

easyNode :: Node -> IO ENode
easyNode node = do
  tp <- peekCString (nodeType node)
  fieldName <- case nodeFieldName node of
    n | n == nullPtr -> return Nothing
    n                -> Just <$> peekCString n
  let isNamed = toInteger (nodeIsNamed node) > 0
  let isExtra = toInteger (nodeIsExtra node) > 0
  return $ ENode tp fieldName isNamed isExtra

withReadNode :: Ptr Node -> (Node -> IO a) -> IO a
withReadNode nodeP f = do
  node <- peek nodeP
  f node

nodeContents :: BS.ByteString -> Node -> IO String
nodeContents fileContents node = do
  let start = fromIntegral $ nodeStartByte node
  let end = fromIntegral $ nodeEndByte node
  let len = end - start
  return $ T.unpack $ T.decodeUtf8 $ BS.take len $ BS.drop start fileContents

withNodeChildren :: Node -> ([Ptr Node] -> IO a) -> IO a
withNodeChildren node f = do
  let childCount = fromIntegral (nodeChildCount node)
  children <- mallocArray childCount
  tsNode <- malloc
  poke tsNode (nodeTSNode node)
  ts_node_copy_child_nodes tsNode children
  let children' = map (plusPtr children . (*) (sizeOf node)) [0..childCount-1]
  res <- f children'
  free children
  free tsNode
  return res

withNamedNodeChildren :: Node -> ([Ptr Node] -> IO a) -> IO a
withNamedNodeChildren node f = withNodeChildren node $ \nodes -> do
  nodes' <- filterM (\nodeP -> do
                        withReadNode nodeP $ \n -> do
                          en <- easyNode n
                          return $ eIsNamed en
                        ) nodes
  f nodes'

withNamedChildNodeMap :: Node -> (H.HashMap String [(Ptr Node, ENode)] -> IO a) -> IO a
withNamedChildNodeMap node f = do
  withNamedNodeChildren node $ \children -> do
    children' <- forM children $ \childP -> do
      withReadNode childP $ \child -> do
        echild <- easyNode child
        return (eType echild, [(childP, echild)])
    f (H.fromListWith (++) children')


failParse :: String -> BS.ByteString -> Ptr Node -> IO a
failParse msg fileContents nodeP = do
  withReadNode nodeP $ \node -> do
    enode <- easyNode node
    contents <- nodeContents fileContents node
    withNamedNodeChildren node $ \children -> do
      showChildren <- forM children $ \childP -> do
        withReadNode childP $ \child -> do
          eChild <- easyNode child
          return $ show eChild
      return $ error $ printf "Failed to parse TreeSitter Node: %s\n\tNode: %s\n\tWithChildren: \n\t\t%s \n\tContents: %s" msg (show enode) (intercalate "\n\t\t" showChildren) contents
