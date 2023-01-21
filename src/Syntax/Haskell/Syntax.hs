--------------------------------------------------------------------
-- |
-- Module    :  Syntax.Haskell.Syntax
-- Copyright :  (c) Zach Kimberg 2023
-- License   :  MIT
-- Maintainer:  zachary@kimberg.com
-- Stability :  experimental
-- Portability: non-portable
--
-- This module defines conversions to and from haskell syntax
--------------------------------------------------------------------

module Syntax.Haskell.Syntax where

import           Control.Monad
import           Data.ByteString          as BS hiding (concat, reverse)
import qualified Data.HashMap.Strict      as H
import           Foreign                  (Ptr)
import           Syntax.Common.TreeSitter
import           Syntax.Haskell.Prgm
import           Text.Printf
import           TreeSitter.Haskell       (tree_sitter_haskell)
import           TreeSitter.Node
import           TreeSitter.Parser
import           TreeSitter.Tree

parseHaskellStatement :: BS.ByteString -> Ptr Node -> IO HsStatement
parseHaskellStatement fileContents statementP = do
  withReadNode statementP $ \statement -> do
    estatement <- easyNode statement
    case eType estatement of
      "function_declaration" -> do
        withNamedChildNodeMap statement $ \children -> do
          -- varId <- case H.lookupDefault [] "variable_identifier" children of
          _varId <- forM (H.lookupDefault [] "variable_identifier" children) $ \(varIdP, _) -> do
            failParse "variable_identifier" fileContents varIdP
          return HsFunctionDeclaration
      _ -> failParse "unknown statement type" fileContents statementP

parseHaskellModule :: BS.ByteString -> Ptr Node -> IO HsModule
parseHaskellModule fileContents nodeP = do
  withReadNode nodeP $ \node -> do
    withNamedChildNodeMap node $ \children -> do
      comments <- forM (H.lookupDefault [] "comment" children) $ \(commentP, _) -> do
        withReadNode commentP $ \commentN -> do
          nodeContents fileContents commentN
      pragmas <- forM (H.lookupDefault [] "pragma" children) $ \(pragmaP, _) -> do
        withReadNode pragmaP $ \pragmaN -> do
          nodeContents fileContents pragmaN
      modId <- case H.lookupDefault [] "module_identifier" children of
        [(idP, _)] -> do
          withReadNode idP $ \idN -> do
            nodeContents fileContents idN
        ids -> error $ printf "Expected one module_identifier but found: %s" (show ids)
      statements <- forM (H.lookupDefault [] "where" children) $ \(whereP, _) -> do
        withReadNode whereP $ \whereN -> do
          withNamedNodeChildren whereN $ \statements -> do
            forM statements $ \statementP -> do
              parseHaskellStatement fileContents statementP
      return $ HsModule (reverse comments) (reverse pragmas) modId (reverse $ concat statements)

parseHaskell :: BS.ByteString -> IO HsModule
parseHaskell fileContents = do
  parser <- ts_parser_new
  _ <- ts_parser_set_language parser tree_sitter_haskell
  withParseTree parser fileContents $ \tree -> do
    withRootNode tree $ \node -> do
      parseHaskellModule fileContents node
