--------------------------------------------------------------------
-- |
-- Module    :  Syntax.Haskell.Convert
-- Copyright :  (c) Zach Kimberg 2023
-- License   :  MIT
-- Maintainer:  zachary@kimberg.com
-- Stability :  experimental
-- Portability: non-portable
--
-- This module defines converting from HsModule to a RawPrgm
--------------------------------------------------------------------

module Syntax.Haskell.Convert where
import           DynFlags
import           GHC.Hs
import           Module
import           Outputable
import           SrcLoc
import           Syntax.Ct.Prgm
import           Text.Printf

convertImport :: DynFlags -> ImportDecl GhcPs -> String
convertImport _ (ImportDecl _ _ name Nothing False False NotQualified _ Nothing Nothing) = moduleNameSlashes $ unLoc name
convertImport flags p = error $ printf "Convert unsupported import:\n%s" (showSDoc flags $ ppr p)

convertDecl :: DynFlags -> HsDecl GhcPs -> RawStatementTree RawExpr ()
convertDecl flags p = error $ printf "Convert unsupported decl:\n%s" (showSDoc flags $ ppr p)

convertModule :: DynFlags -> HsModule GhcPs -> RawPrgm ()
convertModule flags (HsModule (Just name) Nothing imports decls Nothing Nothing) = (map (convertImport flags . unLoc) imports, [RawStatementTree (RawModule $ moduleNameSlashes $ unLoc name) (map (convertDecl flags . unLoc) decls)])
convertModule flags p = error $ printf "Convert unsupported Module:\n%s" (showSDoc flags $ ppr p)
