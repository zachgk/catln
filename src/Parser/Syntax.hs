--------------------------------------------------------------------
-- |
-- Module    :  Parser.Syntax
-- Copyright :  (c) Zach Kimberg 2019
-- License   :  MIT
-- Maintainer:  zachary@kimberg.com
-- Stability :  experimental
-- Portability: non-portable
--
--------------------------------------------------------------------

module Parser.Syntax where

import           Syntax.Types
import           Syntax.Prgm
import           Syntax

type ParseMeta = PreTyped
type PArgMetaMap = ArgMetaMap ParseMeta
type PExpr = RawExpr ParseMeta
type PCompAnnot = CompAnnot PExpr
type PGuard = Guard PExpr
type PDeclSubStatement = RawDeclSubStatement ParseMeta
type PDeclLHS = DeclLHS ParseMeta PExpr
type PDecl = RawDecl ParseMeta
type PObjectMap = ObjectMap ParseMeta
type PRawTypeDef = RawTypeDef ParseMeta
type PStatement = RawStatement ParseMeta
type PObject = Object ParseMeta
type PArrow = Arrow ParseMeta
type PPrgm = RawPrgm ParseMeta
type PReplRes = ReplRes ParseMeta

type DesExpr = Expr ParseMeta
type DesCompAnnot = CompAnnot DesExpr
type DesGuard = Guard DesExpr
type DesPrgm = Prgm ParseMeta

emptyMeta :: ParseMeta
emptyMeta = PreTyped RawTopType
