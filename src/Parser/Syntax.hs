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

import           Syntax

type ParseMeta = PreTyped
type PExpr = RawExpr ParseMeta
type PCompAnnot = CompAnnot ParseMeta
type PGuard = Guard ParseMeta
type PDeclSubStatement = RawDeclSubStatement ParseMeta
type PDeclLHS = DeclLHS ParseMeta
type PDecl = RawDecl ParseMeta
type PObjectMap = ObjectMap ParseMeta
type PRawTypeDef = RawTypeDef ParseMeta
type PStatement = RawStatement ParseMeta
type PObject = Object ParseMeta
type PArrow = Arrow ParseMeta
type PPrgm = RawPrgm ParseMeta
type PReplRes = ReplRes ParseMeta

type DesPrgm = Prgm ParseMeta

emptyMeta :: ParseMeta
emptyMeta = PreTyped RawTopType
