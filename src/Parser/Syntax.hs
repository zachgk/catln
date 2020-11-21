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

import qualified Data.HashMap.Strict as H

import           Syntax.Types
import           Syntax.Prgm
import           Syntax

type ParseMeta = PreTyped
type PTupleArg = RawTupleArg ParseMeta
type PExpr = RawExpr ParseMeta
type PPattern = Pattern PExpr ParseMeta
type PCompAnnot = CompAnnot PExpr
type PGuard = Guard PExpr
type PDeclSubStatement = RawDeclSubStatement ParseMeta
type PDeclLHS = DeclLHS PExpr ParseMeta
type PDecl = RawDecl ParseMeta
type PObjectMap = ObjectMap PExpr ParseMeta
type PMultiTypeDef = MultiTypeDef ParseMeta
type PTypeDef = TypeDef ParseMeta
type PStatement = RawStatement ParseMeta
type PArgMetaMap = H.HashMap ArgName ParseMeta
type PObjArg = ObjArg ParseMeta
type PObject = Object ParseMeta
type PArrow = Arrow PExpr ParseMeta
type PPrgm = RawPrgm ParseMeta
type PPrgms = RawPrgms ParseMeta
type PReplRes = ReplRes ParseMeta



-- Intermediate expr during desugaring
data PSemiExpr m
  = PSCExpr m Constant
  | PSValue m TypeName
  | PSTupleApply m (m, PSemiExpr m) (Maybe ArgName) (PSemiExpr m)
  deriving (Eq, Ord, Show)
type PSExpr = PSemiExpr ParseMeta

type PSCompAnnot = CompAnnot PSExpr
type PSGuard = Guard PSExpr
type PSDeclLHS = DeclLHS PSExpr ParseMeta

data PSemiDecl = PSemiDecl PSDeclLHS [PSCompAnnot] (Maybe PSExpr)
  deriving (Eq, Ord, Show)




type DesExpr = IExpr ParseMeta
type DesCompAnnot = CompAnnot DesExpr
type DesGuard = Guard DesExpr
type DesObjectMap = ObjectMap DesExpr ParseMeta
type DesObject = Object ParseMeta
type DesArrow = Arrow DesExpr ParseMeta
type DesPrgm = Prgm DesExpr ParseMeta

emptyMeta :: ParseMeta
emptyMeta = PreTyped TopType
