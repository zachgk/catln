--------------------------------------------------------------------
-- |
-- Module    :  Syntax.HsBuiltins.Render
-- Copyright :  (c) Zach Kimberg 2026
-- License   :  MIT
-- Maintainer:  zachary@kimberg.com
-- Stability :  experimental
-- Portability: non-portable
--
-- A derived declaration written as the statement it declares.
--
-- "Semantics.CtType" reads a Haskell type into a 'CtDecl' — a path, its constructors, and what the
-- page says about it — and stops there. This is where that becomes Catln, with the
-- "Syntax.Ct.Builder" combinators rather than by writing text and parsing it back.
--
-- The split is what lets a type's reading be written beside the type. "Syntax.Ct.Builder" imports
-- "Semantics.Prgm", so a 'Semantics.CtType.CtType' class producing statements could never have an
-- instance in "Semantics.Prgm" or "Semantics.Types" — the instances would have to sit above the
-- syntax, far from what they describe. Describing a declaration and rendering it separately costs
-- one datatype and removes that.
--------------------------------------------------------------------

module Syntax.HsBuiltins.Render where

import           Semantics.CtType
import           Syntax.Ct.Builder
import           Syntax.Ct.Parser.Syntax (PExpr, PStatementTree)
import           Syntax.Ct.Prgm          (RawStatement (..),
                                          RawStatementTree (..))

-- | The statement declaring a type, with whatever it says about itself beneath it.
--
-- A sum is ONE statement with its constructors written inline in the class's member list, which is
-- how @stack\/core\/bool.ct@ writes @class(Boolean, [True, False])@ and @stack\/core\/maybe.ct@
-- writes @class(Maybe[$T], [Maybe\/Some[$T -> $_T](value -> $T), Maybe\/Nothing])@ — rather than a
-- class and a @data@ per arm, which would be the same thing said in more places.
renderCtDecl :: CtDecl -> PStatementTree
renderCtDecl (CtDecl path cons doc) = RawStatementTree statement docStatements
  where
    statement = case cons of
      [con] -> dataSt (renderCtCon con)
      _     -> classWithObjs (rawVal path) (map renderCtCon cons) []
    docStatements = [RawStatementTree (RawAnnot (rawComment d)) [] | d <- doc]

-- | One constructor as it is written in a declaration: its path applied to each argument's declared
-- type. A constructor with no arguments is the bare path, since 'applyRawEIArgs' of nothing is what
-- it was given.
renderCtCon :: CtCon -> PExpr
renderCtCon (CtCon path vars fields) = rawVal path `withVars` map declVar vars `applyRawEIArgs` map field fields
  where
    -- A parameter DECLARED is applied to nothing, which is the @[$A, $B]@ of
    -- @data(Pair[$A, $B](…))@ and the @[$T]@ of @class(ConsList[$T], …)@. A parameter applied to
    -- something is a 'CtTypeRef' and goes through 'renderCtTypeRef' instead.
    withVars base [] = base
    withVars base vs = base `applyRawExprEVars` vs
    declVar varName = (rawVal varName, Nothing)
    -- The argument's name as WRITTEN, not as a 'Semantics.Types.partialKey' — which would make it
    -- absolute (@\/name@) here, where the parser leaves a written @name@ relative for the desugar to
    -- make absolute. Both reach the same declaration; only this one reads back as what a person
    -- would have typed.
    field (argName, argType) = (rawVal argName, IArgM (Just (renderCtTypeRef argType)))

-- | A type as it is written where it is used: its path, with whatever it carries applied to it.
-- Applying nothing is the path alone, which is how every unparameterized type is written.
renderCtTypeRef :: CtTypeRef -> PExpr
renderCtTypeRef (CtTypeRef path vars) = rawVal path `applyRawExprEVars` map var vars
  where var (varName, varType) = (rawVal varName, Just (renderCtTypeRef varType))

-- | The declarations of a page, as the statements it carries.
renderCtDecls :: CtPage -> [PStatementTree]
renderCtDecls = map renderCtDecl . cpDecls
