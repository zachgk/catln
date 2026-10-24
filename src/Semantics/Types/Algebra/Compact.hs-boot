module Semantics.Types.Algebra.Compact where

import           Semantics.Types.Data

excludeConstants :: [Constant] -> Type -> Type
constantWithinTop :: (TypeGraph tg) => TypeEnv tg -> TypeVarArgEnv -> Constant -> (TypePredicates, PartialLeafs, [Constant]) -> Maybe [TypePredicates]
compactPartialsWithClassPred :: (TypeGraph tg) => TypeEnv tg -> TypeVarArgEnv -> [PartialType] -> PartialLeafs
compactType :: (TypeGraph tg) => TypeEnv tg -> TypeVarArgEnv -> Type -> Type
