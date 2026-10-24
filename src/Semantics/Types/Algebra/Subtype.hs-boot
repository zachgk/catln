module Semantics.Types.Algebra.Subtype where

import           Semantics.Types.Data

comparedPartialsEnv :: (TypeGraph tg) => TypeEnv tg -> TypeVarArgEnv -> PartialType -> PartialType -> TypeVarArgEnv
isSubtypeOfWithEnv :: (TypeGraph tg) => TypeEnv tg -> TypeVarArgEnv -> Type -> Type -> Bool
