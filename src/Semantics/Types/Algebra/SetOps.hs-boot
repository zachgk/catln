module Semantics.Types.Algebra.SetOps where

import           Semantics.Types.Data

unionTypesWithEnv :: (TypeGraph tg) => TypeEnv tg -> TypeVarArgEnv -> Type -> Type -> Type
unionAllTypesWithEnv :: (TypeGraph tg, Foldable f) => TypeEnv tg -> TypeVarArgEnv -> f Type -> Type
unionAllTypesWithEnvOnce :: (TypeGraph tg) => TypeEnv tg -> TypeVarArgEnv -> [Type] -> Type
unionAllTypes :: (TypeGraph tg, Foldable f) => TypeEnv tg -> f Type -> Type
intersectPartials :: (TypeGraph tg) => TypeEnv tg -> TypeVarArgEnv -> PartialType -> PartialType -> (TypeVarArgEnv, [PartialType])
intersectPartialLeafsWithVarEnv :: (TypeGraph tg) => TypeEnv tg -> TypeVarArgEnv -> PartialLeafs -> PartialLeafs -> (TypeVarArgEnv, PartialLeafs)
intersectTypesWithVarEnv :: (TypeGraph tg) => TypeEnv tg -> TypeVarArgEnv -> Type -> Type -> (TypeVarArgEnv, Type)
intersectTypesEnv :: (TypeGraph tg) => TypeEnv tg -> TypeVarArgEnv -> Type -> Type -> Type
differencePartialLeafs :: (TypeGraph tg) => TypeEnv tg -> TypeVarArgEnv -> PartialLeafs -> PartialLeafs -> Type
partialEscapes :: (TypeGraph tg) => TypeEnv tg -> TypeVarArgEnv -> PartialType -> PartialType -> [PartialType]
differenceTypeWithEnv :: (TypeGraph tg) => TypeEnv tg -> TypeVarArgEnv -> Type -> Type -> (TypeVarArgEnv, Type)
complementTypeEnv :: (TypeGraph tg) => TypeEnv tg -> TypeVarArgEnv -> Type -> Type
meetInner :: (TypeGraph tg) => TypeEnv tg -> TypeVarArgEnv -> Type -> Type -> Type
