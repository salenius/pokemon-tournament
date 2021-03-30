{-# LANGUAGE ViewPatterns, RecordWildCards #-}

module Domain.SideEffect.Poisoned where

import Domain.SideEffect.AilmentCommon
import Domain.Attribute.TypeOf
import Domain.Attribute.Ability
import Domain.Pokemon.Species

data IsImmunity = IsImmunity deriving (Eq,Show)

data IsPoisonType = IsPoisonType | IsSteelType deriving (Eq,Show)

type TargetPoisoned = TargetAilmented IsImmunity IsPoisonType

type UserPoisoned = UserAilmented IsImmunity IsPoisonType


isImmunity :: Ability -> Maybe IsImmunity
isImmunity Immunity = Just IsImmunity
isImmunity _ = Nothing

isPoisonType :: TypeOfPokemon -> Maybe IsPoisonType
isPoisonType (elem Poison . getTypeOfPokemon -> True) = Just IsPoisonType
isPoisonType (elem Steel . getTypeOfPokemon -> True) = Just IsSteelType
isPoisonType _ = Nothing

userPoisoned :: AilmentData battle -> battle -> UserPoisoned
userPoisoned = userAilmented isImmunity isPoisonType

targetPoisoned :: AilmentData battle -> battle -> TargetPoisoned
targetPoisoned = targetAilmented isImmunity isPoisonType

