{-# LANGUAGE ViewPatterns #-}

module Domain.SideEffect.Frozen where

import Domain.SideEffect.AilmentCommon
import Domain.Attribute.Ability
import Domain.Attribute.TypeOf
import Domain.Algebra.Effect
import Domain.Pokemon.Species
import Control.Monad.Reader
import Control.Applicative
import Data.Maybe

type UserFrozen = UserAilmented IsMagmaArmor IsIceType

data IsMagmaArmor = IsMagmaArmor deriving (Eq,Show)

data IsIceType = IsIceType deriving (Eq,Show)

isIceType :: TypeOfPokemon -> Maybe IsIceType
isIceType (elem Ice . getTypeOfPokemon -> True) = Just IsIceType
isIceType _ = Nothing

isMagmaArmor :: Ability -> Maybe IsMagmaArmor
isMagmaArmor MagmaArmor = Just IsMagmaArmor
isMagmaArmor _ = Nothing

data TargetFrozen =
  HasAilmentBlocking (AilmentBlocking IsMagmaArmor)
  | MoldBreakerCanceling (MoldBreakerCancels IsMagmaArmor)
  | HasTypeBlockingFreeze (TypeBlocking IsIceType)
  | FreezingGeneralCase
  deriving (Eq,Show)

instance AilmentToEffect TargetFrozen where
  toEffect (HasAilmentBlocking _) = doNothing
  toEffect (MoldBreakerCanceling _) = return FreezingGeneralCase
  toEffect (HasTypeBlockingFreeze _) = doNothing
  toEffect x = return x
  
userFrozen :: AilmentData battle -> battle -> UserFrozen
userFrozen = userAilmented isMagmaArmor isIceType

targetFrozen :: AilmentData battle -> battle -> TargetFrozen
targetFrozen ad b =
  fromMaybe FreezingGeneralCase .
  flip runReaderT (ad,b) $
  HasTypeBlockingFreeze <$> hasBlockingType isIceType <|>
  HasAilmentBlocking <$> (hasAlreadyAilment <|> hasSafeguard) <|>
  MoldBreakerCanceling <$> moldBreakerBlocks isMagmaArmor <|>
  HasAilmentBlocking <$>  hasBlockingAbility isMagmaArmor

