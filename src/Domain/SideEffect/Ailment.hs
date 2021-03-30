{-# LANGUAGE ViewPatterns, RecordWildCards #-}

module Domain.SideEffect.Ailment where

import Domain.Attribute.Counterparty
import Domain.Attribute.Ailment
import Domain.Attribute.Ability
import Domain.Attribute.Screen
import Domain.Attribute.TypeOf
import Domain.Pokemon.Species
import Domain.Attribute.Weather
import Domain.Algebra.EffectOnCounterparty
import Domain.Algebra.Path
import Data.Maybe
import Data.Functor.Contravariant


data AilmentSpawnsEffect =
  TargetHasSynchronize
  deriving (Eq,Show)


data BoreAilment burn para pois free slee =
  BoreBurn burn
  | BoreParalysis para
  | BorePoison pois
  | BoreFreeze free
  | BoreSleep slee
  deriving (Eq,Show)

type IsBlockedOrNot ability tyyppi =
  Either (MoldBreakerBlocks ability) (AilmentBlockingExtended
                                      (AilmentBlocking ability) tyyppi) 

type UserAilment =
  BoreAilment (UserAilmentPathBasic IsWaterVeil IsFireType) (UserAilmentPathBasic IsLimber IsElectricType) (UserAilmentPathBasic IsImmunity IsPoisonType) UserFrozen UserSleep

type TargetAilment =
  BoreAilment (TargetAilmentPathBasic IsWaterVeil IsFireType) (TargetAilmentPathBasic IsLimber IsElectricType) (TargetAilmentPathBasic IsImmunity IsPoisonType) TargetFrozen TargetSleep

type UserAilmentPathBasic ability tyyppi =
  Path (AilmentBlockingExtended (AilmentBlocking ability) tyyppi) ()

type TargetAilmentPathBasic ability tyyppi =
  Path (IsBlockedOrNot ability tyyppi) AilmentSpawnsEffect

type TargetFrozen = Path (IsBlockedOrNot IsMagmaArmor IsIceType) ()

type TargetSleep =
  Path (Either (MoldBreakerBlocks IsInsomnia) (AilmentBlocking IsInsomnia)) ()

type UserFrozen =
  Path (AilmentBlockingExtended IsInsomnia IsIceType) ()

type UserSleep =
  Path (AilmentBlocking IsInsomnia) ()

-- Apudatatyypit


data IsLimber = IsLimber deriving (Eq,Show)

data IsMagmaArmor = IsMagmaArmor deriving (Eq,Show)


data IsWaterVeil =
  IsWaterVeil
  | IsWaterBubble
  deriving (Eq,Show)

data IsInsomnia =
  IsInsomnia
  | IsVitalSpirit
  deriving (Eq,Show)

data IsAilmentImmunityAbility =
  ImmunityToParalysis IsLimber
  | ImmunityToBurn IsWaterVeil
  | ImmunityToPoison IsImmunity
  | ImmunityToFreeze IsMagmaArmor
  | ImmunityToSleep IsInsomnia
  | CaseHealthy
  deriving (Eq,Show)

data IsIceType = IsIceType deriving (Eq,Show)


data IsFireType = IsFireType deriving (Eq,Show)

data IsElectricType = IsElectricType deriving (Eq,Show)




hasAilmentBlockingAbility
  :: Ailment
  -> Ability
  -> Maybe (AilmentBlocking IsAilmentImmunityAbility)
hasAilmentBlockingAbility ailm ab =
  case ailm of
    Burned -> f isWaterVeil ImmunityToBurn
    Poisoned -> f isImmunity ImmunityToPoison
    Paralyzed -> f isLimber ImmunityToParalysis
    Frozen -> f isMagmaArmor ImmunityToFreeze
    Sleep -> f isInsomnia ImmunityToSleep
    Healthy -> return $ HasAilmentBlockingAbility CaseHealthy
  where
    f fn imm = do
      ar <- fn ab
      return $ HasAilmentBlockingAbility $ imm ar

moldBreakerBlocks
  :: Ability
  -> AilmentBlocking ability
  -> Maybe (MoldBreakerBlocks ability)
moldBreakerBlocks ab HasLeafGuard = do
  m' <- isMoldBreaker ab
  return $ MoldBreakerBlocks m' HasLeafGuard
moldBreakerBlocks ab (HasAilmentBlockingAbility a) = do
  m' <- isMoldBreaker ab
  return $ MoldBreakerBlocks m' (HasAilmentBlockingAbility a)
moldBreakerBlocks ab (HasComatose c) = do
  m' <- isMoldBreaker ab
  return $ MoldBreakerBlocks m' (HasComatose c)
moldBreakerBlocks _ _ = Nothing

-- Apuparserit


isLimber :: Ability -> Maybe IsLimber
isLimber Limber = Just IsLimber
isLimber _ = Nothing

isMagmaArmor :: Ability -> Maybe IsMagmaArmor
isMagmaArmor MagmaArmor = Just IsMagmaArmor
isMagmaArmor _ = Nothing


isWaterVeil :: Ability -> Maybe IsWaterVeil
isWaterVeil WaterVeil = Just IsWaterVeil
isWaterVeil WaterBubble = Just IsWaterBubble
isWaterVeil _ = Nothing

isInsomnia :: Ability -> Maybe IsInsomnia
isInsomnia Insomnia = Just IsInsomnia
isInsomnia VitalSpirit = Just IsVitalSpirit
isInsomnia _ = Nothing
