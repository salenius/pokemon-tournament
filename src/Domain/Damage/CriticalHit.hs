{-# LANGUAGE TemplateHaskell, GADTs, RankNTypes #-}

module Domain.Damage.CriticalHit (
  reduce,
  produceCriticalHit,
  CriticalHitDamage(..),
  CriticalHitCalc(..),
  damageAbilityOfPokemon,
  damageAbilityImpactOnHit,
  calcInitialLevel,
  calcAbilityOfPokemon,
  calcDrawRandomDouble,
  calcAilmentOfPokemon,
  calcIntoCriticalHit
                                 ) where

import Control.Lens
import Domain.Attribute.CriticalHit
import Domain.Attribute.Ability
import Domain.Attribute.Ailment
import Domain.Attribute.HeldItem
import Domain.Attribute.Counterparty
import Domain.Attribute.Damage

data CriticalHitDamage battle = CriticalHitDamage
  {
    _damageAbilityOfPokemon :: Counterparty -> battle -> Ability
  , _damageAbilityImpactOnHit :: Ability -> CriticalHit -> Double
  }

makeLenses ''CriticalHitDamage

data CriticalHitCalc monad battle = CriticalHitCalc
  {
    _calcInitialLevel :: Int
  , _calcCriticalHitChance :: Int -> Double
  , _calcAbilityOfPokemon :: Counterparty -> battle -> Ability
  , _calcAilmentOfPokemon :: Counterparty -> battle -> Ailment
  , _calcItemOfPokemon :: Counterparty -> battle -> Maybe HeldItem
  , _calcDrawRandomDouble :: monad Double
  , _calcIntoCriticalHit :: Int -> Double -> CriticalHit
  }

makeLenses ''CriticalHitCalc

criticalHitChance :: Int -> Double
criticalHitChance x
  | x <= 0 = 0.0625
  | x == 1 = 0.125
  | x == 2 = 0.5
  | otherwise = 1.0

reduce :: CriticalHitDamage battle -> battle -> Damage
reduce = undefined

produceCriticalHit :: Monad m => CriticalHitCalc m battle -> battle -> m CriticalHit
produceCriticalHit calc battle = do
  let lvl = unwrapState (view calcInitialLevel calc) . parseState calc $ battle
  rand <- view calcDrawRandomDouble calc
  let std = view calcCriticalHitChance calc lvl
  return $ case rand < std of
    True -> IsCritical
    False -> NotCritical

data BattleState a where
  NoEffect :: BattleState a
  WrapEffect :: BattleState a -> BattleState CriticalHit
  TargetHasBattleArmor :: BattleState Ability
  TargetHasShellArmor :: BattleState Ability
  UserHasMerciless :: BattleState Ailment
  UserHasMoldBreaker :: BattleState Ability -> BattleState a
  UserHasScopeLens :: BattleState HeldItem
  CombineFactors :: BattleState a -> BattleState a -> BattleState a

type ParseState a =
  forall battle. forall m. CriticalHitCalc m battle -> battle -> BattleState a

parseState :: ParseState CriticalHit
parseState calc battle =
  let
    targetHasBArmor = case view calcAbilityOfPokemon calc Target battle of
      ShellArmor -> TargetHasShellArmor
      BattleArmor -> TargetHasBattleArmor
      _ -> NoEffect

    targetHasBArmor' = case view calcAbilityOfPokemon calc User battle of
      MoldBreaker -> UserHasMoldBreaker targetHasBArmor
      Teravolt -> UserHasMoldBreaker targetHasBArmor
      Turboblaze -> UserHasMoldBreaker targetHasBArmor
      _ -> targetHasBArmor

    userHasMerciless =
      case (view calcAbilityOfPokemon calc User battle,
            view calcAilmentOfPokemon calc Target battle) of
        (Merciless, Poisoned) -> UserHasMerciless
        _ -> NoEffect

    userHasScopeLens = case view calcItemOfPokemon calc User battle of
      Just ScopeLens -> UserHasScopeLens
      _ -> NoEffect

  in WrapEffect targetHasBArmor' `CombineFactors`
  WrapEffect userHasMerciless `CombineFactors`
  WrapEffect userHasScopeLens

unwrapState :: Int -> BattleState a -> Int
unwrapState x NoEffect = x
unwrapState x (WrapEffect y) = unwrapState x y
unwrapState _ TargetHasBattleArmor = 0
unwrapState _ TargetHasShellArmor = 0
unwrapState x (UserHasMoldBreaker _) = x
unwrapState x UserHasScopeLens = x + 1
unwrapState _ UserHasMerciless = 4
unwrapState _ (CombineFactors (WrapEffect TargetHasBattleArmor) _) = 0
unwrapState _ (CombineFactors _ (WrapEffect TargetHasBattleArmor)) = 0
unwrapState _ (CombineFactors (WrapEffect TargetHasShellArmor) _) = 0
unwrapState _ (CombineFactors _ (WrapEffect TargetHasShellArmor)) = 0
unwrapState _ (CombineFactors (WrapEffect UserHasMerciless) _) = 4
unwrapState _ (CombineFactors _ (WrapEffect UserHasMerciless)) = 4
unwrapState x (CombineFactors y z) = unwrapState (unwrapState x y) z
