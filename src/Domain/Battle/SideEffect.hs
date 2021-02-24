{-# LANGUAGE TemplateHaskell, FlexibleContexts, MultiParamTypeClasses,
PatternSynonyms #-}

module Domain.Battle.SideEffect where

import Domain.Effect
import Domain.Battle.Logic
import Control.Lens
import Control.Applicative
import Domain.Attribute.Damage as D
import Domain.Attribute.MoveExecution
import Domain.Attribute.HP
import Domain.Attribute.Ailment
import Domain.Attribute.Ability
import Domain.Attribute.HeldItem
import Domain.Attribute.Counterparty
import Domain.Attribute.TypeOf
import Domain.Attribute.ModifStat
import Domain.Attribute.Weather as W
import Data.Maybe

data SideEffect =
  CauseAilment Ailment Counterparty
  | DropStat Counterparty ModifStat Int 
  | RaiseStat Counterparty ModifStat Int
  | AddPercentageOfHP Counterparty Double
  | CauseDamage D.Damage
  | CauseRecoil Double D.Damage
  | CauseFlinch
  | CauseLeechSeeded
  | SwitchPokemon Counterparty
  | PreventOHKO Counterparty
  | DropItem Counterparty
  | WithProbability Double SideEffect
  deriving (Eq,Show)

withProbability :: Double -> SideEffect -> SideEffect
withProbability prob (WithProbability prob' eff) =
  WithProbability (prob + prob') eff
withProbability prob eff = WithProbability prob eff

data UnparsedSideEffect = UnparsedSideEffect
  {
    sideEffect :: SideEffect
  , sideEffectValidation :: EffectValidation
  } deriving (Eq,Show)

appendValidation :: EffectValidation -> UnparsedSideEffect -> UnparsedSideEffect
appendValidation vldtn (UnparsedSideEffect eff vldtn') =
  UnparsedSideEffect eff $ vldtn `And` vldtn'

type EffectValidation = Fact StrikeFact

type SideEffect' = Effect UnparsedSideEffect

liftEffect eff = return . UnparsedSideEffect eff

moldBreaker = userAbilityIs MoldBreaker `Or`
  userAbilityIs Teravolt `Or`
  userAbilityIs Turboblaze

leafGuard = userAbilityIs LeafGuard `And` weatherIs W.Sunny

leafGuard' = intoTarget leafGuard `And` (Not moldBreaker)

whiteHerb' = targetHeldItemIs (Just WhiteHerb)

sturdy = userAbilityIs Sturdy `And` userHpIsFull

sturdy' = intoTarget sturdy `And` (Not moldBreaker)

poisonImmunityType =
  userHasTypeOf Poison `Or`
  userHasTypeOf Steel

poisonImmunityType' = intoTarget poisonImmunityType

clearBody' =
  ((targetAbilityIs ClearBody `Or`
  targetAbilityIs WhiteSmoke) `And` Not moldBreaker) `Or`
  targetAbilityIs FullMetalBody

notHealthy = Not $ userAilmentIs Healthy
notHealthy' = intoTarget notHealthy

synchronize' = targetAbilityIs Synchronize 

addSynchronize = enrichValidation synchronize'

chain' a b = a `chain` return b

spread' a b = a `spread` return b


-- Enrich effect by validations

enrichValidation fact eff = appendValidation fact <$> intoValidation eff

intoValidation :: SideEffect -> SideEffect'
intoValidation (WithProbability prob eff) =
  enrichValidation (Fact $ RandomDoubleLT prob) eff

intoValidation eff@(CauseAilment Poisoned Target) = do
  let vldtn = Not $ poisonImmunityType' `Or` leafGuard' `Or` notHealthy'
  cont <- addSynchronize $ CauseAilment Poisoned User
  liftEffect eff vldtn `chain` return cont

intoValidation eff@(CauseAilment Poisoned User) =
  liftEffect eff (Not $ poisonImmunityType `Or` leafGuard `Or` notHealthy)
  
intoValidation eff@(CauseAilment Healthy _) =
  liftEffect eff AlwaysTrue

intoValidation eff@(CauseAilment Burned Target) = do
  let vldtn = Not $ targetHasTypeOf Fire `Or` leafGuard' `Or` notHealthy'
  cont <- addSynchronize $ CauseAilment Burned User
  liftEffect eff vldtn `chain` return cont

intoValidation eff@(CauseAilment Burned User) = do
  liftEffect eff (Not $ userHasTypeOf Fire `Or` leafGuard `Or` notHealthy)

intoValidation eff@(CauseAilment Paralyzed Target) = do
  let vldtn = Not $ targetHasTypeOf Electric `Or` leafGuard' `Or` notHealthy'
  cont <- addSynchronize $ CauseAilment Paralyzed User
  liftEffect eff vldtn `chain` return cont

intoValidation eff@(CauseAilment Paralyzed User) = do
  liftEffect eff (Not $ userHasTypeOf Electric `Or` leafGuard `Or` notHealthy)

intoValidation eff@(CauseAilment Frozen User) =
  liftEffect eff (Not $ userHasTypeOf Ice `Or` leafGuard `Or` notHealthy)

intoValidation eff@(CauseAilment Frozen Target) =
  liftEffect eff (Not $ targetHasTypeOf Ice `Or` leafGuard' `Or` notHealthy')

intoValidation eff@(CauseAilment Sleep Target) =
  liftEffect eff (Not $ leafGuard `Or` notHealthy)

intoValidation eff@(CauseAilment Sleep User) =
  liftEffect eff (Not $ leafGuard)

intoValidation eff@(RaiseStat _ _ _) =
  liftEffect eff AlwaysTrue

intoValidation eff@(DropStat User _ _) =
  liftEffect eff (Not $ userHeldItemIs (Just WhiteHerb))

intoValidation eff@(DropStat Target Attack' _) =
  liftEffect eff (Not $ whiteHerb' `Or` clearBody' `Or` targetAbilityIs HyperCutter)

intoValidation eff@(DropStat Target Defence' _) =
  liftEffect eff (Not $ whiteHerb' `Or` clearBody' `Or` targetAbilityIs BigPecks)

intoValidation eff@(DropStat Target Accuracy' _) =
  liftEffect eff (Not $ whiteHerb' `Or` clearBody' `Or` targetAbilityIs KeenEye)

intoValidation eff@(DropStat Target _ _) =
  liftEffect eff (Not $ whiteHerb' `Or` clearBody')

intoValidation CauseFlinch = do
  let vldtn = Not $ targetAbilityIs InnerFocus
  cont <- enrichValidation (targetAbilityIs Steadfast) $ RaiseStat Target Speed' 1
  liftEffect CauseFlinch vldtn `chain` return cont

intoValidation eff@(SwitchPokemon User) = do
  naturalCure <- userAbilityIs NaturalCure `enrichValidation` CauseAilment Healthy User
  regenerator <- userAbilityIs Regenerator `enrichValidation` AddPercentageOfHP User 0.33
  intimidate <- player2nextPokemon (userAbilityIs Intimidate) `enrichValidation`
    DropStat Target Attack' 1
  liftEffect eff AlwaysTrue `chain`
    (return naturalCure `spread'` regenerator `spread'` intimidate)

intoValidation eff@(SwitchPokemon Target) = do
  naturalCure <- targetAbilityIs NaturalCure `enrichValidation` CauseAilment Healthy Target
  regenerator <- targetAbilityIs Regenerator `enrichValidation` AddPercentageOfHP Target 0.33
  intimidate <- player2nextPokemon (targetAbilityIs Intimidate) `enrichValidation`
    DropStat User Attack' 1
  liftEffect eff (Not $ targetAbilityIs SuctionCups) `chain`
    (return naturalCure `spread'` regenerator `spread'` intimidate)

intoValidation eff@(AddPercentageOfHP _ _) =
  liftEffect eff AlwaysTrue

intoValidation eff@(CauseDamage _) = do
  let sitrusBerry = targetHeldItemIs (Just SitrusBerry) `And` targetHpLessThan 0.5
  sitrusBerry' <- sitrusBerry `enrichValidation` AddPercentageOfHP Target 0.25
  liftEffect eff AlwaysTrue `chain`
    (return sitrusBerry' `spread` intoValidation (PreventOHKO Target))

intoValidation eff@(CauseRecoil _ _) = do
  let sitrusBerry = userHeldItemIs (Just SitrusBerry) `And` userHpLessThan 0.5
  sitrusBerry' <- sitrusBerry `enrichValidation` AddPercentageOfHP User 0.25
  liftEffect eff (Not $ userAbilityIs RockHead) `chain`
    (return sitrusBerry' `spread` intoValidation (PreventOHKO User))

intoValidation eff@(PreventOHKO Target) =
  liftEffect eff $ sturdy' `Or`
  (targetHpIsFull `And` targetHeldItemIs (Just FocusSash))

intoValidation eff@(PreventOHKO User) =
  liftEffect eff $ sturdy `Or`
  (userHpIsFull `And` userHeldItemIs (Just FocusSash))

intoValidation eff@(DropItem _) =
  liftEffect eff AlwaysTrue

intoValidation CauseLeechSeeded =
  liftEffect CauseLeechSeeded (Not $ targetHasTypeOf Grass)


-- 

itemIsValidated :: EffectValidation -> [EffectValidation]
itemIsValidated vldtn = foldr (\v m -> case v of
                                  ForCounterparty _ (HeldItemIs _)  -> Fact v : m
                                  _ -> m) [] vldtn

-- validations2dropItems :: [EffectValidation] -> [UnparsedSideEffect]
-- validations2dropItems =
  -- map (flip UnparsedSideEffect (DropItem))
