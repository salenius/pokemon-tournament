{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}

module Effect.Logic.AilmentDecision where

import Effect.Data
import Attribute.Counterparty
import Attribute.Ability
import Attribute.TypeOf
import Effect.Logic.AilmentParse
import Effect.Logic.AilmentData

class AilmentDecision a where
  decide :: a -> Effect AilmentEffect

instance AilmentDecision TargetPoisoned where
  decide (MoldBreakerStops _) = pure (Target, Poison')
  decide (SynchronizeKicks (_,a)) = pure (Target, Poison') `chain` decide a
  decide Basecase = pure (Target, Poison')
  decide _ = doNothing

instance AilmentDecision UserPoisoned where
  decide Basecase = pure (User, Poison')
  decide _ = doNothing

instance AilmentDecision TargetBurned where
  decide (MoldBreakerStops _) = pure (Target, Burn')
  decide (SynchronizeKicks (_,a)) = pure (Target, Burn') `chain` decide a
  decide Basecase = pure (Target, Burn')
  decide _ = doNothing

instance AilmentDecision UserBurned where
  decide Basecase = pure (User, Burn')
  decide _ = doNothing

instance AilmentDecision TargetParalyzed where
  decide (MoldBreakerStops _) = pure (Target, Paralysis')
  decide (SynchronizeKicks (_,a)) = pure (Target, Paralysis') `chain` decide a
  decide Basecase = pure (Target, Paralysis')
  decide _ = doNothing

instance AilmentDecision UserParalyzed where
  decide Basecase = pure (User, Paralysis')
  decide _ = doNothing

instance AilmentDecision TargetFrozen where
  decide (MoldBreakerStops _) = pure (Target, Freeze')
  decide Basecase = pure (Target, Freeze')
  decide _ = doNothing

instance AilmentDecision UserFrozen where
  decide Basecase = pure (User, Freeze')
  decide _ = doNothing

instance AilmentDecision TargetSleep where
  decide (MoldBreakerStops _) = pure (Target, Sleep')
  decide Basecase = pure (Target, Sleep')
  decide _ = doNothing

instance AilmentDecision UserSleep where
  decide Basecase = pure (User, Sleep')
  decide _ = doNothing
 
instance AilmentDecision UserSleepSelfInflicted where
  decide Basecase = pure (User, Sleep')
  decide (AlreadyAilment _) = pure (User, Sleep')
  decide _ = doNothing

instance AilmentDecision AilmentChoice where
  decide (UserBurned x) = decide x
  decide (TargetBurned x) = decide x
  decide (UserPoisoned x) = decide x
  decide (TargetPoisoned x) = decide x
  decide (UserParalyzed x) = decide x
  decide (TargetParalyzed x) = decide x
  decide (UserFrozen x) = decide x
  decide (TargetFrozen x) = decide x
  decide (UserSleep x) = decide x
  decide (TargetSleep x) = decide x
  decide (UserSleepSelfInflicted x) = decide x
  
  

data AilmentCondition ability moldb syncr typeimm alr =
  AbilityBlocks (Ability' ability)
  | MoldBreakerStops moldb
  | SynchronizeKicks syncr
  | TypeImmunity typeimm
  | AlreadyAilment alr
  | SafeguardBlocks Safeguard
  | Basecase
  deriving (Eq,Show,Ord)

type TargetAilment ab tp syncr =
  AilmentCondition ab (MoldBreaker' ab) (Synchronizing syncr) (TargetHas tp) (TargetHas SomeAilment)

type UserAilment ab tp =
  AilmentCondition ab () () tp (UserHas SomeAilment)

type TargetPoisoned = TargetAilment Immunity PoisonOrSteelType UserPoisoned

type TargetBurned = TargetAilment WaterVeil_ FireType UserBurned

type TargetParalyzed = TargetAilment Limber ElectricType UserParalyzed

type UserParalyzed = UserAilment Limber ElectricType

type UserPoisoned = UserAilment Immunity PoisonOrSteelType

type UserBurned = UserAilment WaterVeil_ FireType

type TargetFrozen = TargetAilment MagmaArmor IceType ()

type TargetSleep = TargetAilment Insomnia_ () ()

type UserFrozen = UserAilment MagmaArmor IceType

type UserSleep = UserAilment Insomnia_ ()

type UserSleepSelfInflicted = AilmentCondition Insomnia_ () () () ()

data AilmentChoice =
  TargetBurned TargetBurned
  | UserBurned UserBurned
  | TargetPoisoned TargetPoisoned
  | UserPoisoned UserPoisoned
  | TargetParalyzed TargetParalyzed
  | UserParalyzed UserParalyzed
  | TargetFrozen TargetFrozen
  | UserFrozen UserFrozen
  | TargetSleep TargetSleep
  | UserSleep UserSleep
  | UserSleepSelfInflicted UserSleepSelfInflicted
  deriving (Eq,Show,Ord)


type AilmentEffect = (Counterparty, SomeAilment)
