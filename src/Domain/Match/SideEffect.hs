module Domain.Match.SideEffect (
  SideEffect,
  SideEffectType(..),
  HPAdding(..),
  Screen(..),
  addPercentOfHP,
  addAmountOfHP,
  equalizeHPs,
  causeAilment,
  causeFlinch,
  causeRecoil,
  dropStat,
  increaseStat,
  causeLeechSeeded,
  dropItem,
  switchPokemon,
  putLightScreen,
  removeLightScreen,
  putReflect,
  removeReflect,
  protectUser,
  yawnTarget,
  drainDamage,
  changeWeather,
  causeConfusion,
  thawOut,
  withProbability,
  forRounds,
  giveValidation
                               ) where

import Domain.Attribute.Ailment
import Domain.Attribute.Ability
import Domain.Attribute.HeldItem
import Domain.Attribute.Counterparty
import Domain.Attribute.ModifStat
import Domain.Attribute.Weather
import Domain.Attribute.TypeOf

import Domain.Match.Validation
import Domain.Match.BattleEffect

data HPAdding =
  PercentOf Double
  | Amount Int
  | Equalize
  deriving (Eq,Show)

data Screen =
  LightScreen
  | Reflect
  deriving (Eq,Show)

data SideEffectType =
   DoNothing
  | CauseAilment Counterparty Ailment
  | ThawOut Counterparty
  | CauseFlinch
  | DropStat Counterparty ModifStat Int
  | IncreaseStat Counterparty ModifStat Int
  | DropItem Counterparty
  | CauseLeechSeeded
  | CauseConfusion Counterparty
  | ChangeWeather Weather
  | SwitchPokemon Counterparty
  | AddHP Counterparty HPAdding
  | CauseRecoil Double
  | DrainDamage Double
  | PutScreen Screen Counterparty
  | RemoveScreen Screen Counterparty
  | ProtectUser
  | YawnTarget
  deriving (Eq,Show)

instance Semigroup SideEffectType where
  a <> DoNothing = a
  DoNothing <> a = a
  a <> b = a

instance Monoid SideEffectType where
  mempty = DoNothing

type SideEffect' a = BattleEffect SideEffectType a

type SideEffect = SideEffect' Validation

addHP :: Counterparty -> HPAdding -> SideEffect' a
addHP cp add = Apply $ AddHP cp add

addPercentOfHP cp pct = addHP cp (PercentOf pct)
addAmountOfHP cp amt = addHP cp (Amount amt)
equalizeHPs = addHP User Equalize

causeAilment :: Counterparty -> Ailment -> SideEffect' a
causeAilment cp a = Apply $ CauseAilment cp a

causeFlinch :: SideEffect' a
causeFlinch = Apply CauseFlinch

causeRecoil :: Double -> SideEffect' a
causeRecoil d = Apply $ CauseRecoil d

dropStat :: Counterparty -> ModifStat -> Int -> SideEffect' a
dropStat cp stat level = Apply $ DropStat cp stat level

increaseStat :: Counterparty -> ModifStat -> Int -> SideEffect' a
increaseStat cp stat level = Apply $ IncreaseStat cp stat level

changeWeather :: Weather -> SideEffect' a
changeWeather = Apply . ChangeWeather

causeConfusion :: Counterparty -> SideEffect' a
causeConfusion = Apply . CauseConfusion

causeLeechSeeded :: SideEffect' a
causeLeechSeeded = Apply CauseLeechSeeded

dropItem :: Counterparty -> SideEffect' a
dropItem = Apply . DropItem

switchPokemon :: Counterparty -> SideEffect' a
switchPokemon = Apply . SwitchPokemon

putLightScreen :: Counterparty -> SideEffect' a
putLightScreen = Apply . PutScreen LightScreen

putReflect :: Counterparty -> SideEffect' a
putReflect  = Apply . PutScreen Reflect

removeLightScreen :: Counterparty -> SideEffect' a
removeLightScreen = Apply . RemoveScreen LightScreen

removeReflect :: Counterparty -> SideEffect' a
removeReflect  = Apply . RemoveScreen Reflect

thawOut :: Counterparty -> SideEffect' a
thawOut = Apply . ThawOut

drainDamage :: Double -> SideEffect' a
drainDamage d = Apply $ DrainDamage d

protectUser :: SideEffect' a
protectUser = Apply $ ProtectUser

yawnTarget :: SideEffect' a
yawnTarget = Apply $ YawnTarget

withProbability :: Double -> SideEffect' a -> SideEffect' a
withProbability d x = WithProbability d x

forRounds :: Int -> SideEffect' a -> SideEffect' a
forRounds = ForRounds

---

makeConf :: SideEffectType -> Validation
makeConf DoNothing = AlwaysTrue
makeConf (CauseAilment Target Poisoned) =
  Target `typeIs` (\t -> Poison `notElem` t && Steel `notElem` t) `And`
  (moldBreaker `Or` abilityIs Target (\ab -> ab /= Immunity)) `And`
  (moldBreaker `Or`
   abilityIs Target (\ab -> ab /= LeafGuard) `Or`
   weatherIs (\w -> w /= Sunny))
makeConf CauseFlinch =
  Not (notMoldBreaker `And` innerFocus)
  where
    innerFocus = abilityIs Target (\ab -> ab == InnerFocus)

giveValidation :: SideEffect -> SideEffect
giveValidation (Apply a) = ValidateEffect (makeConf a) (Apply a)
giveValidation (Chain a b) = Chain (giveValidation a) (giveValidation b)
giveValidation (ForRounds d x) = ForRounds d (giveValidation x)
giveValidation (WithProbability i x) = WithProbability i (giveValidation x)
giveValidation (ValidateEffect v x) = ValidateEffect v (giveValidation x)
giveValidation (Spread a b) = Spread (giveValidation a) (giveValidation b)

