{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE ViewPatterns #-}


module Entity.Move.Algebra where

import Types.BuiltIn
import Types.Effect
import qualified Types.BuiltIn as Tp
import Attribute.Category
import Attribute.Ailment
import Attribute.Screen
import Attribute.Counterparty
import Attribute.Charging
import Attribute.HP
import Attribute.Weather
import Control.Lens
import Stats.Base
import Domain.Common
import Domain.SideEffect
import Domain.Logic

--- This algebra contains all basic attributes
class Monad m => MoveBasic m where
  pp :: Int -> m ()
  typeOf :: TypeOf -> m ()
  normal :: m ()
  normal = typeOf Tp.Normal
  fighting :: m ()
  fighting = typeOf Fighting
  flying :: m ()
  flying = typeOf Flying
  water :: m ()
  water = typeOf Water
  fire :: m ()
  fire = typeOf Fire
  grass :: m ()
  grass = typeOf Grass
  electric :: m ()
  electric = typeOf Electric
  bug :: m ()
  bug = typeOf Bug
  poisonType :: m ()
  poisonType = typeOf Poison
  rock :: m ()
  rock = typeOf Rock
  ground :: m ()
  ground = typeOf Ground
  steel :: m ()
  steel = typeOf Steel
  ice :: m ()
  ice = typeOf Ice
  dark :: m ()
  dark = typeOf Dark
  ghost :: m ()
  ghost = typeOf Ghost
  psychicType :: m ()
  psychicType = typeOf Psychic
  dragon :: m ()
  dragon = typeOf Dragon
  fairy :: m ()
  fairy = typeOf Fairy
  category :: MoveCategory -> m ()
  physical :: m ()
  physical = category $ review (_DamagingCategory . _PhysicalMove) Physical
  special :: m ()
  special = category $ review (_DamagingCategory . _SpecialMove) Special
  status :: m ()
  status = category $ review _StatusMove Status
  basepower :: Int -> m ()
  accuracy :: Double -> m ()
  alwaysHits :: m ()
  contact :: MakesContact -> m ()
  priority :: Int -> m ()


--- This is used if move has varying basepower under certain conditions
class (MoveBasic m, BattleStrikeGetter b) => BasepowerModification m b | m -> b where
  multiplyBy :: Double -> m () -> b Bool -> m ()
  multiplyBy i = modifyBy (\x v -> if x then v * i else v)
  doubleIf :: m () -> b Bool -> m ()
  doubleIf = multiplyBy 2
  modifyBy :: (a -> Double -> Double) -> m () -> b a -> m ()

--- Basic side effects
class (SideEffect b,
       MoveBasic m) => EffectWhenHits m b | m -> b where
  putEffect :: b Action -> m ()
  (%) :: Double -> m () -> m ()
  --- Non mandatory
  causeAilment :: Ailment -> Counterparty -> m ()
  causeAilment a cp = putEffect $ case (a,cp) of
    (isn't _Healthy -> False, User) -> healUser
    (isn't _Healthy -> False, Target) -> healTarget
    (isn't _Poisoned -> False, User) -> poisonUser
    (isn't _Poisoned -> False, Target) -> poisonTarget
    (isn't _Burned -> False, User) -> burnUser
    (isn't _Burned -> False, Target) -> burnTarget
    (isn't _Paralyzed -> False, User) -> paralyzeUser
    (isn't _Paralyzed -> False, Target) -> paralyzeTarget
    (isn't _Frozen -> False, User) -> freezeUser
    (isn't _Frozen -> False, Target) -> freezeTarget
    (isn't _Sleep -> False, User) -> sleepUserByItself
    _ -> sleepTarget
  heal :: Counterparty -> m ()
  heal = causeAilment $ review _Healthy Healthy'
  poison :: Counterparty -> m ()
  poison = causeAilment $ review _Poisoned Poisoned'
  burn :: Counterparty -> m ()
  burn = causeAilment $ review _Burned Burned'
  paralyze :: Counterparty -> m ()
  paralyze = causeAilment $ review _Paralyzed Paralyzed'
  freeze :: Counterparty -> m ()
  freeze = causeAilment $ review _Frozen Frozen'
  sleep :: Counterparty -> m ()
  sleep = causeAilment $ review _Sleep Sleep'
  recoil :: Double -> m ()
  recoil pct = putEffect $ putRecoil pct
  flinch :: Counterparty -> m ()
  flinch User = return ()
  flinch Target = putEffect flinchTarget
  switch :: Counterparty -> m ()
  switch User = putEffect switchUser
  switch Target = putEffect switchTarget
  lower :: Counterparty -> ModifStat -> Int -> m ()
  lower User stat lvl = putEffect $ lowerUserStatByUser stat lvl
  lower Target stat lvl = putEffect $ lowerTargetStat stat lvl
  raise :: Counterparty -> ModifStat -> Int -> m ()
  raise User stat lvl = putEffect $ increaseUserStat stat lvl
  raise Target stat lvl = putEffect $ increaseTargetStat stat lvl
  confuse :: Counterparty -> m ()
  confuse User = return ()
  confuse Target = putEffect confuseTarget
  changeWeather :: Weather -> m ()
  screen :: OnOrOff -> Counterparty -> Screen -> m ()
  screen On User LightScreen = putEffect setLightScreenForUser
  screen On User Reflect = putEffect setReflectForUser
  screen _ _ _ = return ()
  setUp :: Counterparty -> Screen -> m ()
  setUp = screen On
  break :: Counterparty -> Screen -> m ()
  break = screen Off
  leechSeeded :: Counterparty -> m ()
  leechSeeded Target = putEffect leechSeedTarget
  leechSeeded _  = return ()
  putHp :: Counterparty -> (HP -> HP) -> m ()
  drainDamage :: (Double -> Double) -> m ()
  protect' :: Counterparty -> m ()
  yawn' :: Counterparty -> m ()

infixl 3 %
infixl 3 `exceptIf`

--- Nonstandard damage calculation facilities
class (MoveBasic m, BattleStrikeGetter b) => ModifyDamage m b | m -> b where
  exceptIf :: m () -> b Bool -> m ()
  increasedCrit :: Int -> m ()
  editTypeAdvantage :: (TypeOf, TypeEffect) -> m () 
  editStab :: Double -> m ()
  constantDamage :: b Double -> m ()
  tweekRatio :: (Counterparty, ModifStat) -> (Counterparty, ModifStat) -> m ()
  chargeMove :: Int -> m ()
  lockMove :: m ()
  multiHit :: [(Int, Double)] -> m ()
  turnCountOnBattle :: (Int -> Bool) -> m ()
  bypassesAccuracyCheckIf :: b Bool -> m ()
  thaw :: Counterparty -> m ()
  ohko :: (Double -> b Double) -> m ()


data MakesContact = Doesn'tMakeContact | MakesContact deriving (Eq,Show,Ord,Enum)

data OnOrOff = Off | On deriving (Eq,Show,Ord,Enum)

instance Bounded MakesContact where
  minBound = Doesn'tMakeContact
  maxBound = MakesContact

makes :: (Monad m, Bounded a) => (a -> m ()) -> m ()
makes f = f minBound

start :: EffectWhenHits m b => Weather -> m ()
start = changeWeather

no :: (Monad m, Bounded a) => ((a -> m ()) -> m ()) -> ((a -> m ()) -> m ())
no f = \g -> f g

infixl 5 `no`

user = User

target = Target

attack = Attack'
defence = Defence'
sattack = SAttack'
sdefence = SDefence'
speed = Speed'
pokemonAccuracy = Accuracy'
evasion = Evasion'

rain = Rainy
sunlight = Sunny
hail = Hail
sandStorm = Sandstorm

lightScreen' = LightScreen
reflect' = Reflect

-----

-- type ReadBattle a = forall m. (BattleStrikeGetter m) => m a
