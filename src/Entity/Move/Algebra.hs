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

class MoveBasic m => TweekProbabilities m where
  (%) :: Double -> m () -> m ()
  exceptIf :: m () -> m Bool -> m ()

class (MoveBasic m, BattleStrikeGetter m) => BasepowerModification m where
  multiplyBy :: Double -> m () -> m Bool -> m ()
  multiplyBy i = modifyBy (\x v -> if x then v * i else v)
  doubleIf :: m () -> m Bool -> m ()
  doubleIf = multiplyBy 2
  modifyBy :: (a -> Double -> Double) -> m () -> m a -> m ()

class (MoveBasic m, TweekProbabilities m) => EffectWhenHits m where
  causeAilment :: Ailment -> Counterparty -> m ()
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
  flinch :: Counterparty -> m ()
  switch :: Counterparty -> m ()
  lower :: Counterparty -> ModifStat -> Int -> m ()
  raise :: Counterparty -> ModifStat -> Int -> m ()
  confuse :: Counterparty -> m ()
  changeWeather :: Weather -> m ()
  screen :: OnOrOff -> Counterparty -> Screen -> m ()
  setUp :: Counterparty -> Screen -> m ()
  setUp = screen On
  break :: Counterparty -> Screen -> m ()
  break = screen Off
  leechSeeded :: Counterparty -> m ()
  putHp :: Counterparty -> (HP -> HP) -> m ()
  drainDamage :: (Double -> Double) -> m ()
  protect' :: Counterparty -> m ()
  yawn' :: Counterparty -> m ()

infixl 3 %
infixl 3 `exceptIf`

class (MoveBasic m, TweekProbabilities m) => ModifyDamage m where
  increasedCrit :: Int -> m ()
  editTypeAdvantage :: (TypeOf, TypeEffect) -> m () 
  editStab :: Double -> m ()
  constantDamage :: Double -> m ()
  tweekRatio :: (Counterparty, ModifStat) -> (Counterparty, ModifStat) -> m ()
  chargeMove :: Int -> m ()
  lockMove :: m ()
  multiHit :: [(Int, Double)] -> m ()
  turnCountOnBattle :: (Int -> Bool) -> m ()
  thaw :: Counterparty -> m ()
  ohko :: (Double -> Double) -> m ()


data MakesContact = Doesn'tMakeContact | MakesContact deriving (Eq,Show,Ord,Enum)

data OnOrOff = Off | On deriving (Eq,Show,Ord,Enum)

instance Bounded MakesContact where
  minBound = Doesn'tMakeContact
  maxBound = MakesContact

makes :: (Monad m, Bounded a) => (a -> m ()) -> m ()
makes f = f minBound

start :: EffectWhenHits m => Weather -> m ()
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
