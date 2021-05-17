module Entity.Move.Algebra where

import Types.BuiltIn
import Attribute.Category
import Attribute.Ailment
import Attribute.Counterparty
import Control.Lens
import Stats.Base

class Monad m => MoveBasic m where
  pp :: Int -> m ()
  typeOf :: TypeOf -> m ()
  normal :: m ()
  normal = typeOf Normal
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
  contact :: MakesContact -> m ()
  priority :: Int -> m ()

class MoveBasic m => TweekProbabilities m where
  (%) :: Double -> m () -> m ()

class (MoveBasic m, TweekProbabilities m) => EffectWhenHits m where
  causeAilment :: Ailment -> Counterparty -> m ()
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

infixl 3 %

data MakesContact = Doesn'tMakeContact | MakesContact deriving (Eq,Show,Ord,Enum)

instance Bounded MakesContact where
  minBound = Doesn'tMakeContact
  maxBound = MakesContact

makes :: (Monad m, Bounded a) => (a -> m ()) -> m ()
makes f = f minBound

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
