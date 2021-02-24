{-# LANGUAGE TemplateHaskell, PatternSynonyms, ViewPatterns #-}

module Domain.Battle.Logic where

import Domain.Attribute.Counterparty
import Domain.Attribute.Ailment
import Domain.Attribute.Ability
import Domain.Attribute.HP
import Domain.Attribute.MoveExecution
import Domain.Attribute.ModifStat
import Domain.Attribute.Player
import Domain.Attribute.TypeOf
import Domain.Attribute.HeldItem
import Domain.Attribute.Weather
import Control.Monad.Reader
import Control.Applicative
import Control.Lens
import qualified Data.Map as Map

data Fact a =
  Fact a
  | Or (Fact a) (Fact a)
  | And (Fact a) (Fact a)
  | Not (Fact a)
  | AlwaysTrue
  deriving (Eq,Show)

instance Functor Fact where
  fmap f (Fact a) = Fact (f a)
  fmap f (Not a) = Not (fmap f a)
  fmap f (Or a b) = Or (fmap f a) (fmap f b)
  fmap f (And a b) = And (fmap f a) (fmap f b)
  fmap _ AlwaysTrue = AlwaysTrue

instance Foldable Fact where
  foldr f m (Fact a) = f a m
  foldr f m (Not a) = foldr f m a
  foldr f m (And a b) = foldr f (foldr f m b) a
  foldr f m (Or a b) = foldr f (foldr f m b) a
  foldr _ m AlwaysTrue = m

data StrikeFact =
  PlayerFactIs PlayerFact
  | NextPokemonFactIs PlayerFact
  | BattleFactIs BattleFact
  | RandomDoubleLT Double
  deriving (Eq,Show)

data PlayerFact = PlayerFact
  {
    _counterparty :: Counterparty
  , _simpleFact :: SimpleFact
  } deriving (Eq,Show)

data SimpleFact =
  AbilityIs Ability
  | HeldItemIs (Maybe HeldItem)
  | HasTypeOf TypeOf
  | AilmentIs Ailment
  | HPIsLT Double
  | HPIsGT Double
  | HPIsLTE Double
  | HPIsGTE Double
  deriving (Eq,Show)

data BattleFact =
  WeatherIs Weather
  | PreviousStrikeIs (Maybe MoveExecution)
  deriving (Eq,Show)

makeLenses ''SimpleFact
makePrisms ''BattleFact
makeLenses ''PlayerFact
makePrisms ''StrikeFact

pattern ForCounterparty cp fact = PlayerFactIs (PlayerFact cp fact)
pattern ForUser fact = ForCounterparty User fact
pattern ForTarget fact = PlayerFactIs (PlayerFact Target fact)

userFact = Fact . PlayerFactIs . PlayerFact User
targetFact = Fact . PlayerFactIs . PlayerFact Target
userAbilityIs = userFact . AbilityIs 
targetAbilityIs = targetFact . AbilityIs
userHeldItemIs = userFact . HeldItemIs
targetHeldItemIs = targetFact . HeldItemIs
userHasTypeOf = userFact . HasTypeOf
targetHasTypeOf = targetFact . HasTypeOf
userAilmentIs = userFact . AilmentIs
targetAilmentIs = targetFact . AilmentIs
weatherIs = Fact . BattleFactIs . WeatherIs
userHpLessThan = userFact . HPIsLT
targetHpLessThan = userFact . HPIsLT
userHpIsFull = userFact $ HPIsGTE 1.0
targetHpIsFull = userFact $ HPIsGTE 1.0

player2nextPokemon :: Fact StrikeFact -> Fact StrikeFact
player2nextPokemon fact = fmap f fact
  where
    f (PlayerFactIs g) = NextPokemonFactIs g
    f x = x

changeCounterparty :: Counterparty -> Fact StrikeFact -> Fact StrikeFact
changeCounterparty cp fact = fmap f fact
  where
    f = set (_PlayerFactIs . counterparty) cp

intoTarget = changeCounterparty Target

data BattleCondition = BattleCondition
  {
    _weather :: Weather
  } deriving (Eq,Show)

data PokemonStatus = PokemonStatus
  {
    _ailment :: Ailment
  , _statModifications :: Map.Map ModifStat Int
  , _poisonMultiplier :: Int
  , _flinched :: Flinched
  , _confused :: Confused
  , _leechSeeded :: LeechSeeded
  , _protected :: Protected
  , _yawned :: Yawned
  } deriving (Eq,Show)

data PokemonAttributes = PokemonAttributes
  {
    _typeOfPokemon :: [TypeOf]
  , _ability :: Ability
  , _heldItem :: Maybe HeldItem
  } deriving (Eq,Show)

data PlayerCondition = PlayerConditon
  {
    _lightScreen :: Maybe Int
  , _reflect :: Maybe Int
  } deriving (Eq,Show)

data PokemonInfo = PokemonInfo
  {
    _hpAmount :: HP
  , _status :: PokemonStatus
  , _attributes :: PokemonAttributes
  } deriving (Eq,Show)

data PlayerInfo = PlayerInfo
  {
    _pokemon :: PokemonInfo
  , _plrCondition :: PlayerCondition
  } deriving (Eq,Show)

data StrikeInfo = StrikeInfo
  {
    _user :: PlayerInfo
  , _target :: PlayerInfo
  , _strCondition :: BattleCondition
  } deriving (Eq,Show)

data BattleInfo = BattleInfo
  {
    _player1 :: PlayerInfo
  , _player2 :: PlayerInfo
  , _btlCondition :: BattleCondition
  } deriving (Eq,Show)

data SwitchInfo = SwitchInfo
  {
    _switchingPlayer :: PlayerInfo
  , _stayingPlayer :: PlayerInfo
  , _swtCondition :: BattleCondition
  } deriving (Eq,Show)

makeLenses ''PokemonStatus
makeLenses ''PokemonAttributes
makeLenses ''PlayerCondition
makeLenses ''PokemonInfo
makeLenses ''PlayerInfo
makeLenses ''BattleCondition
makeLenses ''StrikeInfo
makeLenses ''BattleInfo


type ReadStrike m a = ReaderT StrikeInfo m a
type ReadBattle m a = ReaderT BattleInfo m a


a `and'` b = (&&) <$> a <*> b

a `or'` b = (||) <$> a <*> b

not' a = not <$> a

equals x a = (==) x <$> a

memberOf :: (Eq a, Monad m) => a -> m [a] -> m Bool
memberOf y lst = elem y <$> lst
