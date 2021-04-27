{-# LANGUAGE RankNTypes #-}

module Domain.Battle.State where

import Domain.Battle.Id
import Domain.Battle.State.Attributes
import Domain.Attribute.Player
import Domain.Attribute.HeldItem
import Domain.Attribute.MoveExecution
import Domain.Attribute.Weather


data BattleState =
  OngoingBattle CurrentBattle
  | EndedBattle { winner :: Player }
  deriving (Eq,Show)

data CurrentBattle = CurrentBattle
  {
    _whichTurn :: Int
  , _phaseOfBattle :: BattlePhase
  , _currentWeather :: WeatherStatus
  } deriving (Eq,Show)


data BattlePhase =
  MakeChoices (PlayersInBattle (PlayerState PokemonState))
  | DetermineFaster FasterDetermination
  | StrikeFirst FirstStrike
  | StrikeSecond SecondStrike
  | EndRound (PlayersInBattle PlayerInBattle)
  | GameOver Player
  deriving (Eq,Show)

data MoveChoice =
  PickedMove MoveId
  | UsedItem HeldItem
  | SwitchedPokemon
  deriving (Eq,Show)

data FasterDetermination = FasterDetermination
  {
    _playersInDetermination :: PlayersInBattle (PlayerState PokemonState)
  , _choicesInDetermination :: PlayersInBattle MoveChoice
  } deriving (Eq,Show)

data FirstStrike = FirstStrike
  {
    _firstStrikePlayers :: CounterpartiesInStrike
  , _firstStrikeChoicesMade :: PlayersInBattle MoveChoice
  } deriving (Eq,Show)

data SecondStrike = SecondStrike
  {
    _secondStrikePlayers :: CounterpartiesInStrike
  , _secondStrikeChoicesMade :: PlayersInBattle MoveChoice
  , _secondStrikePreviousStrike :: MoveExecution
  } deriving (Eq,Show)

