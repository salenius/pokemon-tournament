module Battle.Data where

import Attribute.MoveExecution
import Attribute.Counterparty
import Attribute.Weather
import Control.Monad.RWS
import Control.Monad.Reader
import Battle.Player
import Battle.Pokemon

data BattleState =
  OngoingBattle CurrentBattle
  | EndedBattle { winner :: Player }

data CurrentBattle = CurrentBattle
  {
    _whichTurn :: Int
  , _phaseOfBattle :: BattlePhase
  , _currentWeather :: Weather
  }

data BattlePhase =
  MakeChoices (PlayersInBattle (ReadonlyPlayer MoveToPick))
  | DetermineFaster FasterDetermination
  | StrikeFirst FirstStrike
  | StrikeSecond SecondStrike
  | EndRound (PlayersInBattle (VulnerablePlayer MoveToPick))
  | GameOver Player

type FasterDetermination = (PlayersInBattle (ReadonlyPlayer MoveToPick), Choices)

type FirstStrike = StrikeData () (VulnerablePlayer MoveToPick)

type SecondStrike = StrikeData MoveExecution (VulnerablePlayer MoveToPick)

data StrikeData prev plr = StrikeData
  {
    _strikeParties :: PlayersInBattle plr
  , _strikeUser :: Player
  , _strikeChoices :: Choices
  , _previousMove :: prev
  } deriving (Eq,Show)

data PlayersInBattle plr = PlayersInBattle
  {
    _player1 :: plr
  , _player2 :: plr
  } deriving (Eq,Show)

data StrikePhase a = StrikePhase
  {
    _strikingPlayer :: Player
  }

data MoveToPick = MoveToPick
  {
    _ppLeft :: Int
  , _moveChoiceAlgorithm
    :: ReaderT BattleState IO Choice'
  , _moveStrikeAlgorithm
    :: RWST (Maybe MoveExecution, Choices) () BattleState IO MoveExecution
  }

type Choices = (Choice', Choice')

type Choice' = ()
