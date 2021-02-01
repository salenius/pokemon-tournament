{-# LANGUAGE TemplateHaskell, RankNTypes #-}

module Domain.Entity.Battle where

import Control.Lens
import Domain.Attribute.Weather as W
import Domain.Attribute.TypeOf as Tp
import Domain.Attribute.Player
import Domain.Attribute.HeldItem
import Domain.Attribute.HP
import Domain.Attribute.Screen
import Domain.Attribute.MoveExecution
import Domain.Entity.Move
import Domain.Entity.Pokemon as P
import Domain.Entity.Pokemon.Species as PS
import Domain.Data.Trainer as T
import Domain.Entity.Trainer
import Domain.Entity.PokemonState

-- Data types

type Trainer' = Trainer (Pokemon Move)

data WhichChoice =
  MoveForStrike Move
  | SwitchAnotherPokemon
  | ItemUsed HeldItem
  deriving (Show)

data ChoicesMade = ChoicesMade
  {
    _choiceByPlayer1 :: WhichChoice
  , _choiceByPlayer2 :: WhichChoice
  } deriving (Show)

makeLenses ''ChoicesMade

data PokemonInBattle =
  AbleToBattle PokemonState
  | UnableToBattle
  deriving (Eq,Show)

makePrisms ''PokemonInBattle

data TemporaryWeather' = TemporaryWeather'
  {
    _currentWeather :: Weather
  , _weatherRounds :: Int
  } deriving (Eq,Show)

makeLenses ''TemporaryWeather'

data WeatherState =
  PermanentWeather Weather
  | TemporaryWeather TemporaryWeather'
   deriving (Eq,Show)

makePrisms ''WeatherState

{-------------------
 Player related
--------------------}

data PlayerCondition = PlayerCondition
  {
    _lightScreen :: LightScreen
  , _reflect :: Reflect
  } deriving (Eq,Show)

makeLenses ''PlayerCondition

defaultPlayerCondition :: PlayerCondition
defaultPlayerCondition =
  PlayerCondition NoLightScreen NoReflect

data PlayerState = PlayerState
  {
    _currentPokemon :: PokemonInBattle
  , _pokemonParty :: [PokemonInBattle]
  , _playerCondition :: PlayerCondition
  } 

makeLenses ''PlayerState

currentAlivePokemon = currentPokemon . _AbleToBattle

modifyCurrentPokemon :: (PokemonState -> PokemonState) -> (PlayerState -> PlayerState)
modifyCurrentPokemon pkmnfn plr =
  over (currentPokemon . _AbleToBattle) pkmnfn plr

switchPokemon :: (PokemonState -> Bool) -> PlayerState -> PlayerState
switchPokemon pkmnBool plr =
  let cur = view currentPokemon plr
      safeHead [] = Nothing
      safeHead (x:_) = Just x
      nxtPkmn = safeHead . filter pkmnBool' . view pokemonParty $ plr
      pkmnBool' UnableToBattle = False
      pkmnBool' (AbleToBattle x) = pkmnBool x
  in case nxtPkmn of
    Nothing -> plr
    Just pkmn -> over pokemonParty ((:) cur) . set currentPokemon pkmn $ plr

putLightScreen :: Int -> PlayerState -> PlayerState
putLightScreen v = set (playerCondition . lightScreen) (LightScreen v)

putReflect :: Int -> PlayerState -> PlayerState
putReflect v = set (playerCondition . reflect) (Reflect v)

removeLightScreen :: PlayerState -> PlayerState
removeLightScreen = set (playerCondition . lightScreen) NoLightScreen

removeReflect :: PlayerState -> PlayerState
removeReflect = set (playerCondition . reflect) NoReflect


instance Show PlayerState where
  show b =
    "PlayerState {_currentPokemon = " ++
    (f $ preview (currentAlivePokemon . pokemonIndividual . P.species . PS.speciesName) b) ++
    -- ", hpAmount = " ++ (show $ preview (currentAlivePokemon . hpAmount) b) ++
    -- ", ailment = " ++ (show $ preview (currentAlivePokemon . currentStatus . ailment) b) ++
    ", restOfParty = " ++ (show . map f . map g $ (view pokemonParty b)) ++
    "}"
    where
      g = (\p -> preview (_AbleToBattle . pokemonIndividual . P.species . PS.speciesName) p)
      f (Just a) = show a
      f Nothing = "X"


data PlayerInBattle =
  StillFighting PlayerState
  | HasLost
  deriving (Show)

makePrisms ''PlayerInBattle

newPlayer :: (Trainer' -> [PokemonState]) -> Trainer' -> PlayerInBattle
newPlayer tf trainer
  | tf trainer == [] = HasLost
  | otherwise =
    StillFighting $ PlayerState (AbleToBattle . head $ tf trainer) (map AbleToBattle . tail $ tf trainer) defaultPlayerCondition

{-------------------------
  Different stage related
--------------------------}

data StrikingTurn =
  FirstStrike
  | SecondStrike MoveExecution
  deriving (Eq,Show)

data StrikingPhase' = StrikingPhase'
  {
    _strikingPlayer :: Player
  , _whichChoices :: ChoicesMade
  , _strikingTurn :: StrikingTurn
  } deriving (Show)

makeLenses ''StrikingPhase'

data BattleStage =
  MakeChoices
  | DetermineFaster ChoicesMade
  | StrikingPhase StrikingPhase'
  | EndRound
  deriving (Show)

makePrisms ''BattleStage

data BattleState = BattleState
  {
    _currentRound :: Int
  , _player1 :: PlayerInBattle
  , _player2 :: PlayerInBattle
  , _weather :: WeatherState
  } deriving (Show)

makeLenses ''BattleState

currentWeatherState :: Lens' WeatherState W.Weather
currentWeatherState = lens getter setter
  where
    getter (PermanentWeather w) = w
    getter (TemporaryWeather w) = view currentWeather w
    setter x _ = x

data OnGoingBattle' = OnGoingBattle'
  {
    _battleStage :: BattleStage
  , _battleState :: BattleState
  } deriving (Show)

makeLenses ''OnGoingBattle'

data BattleEnded' = BattleEnded'
  {
    _winner :: Player
  } deriving (Eq,Show)

data Battle =
  OnGoingBattle OnGoingBattle'
  | BattleEnded BattleEnded'
  deriving (Show)

makePrisms ''Battle

player Player1 = _OnGoingBattle . battleState . player1 . _StillFighting
player Player2 = _OnGoingBattle . battleState . player2 . _StillFighting

user :: Traversal' Battle Player
user = _OnGoingBattle . battleStage . _StrikingPhase . strikingPlayer 

modifyTurnPlayer :: (Player -> Player) -> (PlayerState -> PlayerState) -> Battle -> Battle
modifyTurnPlayer f g battle = case b' of
  Just b -> b
  Nothing -> battle
  where
    b' = do
      usr <- preview user battle
      let usr' = f usr
      let plr = player usr'
      return $ over plr g battle

verifyTurnPlayer :: (Player -> Player) -> (PlayerState -> Bool) -> Battle -> Bool
verifyTurnPlayer f bool battle = case b' of
  Just b -> b
  Nothing -> False
  where
    b' = do
      usr <- preview user battle
      let plr = player . f $ usr
      pl <- preview plr battle
      return . bool $ pl

{-
modifyUser = modifyTurnPlayer id
modifyTarget = modifyTurnPlayer f
  where
    f Player1 = Player2
    f Player2 = Player1

verifyUser = verifyTurnPlayer id
verifyTarget = verifyTurnPlayer f
  where
    f Player1 = Player2
    f Player2 = Player1
-}

newBattle
  ::
    (Trainer' -> [Pokemon Move]) ->
    Trainer' ->
    Trainer' ->
    Battle
newBattle partyCnstr t1 t2 =
  let partyfn = fmap newPokemonState . partyCnstr in
    OnGoingBattle $ OnGoingBattle' MakeChoices $
    BattleState 1 (newPlayer partyfn t1) (newPlayer partyfn t2) (PermanentWeather W.Normal)

playWithAll = newBattle (foldr (:) [] . trainerParty)
