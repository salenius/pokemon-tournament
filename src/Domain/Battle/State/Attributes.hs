module Domain.Battle.State.Attributes where

import Domain.Attribute.Player
import Domain.Attribute.Ailment
import Domain.Attribute.HP
import Domain.Attribute.Screen
import Domain.Attribute.HeldItem
import Domain.Attribute.TypeOf
import Domain.Attribute.ModifStat
import Domain.Battle.Algebra
import Domain.Attribute.Quadruple
import Domain.Attribute.Ability
import Data.Map (Map, fromList, lookup)

data BattleAttributes = BattleAttributes
  {
    _player1 :: PlayerState
  , _player2 :: PlayerState
  }

data PlayerState = PlayerState
  {
    _currentPokemonInUse :: PokemonState
  , _restOfPokemonParty :: [PokemonState]
  , _playerWideAttributes :: PlayerAttributes
  }

data PlayerAttributes = PlayerAttributes
  {
    _playerLightScreen :: LightScreen
  , _playerReflect :: Reflect
  , _playerSafeguard :: Safeguard
  } deriving (Eq,Show)

data PokemonState = PokemonState
  {
    _pokemonSpecies :: PokemonSpecies
  , _pokemonHp :: HP
  , _pokemonAttributes :: PokemonAttributes
  , _pokemonStatus :: PokemonStatus
  }

data PokemonAttributes = PokemonAttributes
  {
    _pokemonTempTypeOf :: Maybe [TypeOf]
  , _pokemonTempAbility :: Maybe Ability
  , _pokemonMoves :: Quadruple MoveState
  , _pokemonMoveLocked :: Maybe MoveId
  , _pokemonMoveCharging :: Maybe MoveId
  }

data MoveState = MoveState
  {
    _moveId :: MoveId
  , _ppLeft :: Int
  , _moveTempTypeOf :: Maybe TypeOf
  }

data PokemonStatus = PokemonStatus
  {
    _pokemonAilment :: StatusAilment
  , _pokemonStatModification :: Map ModifStat Int
  , _pokemonFlinched :: Flinched
  , _pokemonProtected :: Protected
  , _pokemonLeechSeeded :: LeechSeeded
  , _pokemonYawned :: Yawned
  }
