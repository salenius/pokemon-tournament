{-# LANGUAGE TemplateHaskell, FlexibleInstances #-}

module Domain.Battle.State.Attributes where

import Domain.Attribute.Player
import Domain.Attribute.Ailment
import Domain.Attribute.HP
import Domain.Attribute.Screen
import Domain.Attribute.HeldItem
import Domain.Attribute.TypeOf
import Domain.Attribute.ModifStat
import Domain.Battle.Id
import Domain.Battle.State.Pokemon
import Domain.Attribute.Quadruple
import Domain.Attribute.Ability
import Data.Map (Map, fromList, lookup)
import Control.Lens

data PlayersInBattle plr = PlayersInBattle
  {
    _player1 :: plr
  , _player2 :: plr
  } deriving (Eq,Show)

data PlayerInBattle =
  StillFighting (PlayerState PokemonInBattle)
  | Defeated
  deriving (Eq,Show)

data CounterpartiesInStrike = CounterpartiesInStrike
  {
    _currentUser :: PlayerInBattle
  , _currentTarget :: PlayerInBattle
  } deriving (Eq,Show)

data PlayerState pkmn = PlayerState
  {
    _currentPokemonInUse :: pkmn
  , _restOfPokemonParty :: [PokemonInBattle]
  , _playerAttributesAll :: PlayerAttributes
  } deriving (Eq,Show)

data PlayerAttributes = PlayerAttributes
  {
    _playerLightScreen :: LightScreen
  , _playerReflect :: Reflect
  , _playerSafeguard :: Safeguard
  } deriving (Eq,Show)

data PokemonInBattle =
  AlivePokemon PokemonState
  | UnableToBattle
  deriving (Eq,Show)

data PokemonState = PokemonState
  {
    _pokemonSpecies :: PokemonSpecies
  , _pokemonCurrentHp :: HP
  , _pokemonAttributesAll :: PokemonAttributes
  , _pokemonStatusAll :: PokemonStatus
  } deriving (Eq,Show)

data PokemonAttributes = PokemonAttributes
  {
    _pokemonTempTypeOf :: Maybe [TypeOf]
  , _pokemonTempAbility :: Maybe Ability
  , _pokemonMoves :: Quadruple MoveState
  } deriving (Eq,Show)

data MoveState = MoveState
  {
    _moveId :: MoveId
  , _ppLeft :: Int
  , _moveTempTypeOf :: Maybe TypeOf
  } deriving (Eq,Show)

data PokemonStatus = PokemonStatus
  {
    _pokemonAilment :: StatusAilment
  , _pokemonStatModification :: Map ModifStat Int
  , _pokemonFlinched :: Flinched
  , _pokemonProtected :: Protected
  , _pokemonLeechSeeded :: LeechSeeded
  , _pokemonYawned :: Yawned
  , _pokemonMoveLocked :: Maybe MoveId
  , _pokemonMoveCharging :: Maybe MoveId
  } deriving (Eq,Show)

makeClassy ''PokemonStatus
makeClassy ''MoveState
makeClassy ''PokemonAttributes
makeClassy ''PokemonState
makeLenses ''PlayerAttributes
makeLenses ''PlayerState

instance Functor PlayerState where
  fmap f plr = plr {_currentPokemonInUse = f $ _currentPokemonInUse plr}

instance Functor PlayersInBattle where
  fmap f btl = btl {_player1 = f $ _player1 btl, _player2 = f $ _player2 btl}

instance Semigroup m => Semigroup (PlayerState m) where
  plr1 <> plr2 =
    over currentPokemonInUse ((<>) (view currentPokemonInUse plr1)) .
    over restOfPokemonParty ((<>) (view restOfPokemonParty plr1)) .
    over playerAttributesAll ((<>) (view playerAttributesAll plr1)) $
    plr2

instance Monoid m => Monoid (PlayerState m) where
  mempty = PlayerState mempty mempty mempty

instance Semigroup PlayerAttributes where
  plr1 <> plr2 =
    over playerLightScreen ((<>) (view playerLightScreen plr1)) .
    over playerReflect ((<>) (view playerReflect plr1)) .
    over playerSafeguard ((<>) (view playerSafeguard plr1)) $
    plr2

instance Monoid PlayerAttributes where
  mempty = PlayerAttributes mempty mempty mempty

instance HasPokemonStatus PokemonState where
  pokemonStatus = pokemonStatusAll

instance HasPokemonAttributes PokemonState where
  pokemonAttributes = pokemonAttributesAll

instance HasPokemonState (PlayerState PokemonState) where
  pokemonState = currentPokemonInUse

