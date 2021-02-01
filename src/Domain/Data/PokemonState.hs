{-# LANGUAGE TemplateHaskell #-}

module Domain.Data.PokemonState where

import qualified Data.Map as Map
import Control.Lens
import Domain.Data.MoveState
import Domain.Attribute.Ailment
import Domain.Attribute.Statistic as S
import Domain.Attribute.TypeOf
import Domain.Attribute.Ability
import Domain.Attribute.Id
import Domain.Attribute.HeldItem
import Domain.Attribute.ModifStat
import Domain.Attribute.Gender
import Domain.Attribute.Nature
import Domain.Attribute.PokemonFactors

data PokemonStatistic = PokemonStatistic
  {
    _attack :: Statistic
  , _defence :: Statistic
  , _sAttack :: Statistic
  , _sDefence :: Statistic
  , _speed :: Statistic
  , _weight :: Double
  } deriving (Eq,Show)

makeLenses ''PokemonStatistic

data PokemonIndividual = PokemonIndividual
  {
    _pokemonTypeOf :: [TypeOf]
  , _pokemonAbility :: Ability
  , _pokemonHeldItem :: Maybe HeldItem
  , _pokemonGender :: Gender
  , _pokemonLevel :: S.Level
  , _pokemonNature :: Nature
  , _pokemonIvs :: IVs
  , _pokemonEvs :: EVs
  } deriving (Eq,Show)

makeLenses ''PokemonIndividual

data PokemonModification = PokemonModification
  {
    _pokemonIsAlive :: Bool
  , _pokemonAilment :: Ailment
  , _pokemonStatModification :: Map.Map ModifStat Int
  , _pokemonFlinched :: Bool
  , _pokemonLeechSeeded :: Bool
  , _pokemonYawned :: Bool
  , _pokemonCharging :: Int
  } deriving (Eq,Show)

makeLenses ''PokemonModification

data PokemonState a = PokemonState
  {
    _pokemonId :: PokemonId
  , _statistic :: PokemonStatistic
  , _individual :: PokemonIndividual
  , _modification :: PokemonModification
  , _moves :: [MoveState a]
  } deriving (Show,Eq)

makeLenses ''PokemonState

