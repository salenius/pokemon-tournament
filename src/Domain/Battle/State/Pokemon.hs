{-# LANGUAGE TemplateHaskell #-}


module Domain.Battle.State.Pokemon where

import Domain.Entity.Pokemon.Statistic
import Domain.Attribute.Statistic
import Domain.Entity.Stats.Pokemon
import Domain.Attribute.Gender
import Domain.Attribute.Ability
import Domain.Attribute.TypeOf
import Domain.Attribute.HeldItem
import Domain.Attribute.Physiology
import Domain.Entity.Pokemon
import Control.Lens
import Data.Map (Map,fromList)

data PokemonSpecies = PokemonSpecies
  {
    _pokemonStats :: BaseStat -> Statistic
  , _pokemonLevelOf :: Int
  , _pokemonActualAbility :: Ability
  , _pokemonActualHeldItem :: Maybe HeldItem
  , _pokemonActualTypeOf :: [TypeOf]
  , _pokemonGender :: Gender
  , _pokemonWeight :: Double
  }

makeLenses ''PokemonSpecies

mkPokemonSpecies :: (PokemonStat pkmn, PokemonPhysiology pkmn, PokemonAttribute pkmn, PokemonIndividual pkmn) => Pokemon pkmn -> PokemonSpecies
mkPokemonSpecies pkmn = PokemonSpecies
  {
    _pokemonStats = flip pokemonStatistic pkmn
  , _pokemonLevelOf = view pokemonLevel pkmn
  , _pokemonActualAbility = abilityIs $ view pokemonSpecies pkmn
  , _pokemonActualHeldItem = Just $ heldItem $ view pokemonSpecies pkmn
  , _pokemonActualTypeOf = typeOfToList . typeIs $ view pokemonSpecies pkmn
  , _pokemonGender = genderIs $ view pokemonSpecies pkmn
  , _pokemonWeight = kilogramsToDouble $ weight $ view pokemonSpecies pkmn
  }

data PokemonSpecies' = PokemonSpecies'
  {
    pokemonStats' :: Map BaseStat Statistic
  , pokemonLevelOf' :: Int
  , pokemonActualAbility' :: Ability
  , pokemonActualHeldItem' :: Maybe HeldItem
  , pokemonActualTypeOf' :: [TypeOf]
  , pokemonGender' :: Gender
  , pokemonWeight' :: Double
  } deriving (Eq,Show)

speciesToSpecies' :: PokemonSpecies -> PokemonSpecies'
speciesToSpecies' sp = PokemonSpecies' x a b c d e f
  where
    x = fromList $ zip (enumFrom BaseHP) (map (view pokemonStats sp) (enumFrom BaseHP))
    a = view pokemonLevelOf sp
    b = view pokemonActualAbility sp
    c = view pokemonActualHeldItem sp
    d = view pokemonActualTypeOf sp
    e = view pokemonGender sp
    f = view pokemonWeight sp

instance Show PokemonSpecies where
  show = show . speciesToSpecies'

instance Eq PokemonSpecies where
  a == b = speciesToSpecies' a == speciesToSpecies' b
