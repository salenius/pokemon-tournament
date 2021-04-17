{-# LANGUAGE TemplateHaskell #-}

module Domain.Pokemon.Individual (
  Pokemon(..),
  pokemonSpecies,
  pokemonHeldItem,
  pokemonMoves,
  pokemonFactors,
  Factors(..),
  defaultFactors,
  pokemonStatistic
                                 ) where

import Domain.Pokemon.Statistic
import Domain.Pokemon.Species
import Domain.Attribute.Statistic
import Domain.Attribute.HeldItem
import Domain.Attribute.Nature
import Domain.Attribute.PokemonFactors
import Domain.Attribute.Quadruple
import Data.Bifunctor
import Control.Lens

data Pokemon pkmn mv = Pokemon
  {
    _pokemonSpecies :: pkmn
  , _pokemonHeldItem :: Maybe HeldItem
  , _pokemonMoves :: Quadruple mv
  , _pokemonFactors :: Factors 
  }


data Factors = Factors
  {
    _levelOf :: Level'
  , _ivOf :: BaseStat -> Int
  , _evOf :: BaseStat -> Int
  , _natureOf :: Nature
  }

makeLenses ''Pokemon
makeLenses ''Factors

instance Bifunctor Pokemon where
  bimap f g = over pokemonSpecies f . over pokemonMoves (fmap g)

instance AsPokemonSpecies pkmn => AsPokemonSpecies (Pokemon pkmn mv) where
  asSpecies pkmn = asSpecies $ view pokemonSpecies pkmn

pokemonStatistic :: AsPokemonSpecies pkmn => BaseStat -> Pokemon pkmn mv -> Int
pokemonStatistic stat pkmn = statistic stat (asStatFactor pkmn)

asStatFactor :: AsPokemonSpecies pkmn => Pokemon pkmn mv -> StatisticFactors
asStatFactor pkmn =
  StatisticFactors (view (pokemonFactors . levelOf) pkmn) pkmnf (view (pokemonFactors . ivOf) pkmn) (view (pokemonFactors . evOf) pkmn) (view (pokemonFactors . natureOf) pkmn)
  where
    pkmnf BaseHP = view baseHp pkmn'
    pkmnf BaseAttack = view baseAttack pkmn'
    pkmnf BaseDefence = view baseDefence pkmn'
    pkmnf BaseSAttack = view baseSAttack pkmn'
    pkmnf BaseSDefence = view baseSDefence pkmn'
    pkmnf BaseSpeed = view baseSpeed pkmn'
    pkmn' = asSpecies pkmn

defaultFactors :: Factors
defaultFactors = Factors 50 f g Bold
  where
    f _ = 31
    g _ = 85
