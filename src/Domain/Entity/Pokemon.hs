{-# LANGUAGE TemplateHaskell, RankNTypes, GADTs, ViewPatterns, PatternSynonyms #-}

module Domain.Entity.Pokemon (
  Pokemon(..),
  pokemonSpecies,
  pokemonFactors,
  pokemonLevel,
  pokemonStatistic
                             ) where

import Domain.Attribute.Ability
import Domain.Attribute.Statistic
import Domain.Attribute.HeldItem
import Domain.Attribute.Gender
import Domain.Attribute.Nature
import Domain.Attribute.Quadruple
import Domain.Attribute.PokemonFactors as PF
import Domain.Entity.Stats.Pokemon
import Domain.Entity.Pokemon.Statistic as Pst
import Domain.Entity.BuiltIn.Pokemon
import Control.Lens

data Pokemon pkmn = Pokemon
  {
    _pokemonSpecies :: pkmn
  , _pokemonFactors :: PokemonFactors
  }



data PokemonFactors = PokemonFactors
  {
    _pokemonLevel' :: Level'
  , _pokemonIV :: StatValues
  , _pokemonEV :: StatValues
  , _pokemonNature :: Nature
  } deriving (Eq,Show)
  
data StatValues = StatValues
  {
    _hpValue :: Int
  , _attackValue :: Int
  , _defenceValue :: Int
  , _sAttackValue :: Int
  , _sDefenceValue :: Int
  , _speedValue :: Int
  } deriving (Eq,Show)


makeLenses ''StatValues
makeLenses ''PokemonFactors
makeLenses ''Pokemon

pokemonLevel :: Lens' (Pokemon pkmn) Level'
pokemonLevel = pokemonFactors . pokemonLevel'

pokemonStatistic :: PokemonStat pkmn => BaseStat -> Pokemon pkmn -> Statistic
pokemonStatistic stat pkmn = Pst.statistic stat (pokemonToStatisticFactors pkmn)

pokemonToStatisticFactors :: PokemonStat pkmn => Pokemon pkmn -> StatisticFactors
pokemonToStatisticFactors pkmn =
  StatisticFactors a b c d e
  where
    a = view pokemonLevel pkmn
    b = flip baseStat pkmn
    c = statValuesToFunction (view (pokemonFactors . pokemonIV) pkmn)
    d = statValuesToFunction (view (pokemonFactors . pokemonEV) pkmn)
    e = view (pokemonFactors . pokemonNature) pkmn

statValuesToFunction :: StatValues -> BaseStat -> Int
statValuesToFunction stats bs = case bs of
  BaseHP -> view hpValue stats
  BaseAttack -> view attackValue stats
  BaseDefence -> view defenceValue stats
  BaseSAttack -> view sAttackValue stats
  BaseSDefence -> view sDefenceValue stats
  BaseSpeed -> view speedValue stats

instance PokemonStat pkmn => PokemonStat (Pokemon pkmn) where
  baseStat bs p = baseStat bs $ view pokemonSpecies p
