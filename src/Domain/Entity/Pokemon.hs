{-# LANGUAGE TemplateHaskell, RankNTypes, GADTs, ViewPatterns, PatternSynonyms #-}

module Domain.Entity.Pokemon where

import Domain.Attribute.Ability
import Domain.Attribute.Statistic
import Domain.Attribute.HeldItem
import Domain.Attribute.Gender
import Domain.Attribute.Nature
import Domain.Attribute.Quadruple
import Domain.Attribute.PokemonFactors as PF
import Domain.Entity.Stats.Pokemon
import Domain.Entity.BuiltIn.Pokemon.Species
import Control.Lens

data Pokemon pkmn mv = MkPokemon
  {
    _species :: pkmn
  , _possibleHeldItem :: HeldItem
  , _moves :: Quadruple mv
  , _pokemonLevel :: Level'
  , _pokemonNature :: Nature
  , _pokemonEvs :: EVs
  , _pokemonIvs :: IVs
  } deriving (Eq,Show,Read)

makeLenses ''Pokemon

instance PokemonStat pkmn => PokemonStat (Pokemon pkmn mv) where
  baseStat bs p = baseStat bs $ view species p
  
---


-- Tilastojen laskeminen

getIv :: BaseStat -> Pokemon p m -> Int
getIv base p = view (pokemonIvs . l base) p
  where
    l BaseHP = hpIv
    l BaseAttack = attackIv
    l BaseDefence = defenceIv
    l BaseSAttack = sAttackIv
    l BaseSDefence = sDefenceIv
    l BaseSpeed = speedIv

getEv :: BaseStat -> Pokemon p m -> Int
getEv base p = view (pokemonEvs . l base) p
  where
    l BaseHP = hpEv
    l BaseAttack = attackEv
    l BaseDefence = defenceEv
    l BaseSAttack = sAttackEv
    l BaseSDefence = sDefenceEv
    l BaseSpeed = speedEv

newStat :: BaseStat -> Pokemon p m -> Int
newStat basestat pkmn =
  statisticCalc basestat (baseStat basestat pkmn) (view pokemonLevel pkmn) (view pokemonNature pkmn) (getIv basestat pkmn) $
  (getEv basestat pkmn)
