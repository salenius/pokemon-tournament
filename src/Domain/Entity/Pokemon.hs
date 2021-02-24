{-# LANGUAGE TemplateHaskell, RankNTypes, GADTs #-}

module Domain.Entity.Pokemon where

import Domain.Entity.Pokemon.Species as PS
import Domain.Attribute.Ability
import Domain.Attribute.Statistic
import Domain.Attribute.HeldItem
import Domain.Attribute.Gender
import Domain.Attribute.Nature
import Domain.Attribute.PokemonFactors as PF
import Control.Lens

data Quadruple m =
  Monoruple m
  | Duoruple m m
  | Triruple m m m
  | Quadruple m m m m
  deriving (Eq,Read)

instance Show m => Show (Quadruple m) where
  show (Monoruple a) = show [a]
  show (Duoruple a b) = show [a,b]
  show (Triruple a b c) = show [a,b,c]
  show (Quadruple a b c d) = show [a,b,c,d]

instance Functor Quadruple where
  fmap f (Monoruple a) = Monoruple $ f a
  fmap f (Duoruple a b) = Duoruple (f a) $ f b
  fmap f (Triruple a b c) = Triruple (f a) (f b) $ f c
  fmap f (Quadruple a b c d) = Quadruple (f a) (f b) (f c) $ f d

data PokemonSpecies' =
  forall pkmn. PokemonSpecies pkmn => PokemonSpecies' { getSpecies :: pkmn}

instance Eq PokemonSpecies' where
  (PokemonSpecies' p) == (PokemonSpecies' p') = 
    baseHp p == baseHp p' &&
    baseAttack p == baseAttack p' &&
    baseDefence p == baseDefence p' &&
    baseSAttack p == baseSAttack p' &&
    baseSDefence p == baseSDefence p' &&
    baseSpeed p == baseSpeed p' &&
    weight p == weight p' &&
    typeOfPokemon p == typeOfPokemon p' &&
    genderRatio p == genderRatio p' &&
    (show $ revertToForm p) == (show $ revertToForm p') &&
    possibleAbilities p == possibleAbilities p' &&
    isPikachu p == isPikachu p'

instance Show PokemonSpecies' where
  show (PokemonSpecies' p) = show p

data Pokemon m = Pokemon
  {
    _species :: PokemonSpecies'
  , _possibleHeldItem :: HeldItem
  , _moves :: Quadruple m
  , _pokemonLevel :: Level'
  , _pokemonNature :: Nature
  , _pokemonEvs :: EVs
  , _pokemonIvs :: IVs
  } deriving (Eq,Show)

makeLenses ''Pokemon

instance PokemonSpecies PokemonSpecies' where
  baseHp (PokemonSpecies' p) = baseHp p
  baseAttack (PokemonSpecies' p) = baseAttack p
  baseDefence (PokemonSpecies' p) = baseDefence p
  baseSAttack (PokemonSpecies' p) = baseSAttack p
  baseSDefence (PokemonSpecies' p) = baseSDefence p
  baseSpeed (PokemonSpecies' p) = baseSpeed p
  weight (PokemonSpecies' p) = weight p
  typeOfPokemon (PokemonSpecies' p) = typeOfPokemon p
  genderRatio (PokemonSpecies' p) = genderRatio p
  possibleAbilities (PokemonSpecies' p) = possibleAbilities p
  revertToForm (PokemonSpecies' p) = PokemonSpecies' $ revertToForm p
  isPikachu (PokemonSpecies' p) = isPikachu p


-- Tilastojen laskeminen

getIv :: BaseStat -> Pokemon m -> Int
getIv base p = view (pokemonIvs . l base) p
  where
    l BaseHP = hpIv
    l BaseAttack = attackIv
    l BaseDefence = defenceIv
    l BaseSAttack = sAttackIv
    l BaseSDefence = sDefenceIv
    l BaseSpeed = speedIv

getEv :: BaseStat -> Pokemon m -> Int
getEv base p = view (pokemonEvs . l base) p
  where
    l BaseHP = hpEv
    l BaseAttack = attackEv
    l BaseDefence = defenceEv
    l BaseSAttack = sAttackEv
    l BaseSDefence = sDefenceEv
    l BaseSpeed = speedEv

getBaseStat :: BaseStat -> Pokemon m -> Int
-- getBaseStat = undefined
getBaseStat base = f . _species
  where
    f = case base of
      BaseHP -> baseHp
      BaseAttack -> baseAttack
      BaseDefence -> baseDefence
      BaseSAttack -> baseSAttack
      BaseSDefence -> baseSDefence
      BaseSpeed -> baseSpeed


newStat :: BaseStat -> Pokemon m -> Int
newStat basestat pkmn =
  statisticCalc basestat (getBaseStat basestat pkmn) (view pokemonLevel pkmn) (view pokemonNature pkmn) (getIv basestat pkmn) $
  (getEv basestat pkmn)

