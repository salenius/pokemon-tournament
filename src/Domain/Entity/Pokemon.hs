{-# LANGUAGE TemplateHaskell, RankNTypes, GADTs, ViewPatterns, PatternSynonyms #-}

module Domain.Entity.Pokemon where

import Domain.Entity.Pokemon.Species as PS
import Domain.Attribute.Ability
import Domain.Attribute.Statistic
import Domain.Attribute.HeldItem
import Domain.Attribute.Gender
import Domain.Attribute.Nature
import Domain.Attribute.PokemonFactors as PF
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

makePrisms ''Quadruple
makeLenses ''Pokemon

instance PokemonSpecies pkmn => PokemonSpecies (Pokemon pkmn mv) where
  baseStat bs p = baseStat bs $ view species p
  ability = ability . view species
  typeOfPokemon = typeOfPokemon . view species
  genderRatio = genderRatio . view species
  weight = weight . view species
  
data Red'sPokemon mv where
  Red'sPikachu :: Pikachu -> Red'sPokemon mv
  Red'sLapras :: Lapras -> Red'sPokemon mv
  Red'sSnorlax :: Snorlax -> Red'sPokemon mv
  Red'sVenusaur :: Venusaur -> Red'sPokemon mv
  Red'sCharizard :: Charizard -> Red'sPokemon mv
  Red'sBlastoise :: Blastoise -> Red'sBlastoise mv

---


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
getBaseStat base = view (species . f)
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



instance Functor Quadruple where
  fmap f (Monoruple a) = Monoruple $ f a
  fmap f (Duoruple a b) = Duoruple (f a) $ f b
  fmap f (Triruple a b c) = Triruple (f a) (f b) $ f c
  fmap f (Quadruple a b c d) = Quadruple (f a) (f b) (f c) $ f d
