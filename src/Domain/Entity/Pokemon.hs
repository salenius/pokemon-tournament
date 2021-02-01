{-# LANGUAGE TemplateHaskell #-}

module Domain.Entity.Pokemon where

import Domain.Entity.Pokemon.Species
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
  deriving (Eq)

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

data Pokemon m = Pokemon
  {
    _species :: PokemonSpecies
  , _possibleAbilities :: [Ability]
  , _possibleHeldItem :: HeldItem
  , _moves :: Quadruple m
  , _pokemonLevel :: Level'
  , _pokemonNature :: Nature
  , _pokemonEvs :: EVs
  , _pokemonIvs :: IVs
  } deriving (Eq)

makeLenses ''Pokemon

instance Functor Pokemon where
  fmap f pkmn = over moves (fmap f) pkmn

instance Show m => Show (Pokemon m) where
  show p = view (species . speciesName) p ++
    " {" ++ "level = " ++ (show $ view pokemonLevel p) ++
    ", ability = " ++ (show $ view possibleAbilities p) ++
    ", item = " ++ (show $ view possibleHeldItem p) ++
    ", moves = " ++ (show $ view moves p) ++
    "}"

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
getBaseStat base p = view (species . l base) p
  where
    l BaseHP = baseHp
    l BaseAttack = baseAttack
    l BaseDefence = baseDefence
    l BaseSAttack = baseSAttack
    l BaseSDefence = baseSDefence
    l BaseSpeed = baseSpeed

newStat :: BaseStat -> Pokemon m -> Int
newStat basestat pkmn =
  statisticCalc basestat (getBaseStat basestat pkmn) (view pokemonLevel pkmn) (view pokemonNature pkmn) (getIv basestat pkmn) $
  (getEv basestat pkmn)
