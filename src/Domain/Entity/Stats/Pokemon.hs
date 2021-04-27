{-# LANGUAGE DeriveFoldable #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE UndecidableInstances #-}


module Domain.Entity.Stats.Pokemon where

import Domain.Attribute.Statistic
import Domain.Attribute.Physiology
import Domain.Attribute.Ability
import Domain.Attribute.HeldItem
import Domain.Attribute.Quadruple
import Domain.Entity.BuiltIn.Move
import Domain.Attribute.TypeOf
import Domain.Attribute.Gender

class PokemonStat pkmn where
  baseStat :: BaseStat -> pkmn -> Int
  baseHp :: pkmn -> Int
  baseHp = baseStat BaseHP
  baseAttack :: pkmn -> Int
  baseAttack = baseStat BaseAttack
  baseDefence :: pkmn -> Int
  baseDefence = baseStat BaseDefence
  baseSAttack :: pkmn -> Int
  baseSAttack = baseStat BaseSAttack
  baseSDefence :: pkmn -> Int
  baseSDefence = baseStat BaseSDefence
  baseSpeed :: pkmn -> Int
  baseSpeed = baseStat BaseSpeed

statCompare :: PokemonStat p => p -> p -> Ordering
statCompare a b = compare (f a) (f b)
    where
      f p = foldr (+) 0 . map (flip baseStat p) $ stats
      stats :: [BaseStat]
      stats = enumFrom BaseHP

class PokemonPhysiology pkmn where
  weight :: pkmn -> Kilograms
  height :: pkmn -> Meters

class PokemonAttribute pkmn where
  abilityIs :: pkmn -> Ability
  typeIs :: pkmn -> TypeOfPokemon
  genderIs :: pkmn -> Gender

class PokemonIndividual pkmn where
  moves :: pkmn -> Quadruple Move
  heldItem :: pkmn -> HeldItem
  

data ListOf2 a =
  ListOf1 a
  | ListOf2 a a
  deriving (Eq,Read,Functor,Foldable)

instance Show a => Show (ListOf2 a) where
  show (ListOf1 x) = show $ [x]
  show (ListOf2 x y) = show $ [x,y]

type TypeOfPokemon = ListOf2 TypeOf

typeOfToList :: TypeOfPokemon -> [TypeOf]
typeOfToList (ListOf1 x) = [x]
typeOfToList (ListOf2 x y) = [x,y]
