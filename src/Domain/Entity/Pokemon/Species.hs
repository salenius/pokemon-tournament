{-# LANGUAGE TemplateHaskell #-}

module Domain.Entity.Pokemon.Species where

import Domain.Attribute.TypeOf
import Domain.Attribute.Gender
import Control.Lens

data PokemonSpecies = PokemonSpecies
  {
    _speciesName :: String
  , _typeOf :: [TypeOf]
  , _baseHp :: Int
  , _baseAttack :: Int
  , _baseDefence :: Int
  , _baseSAttack :: Int
  , _baseSDefence :: Int
  , _baseSpeed :: Int
  , _weight :: Double
  , _genderRatio :: Gender -> Double
  }

makeLenses ''PokemonSpecies

instance Eq PokemonSpecies where
  a == b =
    _speciesName a == _speciesName b &&
    _typeOf a == _typeOf b &&
    _baseHp a == _baseHp b &&
    _baseAttack a == _baseAttack b &&
    _baseDefence a == _baseDefence b &&
    _baseSAttack a == _baseSDefence b &&
    _baseSDefence a == _baseSDefence b &&
    _baseSpeed a == _baseSDefence b &&
    _weight a == _weight b &&
    map (_genderRatio a) g == map (_genderRatio b) g
    where
      g = [Male,Female,Genderless]

instance Show PokemonSpecies where
  show p =
    _speciesName p ++ "\n" ++
    showfn "Type" typeOf ++
    showfn "HP" baseHp ++
    showfn "Attack" baseAttack ++
    showfn "Defence" baseDefence ++
    showfn "Sp.Attack" baseSAttack ++
    showfn "Sp.Defence" baseSDefence ++
    showfn "Speed" baseSpeed ++
    showfn "Weight" weight ++
    "Gender ratio: " ++
    (show $ [(Male, _genderRatio p Male),(Female, _genderRatio p Female),(Genderless, _genderRatio p Genderless)])
    where
      showfn s a = s ++ ": " ++ (show $ view a p) ++ "\n"

male7to1 Male = 0.875
male7to1 Female = 0.125
male7to1 _ = 0

