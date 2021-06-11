{-# LANGUAGE DeriveFoldable #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DeriveTraversable #-}

module Types.Pokemon (
  module BI
  ,PokemonType
  ,pokemonTypeToList
  ,ListOf2(..)
                     ) where

import Types.BuiltIn as BI

type PokemonType = ListOf2 TypeOf

data ListOf2 a =
  ListOf1 a
  | ListOf2 a a
  deriving (Eq,Show,Foldable,Functor,Traversable,Ord)

pokemonTypeToList :: PokemonType -> [TypeOf]
pokemonTypeToList (ListOf1 x) = [x]
pokemonTypeToList (ListOf2 x y) = [x,y]
