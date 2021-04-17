module Domain.Pokemon.TypeOf where

import Domain.Attribute.TypeOf

newtype TypeOfPokemon =
  TypeOfPokemon [TypeOf] deriving (Eq,Show)

getTypeOfPokemon :: TypeOfPokemon -> [TypeOf]
getTypeOfPokemon (TypeOfPokemon t) = t
