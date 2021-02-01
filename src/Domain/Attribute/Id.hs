module Domain.Attribute.Id where

newtype MoveId = MoveId String deriving (Eq)

instance Show MoveId where
  show (MoveId s) = show s

newtype PokemonId = PokemonId String deriving (Eq)

instance Show PokemonId where
  show (PokemonId s) = show s
