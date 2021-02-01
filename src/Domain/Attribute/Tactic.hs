module Domain.Attribute.Tactic where

import Domain.Attribute.HeldItem

data Tactic m p =
  MoveChosen m
  | HeldItemUsed HeldItem
  | PokemonSwitched p
  deriving (Show)

data ChosenTactics m p = ChosenTactics
  {
    _player1Tactic :: Tactic m p
  , _player2Tactic :: Tactic m p
  } deriving (Show)
