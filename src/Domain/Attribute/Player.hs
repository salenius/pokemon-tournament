module Domain.Attribute.Player where

data Player = Player1 | Player2 deriving (Eq,Show)

opponent :: Player -> Player
opponent Player1 = Player2
opponent Player2 = Player1
