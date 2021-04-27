module Domain.Battle.Op where

data MoveOp m b = MoveOp
  {
    strike :: b -> m b
  , choiceScore :: m Int
  } 

