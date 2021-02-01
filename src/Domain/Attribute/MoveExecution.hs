module Domain.Attribute.MoveExecution where

import Domain.Attribute.Damage

data MoveExecution = MoveExecution
  {
    damageCaused :: Damage
  , recoilCaused :: Damage
  , isSuccesful :: Bool
  , hitsTarget :: Bool
  , charges :: Bool
  } | NotExecuted deriving (Eq,Show)


