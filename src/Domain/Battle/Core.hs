{-# LANGUAGE RankNTypes #-}


module Domain.Battle.Core where

import Domain.Battle.Op
import Domain.Battle.Id
import Domain.Battle.State
import Domain.Battle.Effect
import Data.Map (Map)

data Battle = Battle
  {
    _battleState :: BattleState
  , _battleOps :: BattleOps
  }

data BattleOps = BattleOps
  {
    _movesAvailable :: Map MoveId (MoveOp BattleEffect BattleState)
  , _effectInterpreter :: forall a. BattleEffect a -> IO a
  }

