module Domain.Match.MoveSucceeds where

import Domain.Match.Validation
import Domain.Attribute.Counterparty
import Domain.Attribute.Weather
import Domain.Attribute.HeldItem

data SuccessType =
  NormallyExecuted
  | ChargingMove
  | MustAttackSecond
  | OpponentChoseStrikeMove
  | DecliningSuccess SuccessType
  | NormalExecutionIf Validation SuccessType
  deriving (Eq,Show)

powerHerb = User `heldItemIs` (\x -> x == Just PowerHerb)
