{-# LANGUAGE TemplateHaskell #-}

module Domain.Move.StrikingMove where

import Domain.Move.BuiltIn
import Domain.Move.DamageMove
import Domain.Move.StatusMove
import Control.Lens
import Control.Applicative
import Data.Maybe

data StrikingMove status damage directdamage =
  StatusMove status
  | DamageMove damage
  | DirectDamageMove directdamage
  deriving (Eq,Show)

makePrisms ''StrikingMove

type StrikingMoveBuiltIn =
  StrikingMove StatusMoveBuiltIn DamageMoveBuiltIn DirectDamageMoveBuiltIn

strikingMove :: Iso' BuiltInMove StrikingMoveBuiltIn
strikingMove = iso f g
  where
    f mv = fromMaybe (DamageMove Struggle') $
      StatusMove <$> preview _StatusMoveBuiltIn mv <|>
      DamageMove <$> preview _DamageMoveBuiltIn mv <|>
      DirectDamageMove <$> preview _DirectDamageMoveBuiltIn mv
    g (StatusMove mv) = review _StatusMoveBuiltIn mv
    g (DamageMove mv) = review _DamageMoveBuiltIn mv
    g (DirectDamageMove mv) = review _DirectDamageMoveBuiltIn mv

fromStrikingMove :: StrikingMoveBuiltIn -> BuiltInMove
fromStrikingMove = review strikingMove

toStrikingMove :: BuiltInMove -> Maybe StrikingMoveBuiltIn
toStrikingMove = preview strikingMove
