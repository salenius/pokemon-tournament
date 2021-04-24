{-# LANGUAGE TemplateHaskell #-}

module Domain.Entity.BuiltIn.Move (
  module Constr,
  mkMove,
  Move(),
  MakeMove
                                  ) where

import qualified Domain.Entity.BuiltIn.Move.Attacking as Att
import qualified Domain.Entity.BuiltIn.Move.Status as Stat
import Domain.Entity.BuiltIn.Move.Constructors as Constr
import Control.Lens

data Move =
  StatusMove Stat.Move
  | DamagingMove Att.Move
  deriving (Eq,Show,Read,Ord)

makePrisms ''Move

_PhysicalMove :: Prism' Move Att.PhysicalMove
_PhysicalMove = _DamagingMove . Att._Physical

_SpecialMove :: Prism' Move Att.SpecialMove
_SpecialMove = _DamagingMove . Att._Special

_DirectDamageMove :: Prism' Move Att.DirectDamageMove
_DirectDamageMove = _DamagingMove . Att._DirectDamage

class MakeMove mv where
  mkMove :: mv -> Move

instance MakeMove Stat.Move where
  mkMove = review _StatusMove

instance MakeMove Att.SpecialMove where
  mkMove = review _SpecialMove

instance MakeMove Att.PhysicalMove where
  mkMove = review _PhysicalMove

instance MakeMove Att.DirectDamageMove where
  mkMove = review _DirectDamageMove
