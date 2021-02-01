{-# LANGUAGE TemplateHaskell #-}

module Domain.Data.MoveState where

import Domain.Attribute.Id
import Control.Lens

data MoveState b = MoveState
  {
    _moveId :: MoveId
  , _currentPP :: Int
  , action :: b ()
  }

makeLenses ''MoveState

instance Show (MoveState b) where
  show m = (show . view moveId $ m) ++ ", PP: " ++ (show $ view currentPP m)

instance Eq (MoveState b) where
  (==) m1 m2 = _currentPP m1 == _currentPP m2
  
