{-# LANGUAGE TemplateHaskell #-}

module Domain.Damage.Stab where

import Domain.Pokemon.Species
import Domain.Attribute.TypeOf
import Domain.Attribute.Damage
import Control.Lens

data Stab battle = Stab
  {
    _typeOfUser :: battle -> TypeOfPokemon
  , _typeOfMove :: battle -> TypeOf
  , _stabFromMove :: Bool -> Double
  }

makeLenses ''Stab

reduce :: Stab battle -> battle -> Damage
reduce stab btl =
  let
    ptp = getTypeOfPokemon $ view typeOfUser stab btl
    mtp = view typeOfMove stab btl
    isIn = mtp `elem` ptp
    in Damage $ view stabFromMove stab isIn
