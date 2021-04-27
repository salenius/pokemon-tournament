{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Domain.Attribute.Physiology where

newtype Kilograms = Kilograms {kilogramsToDouble :: Double} deriving (Eq,Show,Num)

newtype Meters = Meters {metersToDouble :: Double} deriving (Eq,Show,Num)
