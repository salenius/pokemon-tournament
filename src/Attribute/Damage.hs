module Attribute.Damage where

newtype Damage = Damage { damageToDouble :: Double } deriving (Eq,Show,Ord)
