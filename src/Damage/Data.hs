module Damage.Data where

newtype Damage =
  Damage { damageToDouble :: Double } deriving (Eq,Show,Ord)

instance Semigroup Damage where
  Damage a <> Damage b = Damage $ a * b

instance Monoid Damage where
  mempty = Damage 0
