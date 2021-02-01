module Domain.Attribute.Damage where

newtype Damage = Damage {fromDamage :: Double} deriving (Eq,Show)

instance Semigroup Damage where
  (<>) a b = Damage . (*) (fromDamage a) $ fromDamage b

instance Monoid Damage where
  mempty = Damage 0
  mappend a b = a <> b
