module Domain.Algebra.EffectOnCounterparty where

import Data.Bifunctor
import Domain.Attribute.Counterparty

data EffectOnCounterparty onUser onTarget =
  EffectOnUser onUser
  | EffectOnTarget onTarget
  deriving (Eq,Show)

effectOn :: Counterparty -> a -> EffectOnCounterparty a a
effectOn User v = EffectOnUser v
effectOn Target v = EffectOnTarget v

instance Bifunctor EffectOnCounterparty where
  bimap f _ (EffectOnUser x) = EffectOnUser $ f x
  bimap _ g (EffectOnTarget x) = EffectOnTarget $ g x
