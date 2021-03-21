{-# LANGUAGE KindSignatures, TemplateHaskell, MultiParamTypeClasses,DeriveFunctor #-}

module Domain.Algebra.Effect where

import Control.Lens
import Control.Applicative

data Effect a =
  Chain (Effect a) (Effect a)
  | Spread (Effect a) (Effect a)
  | Alter (Effect a) (Effect a)
  | Effect a
  | DoNothing
  deriving (Eq,Show,Functor)

makePrisms ''Effect

chain :: Effect a -> Effect a -> Effect a
chain a b = a `Chain` b

spread :: Effect a -> Effect a -> Effect a
spread a b = a `Spread` b

effect' :: a -> Effect a
effect' v = reduceEffect $ Effect v

alter :: Effect a -> Effect a -> Effect a
alter a b = a `Alter` b

doNothing :: Effect a
doNothing = reduceEffect DoNothing

reduceEffect :: Effect a -> Effect a
reduceEffect (Chain DoNothing _) = DoNothing
reduceEffect (Chain x DoNothing) = reduceEffect x
reduceEffect (Chain x y) = reduceEffect $ Chain (reduceEffect x) (reduceEffect y)
reduceEffect (Spread x DoNothing) = reduceEffect x
reduceEffect (Spread DoNothing x) = reduceEffect x
reduceEffect (Spread x y) = reduceEffect $ Spread (reduceEffect x) (reduceEffect y)
reduceEffect (Alter x DoNothing) = reduceEffect x
reduceEffect (Alter DoNothing x) = reduceEffect x
reduceEffect (Alter x _) = reduceEffect x
reduceEffect x = x

instance Applicative Effect where
  pure a = Effect a
  DoNothing <*> _ = DoNothing
  _ <*> DoNothing = DoNothing
  Effect f <*> x = reduceEffect $ f <$> x
  Chain f g <*> x = chain (reduceEffect $ f <*> x) (reduceEffect $ g <*> x)
  Spread f g <*> x = spread (reduceEffect $ f <*> x) (reduceEffect $ g <*> x)
  Alter f g <*> x = alter (reduceEffect $ f <*> x) (reduceEffect $ g <*> x)

instance Monad Effect where
  DoNothing >>= _ = DoNothing
  Effect x >>= f = reduceEffect $ f x
  Spread x y >>= f = spread (x >>= f') (y >>= f')
    where
      f' = reduceEffect . f
  Alter x y >>= f = alter (x >>= f') (y >>= f')
    where
      f' = reduceEffect . f
  Chain x y >>= f = chain (x >>= f') (y >>= f')
    where
      f' = reduceEffect . f

instance Semigroup (Effect a) where
  (<>) = spread

instance Monoid (Effect a) where
  mempty = doNothing

instance Alternative Effect where
  empty = DoNothing
  (<|>) = alter
