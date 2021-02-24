module Domain.Effect where

import Control.Applicative

data Effect a =
  Chain (Effect a) (Effect a)
  | Spread (Effect a) (Effect a)
  | Apply a
  | DoNothing
  deriving (Eq,Show)

reduceEffect :: Effect a -> Effect a
reduceEffect (Chain DoNothing _) = DoNothing
reduceEffect (Chain x DoNothing) = reduceEffect x
reduceEffect (Spread x DoNothing) = reduceEffect x
reduceEffect (Spread DoNothing x) = reduceEffect x
reduceEffect x = x

chain :: Effect a -> Effect a -> Effect a
chain x = reduceEffect . Chain x

spread :: Effect a -> Effect a -> Effect a
spread x = reduceEffect . Spread x

apply :: a -> Effect a
apply = Apply

doNothing :: Effect a
doNothing = DoNothing

instance Functor Effect where
  fmap f (Chain a b) = chain (fmap f a) $ fmap f b
  fmap f (Spread a b) = spread (fmap f a) $ fmap f b
  fmap f (Apply a) = Apply . f $ a
  fmap _ DoNothing = DoNothing

instance Applicative Effect where
  pure a = Apply a
  DoNothing <*> _ = DoNothing
  _ <*> DoNothing = DoNothing
  Apply f <*> x = reduceEffect $ f <$> x
  Chain f g <*> x = chain (reduceEffect $ f <*> x) (reduceEffect $ g <*> x)
  Spread f g <*> x = spread (reduceEffect $ f <*> x) (reduceEffect $ g <*> x)

instance Monad Effect where
  DoNothing >>= _ = DoNothing
  Apply x >>= f = reduceEffect $ f x
  Spread x y >>= f = spread (x >>= f') (y >>= f')
    where
      f' = reduceEffect . f
  Chain x y >>= f = chain (x >>= f') (y >>= f')
    where
      f' = reduceEffect . f

instance Alternative Effect where
  empty = DoNothing
  (<|>) = (<>)

instance Semigroup (Effect a) where
  Spread a b <> c = spread a (spread b c)
  a <> Spread b c = spread a (spread b c)
  a <> b = spread a b

instance Monoid (Effect a) where
  mempty = DoNothing

instance Foldable Effect where
  foldr f m DoNothing = m
  foldr f m (Apply a) = f a m
  foldr f m (Spread a DoNothing) = foldr f m a
  foldr f m (Spread DoNothing a) = foldr f m a
  foldr f m (Spread a b) = foldr f (foldr f m b) a
  foldr f m (Chain DoNothing _) = m
  foldr f m (Chain a DoNothing) = foldr f m a
  foldr f m (Chain a b) = foldr f (foldr f m b) a

(€) :: Effect a -> Effect a -> Effect a
Chain a b € c = chain a (chain b c)
a € Chain b c = chain a (chain b c)
a € b = chain a b
