{-# LANGUAGE TemplateHaskell #-}

module Domain.Attribute.Quadruple where

import Control.Lens

data Quadruple m =
  Monoruple m
  | Duoruple m m
  | Triruple m m m
  | Quadruple m m m m
  deriving (Eq,Read)

instance Show m => Show (Quadruple m) where
  show (Monoruple a) = show [a]
  show (Duoruple a b) = show [a,b]
  show (Triruple a b c) = show [a,b,c]
  show (Quadruple a b c d) = show [a,b,c,d]

instance Functor Quadruple where
  fmap f (Monoruple a) = Monoruple $ f a
  fmap f (Duoruple a b) = Duoruple (f a) $ f b
  fmap f (Triruple a b c) = Triruple (f a) (f b) $ f c
  fmap f (Quadruple a b c d) = Quadruple (f a) (f b) (f c) $ f d


makePrisms ''Quadruple
