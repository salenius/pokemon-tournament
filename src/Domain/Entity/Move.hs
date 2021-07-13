{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FunctionalDependencies #-}


module Domain.Entity.Move where

import Domain.Entity.Common

data Category =
  Physical
  | Special
  | Status
  deriving (Eq,Show,Ord,Enum)

data MoveAttribute typeof choice success prob damage effect =
  Name String
  | PP Int
  | Basepower Int
  | Category Category
  | Accuracy (Accuracy prob)
  | MakesContact
  | Priority Int
  | TypeOf typeof
  | SideEffect effect
  | MoveSucceeds success
  | DamageDealing damage
  | Choice choice
  deriving (Eq,Show,Ord)

data Accuracy prob =
  HitProbability Double
  | DependentProbability prob
  | NeverMisses
  deriving (Eq,Show,Ord)


type Move typeof choice success prob damage effect
  = AST (MoveAttribute typeof choice success prob damage effect)

class MoveAlgebra a where
  name :: String -> a -> a
  pp :: Int -> a -> a
  basepower :: Int -> a -> a
  accuracy :: Double -> a -> a
  category :: Category -> a -> a
  physical :: a -> a
  physical = category Physical
  special :: a -> a
  special = category Special
  status :: a -> a
  status = category Status
  makesContact :: a -> a
  alwaysHits :: a -> a
  priority :: Int -> a -> a

class MoveTypeAlgebra a t | a -> t where
  typeof :: t -> a -> a

instance MoveAlgebra (Move typeof choice success prob damage effect) where
  name s = Branch (Name s)
  pp i = Branch (PP i)
  basepower i  = Branch (Basepower i)
  category c = Branch (Category c)
  accuracy d = Branch (Accuracy (HitProbability d))
  makesContact = Branch MakesContact
  priority i = Branch (Priority i)
  alwaysHits = Branch (Accuracy NeverMisses)
