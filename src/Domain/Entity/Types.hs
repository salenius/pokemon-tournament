{-# LANGUAGE TypeFamilies #-}

module Domain.Entity.Types where

class TypeOfAlgebra a where
  supereffetiveAgainst :: a TypeOf -> a TypeOf -> a TypeOf
  weakAgainst :: a TypeOf -> a TypeOf -> a TypeOf
  powerlessAgainst :: a TypeOf -> a TypeOf -> a TypeOf
  neutralAgainst :: a TypeOf -> a TypeOf -> a TypeOf

