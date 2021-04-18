{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ViewPatterns #-}



module Domain.Entity.BuiltIn.Pokemon.Gender where

import Domain.Attribute.Gender
import Control.Lens

data Male7to1 =
  Male7of8
  | Female1of8
  deriving (Eq,Show,Read)

data Male3to1 =
  Male3of4
  | Female1of4
  deriving (Eq,Show,Read)

data Male1to1 =
  Male1of2
  | Female1of2
  deriving (Eq,Show,Read)

type TotalGenderless = ()
type TotalMale = ()
type TotalFemale = ()

--

instance Semigroup Male7to1 where
  _ <> a = a

instance Semigroup Male3to1 where
  _ <> a = a

instance Semigroup Male1to1 where
  _ <> a = a

instance Monoid Male7to1 where
  mempty = Male7of8

instance Monoid Male3to1 where
  mempty = Male3of4

instance Monoid Male1to1 where
  mempty = Female1of2

instance GenderParser Male7to1 where
  _Gender = _Male7to1

instance GenderParser Male3to1 where
  _Gender = _Male3to1

instance GenderParser Male1to1 where
  _Gender = _Male1to1

--

class GenderParser gndr where
  _Gender :: Prism' Gender gndr

_Male7to1 :: Prism' Gender Male7to1
_Male7to1 = mkprism2 (Male, Male7of8) (Female, Female1of8)

_Male3to1 :: Prism' Gender Male3to1
_Male3to1 = mkprism2 (Male, Male3of4) (Female, Female1of4)

_Male1to1 :: Prism' Gender Male1to1
_Male1to1 = mkprism2 (Male, Male1of2) (Female, Female1of2)


mkprism2 :: Eq a => (Gender, a) -> (Gender, a) -> Prism' Gender a
mkprism2 (m, m') (f, f') = prism h g
  where
    h ((==) m' -> True) = m
    h _ = f
    g ((==) m -> True) = Right m'
    g ((==) f -> True) = Right f'
    g x = Left x
