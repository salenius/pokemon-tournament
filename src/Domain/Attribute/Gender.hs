module Domain.Attribute.Gender where

data Gender =
  Male
  | Female
  | Genderless
  deriving (Eq,Show,Read)

data GenderRatio =
  Male7to1
  | Male3to1
  | EvenRatio
  | AlwaysGenderless
  | AlwaysMale
  | AlwaysFemale
  deriving (Eq,Show,Read)
