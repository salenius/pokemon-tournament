module Domain.Attribute.Screen where

data LightScreen =
  NoLightScreen
  | LightScreen Int
  deriving (Eq,Show,Ord)

instance Semigroup LightScreen where
  NoLightScreen <> NoLightScreen = NoLightScreen
  NoLightScreen <> a = a
  a <> NoLightScreen = a
  LightScreen a <> LightScreen b = LightScreen $ a + b

data Reflect =
  NoReflect
  | Reflect Int
  deriving (Eq,Show,Ord)

instance Semigroup Reflect where
  NoReflect <> NoReflect = NoReflect
  NoReflect <> a = a
  a <> NoReflect = a
  Reflect a <> Reflect b = Reflect $ a + b

data Safeguard =
  NoSafeguard
  | Safeguard Int
  deriving (Eq,Show,Ord)

instance Semigroup Safeguard where
  NoSafeguard <> NoSafeguard = NoSafeguard
  NoSafeguard <> a = a
  a <> NoSafeguard = a
  Safeguard a <> Safeguard b = Safeguard $ a + b
