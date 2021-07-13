{-# LANGUAGE DeriveFoldable #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE DeriveFoldable #-}


module Domain.Entity.Pokemon where

import Domain.Entity.Common hiding ((.))

data PokemonAttribute typeof ability item mv =
  Name String
  | Type1 typeof
  | Type2 typeof
  | AddAbility (AbilityOp ability)
  | Physiology Physiology
  | Gender Gender
  | Stat Stat Int
  | AddMove MoveOp mv
  | HeldItem item
  | Level Int
  | Nature Nature
  | EV Stat Int
  | IV Stat Int
  deriving (Eq,Show,Ord,Functor,Foldable)

newtype Pokemon typeof ability item mv =
  Pokemon {unPokemon :: (AST (PokemonAttribute typeof ability item mv))} deriving (Eq,Show,Ord,Functor,Foldable)

class MapPokemon pkmn where
  mapType :: (typ -> typ') -> (pkmn typ a b c) -> (pkmn typ' a b c)
  mapAbility :: (ab -> ab') -> (pkmn t ab m i) -> (pkmn t ab' m i)
  mapItem :: (it -> it') -> (pkmn t a it m) -> (pkmn t a it' m)
  mapMove :: (mv -> mv') -> (pkmn t a i mv) -> (pkmn t a i mv')

instance MapPokemon PokemonAttribute where
  mapType = undefined
  mapAbility = undefined
  mapItem = undefined
  mapMove = fmap

instance MapPokemon Pokemon where
  mapType = undefined
  mapAbility = undefined
  mapItem = undefined
  mapMove = fmap
  
-- instance Functor (PokemonAttribute typeof ability item) where
  -- fmap f (AddMove m mv) = AddMove m (f mv)
  -- fmap f x = x

wrap :: PokemonAttribute a b c d -> Pokemon a b c d -> Pokemon a b c d
wrap a = Pokemon . Branch a . unPokemon

data Stat =
  HP
  | Attack
  | Defence
  | SAttack
  | SDefence
  | Speed
  deriving (Eq,Show,Ord,Enum)

data Gender =
  ProbabilityOfMale Double
  | ProbabilityOfFemale Double
  | Genderless
  deriving (Eq,Show,Ord)

data AbilityOp ability =
  SlotAbility ability
  | HiddenAbility ability
  deriving (Eq,Show,Ord)

data MoveOp =
  Move1
  | Move2
  | Move3
  | Move4
  deriving (Eq,Show,Ord,Enum)

data Physiology =
  Weight Kilograms
  | Height Meters
  deriving (Eq,Show,Ord)

newtype Kilograms = Kilograms Double deriving (Eq,Show,Ord)
newtype Meters = Meters Double deriving (Eq,Show,Ord)

class PokemonAlgebra a where
  name :: String -> a -> a
  -- type1 :: TypeOf -> a -> a
  -- type2 :: TypeOf -> a -> a
  stat :: Stat -> Int -> a -> a
  hp :: Int -> a -> a
  hp = stat HP
  attack :: Int -> a -> a
  attack = stat Attack
  defence :: Int -> a -> a
  defence = stat Defence
  sattack :: Int -> a -> a
  sattack = stat SAttack
  sdefence :: Int -> a -> a
  sdefence = stat SDefence
  speed :: Int -> a -> a
  speed = stat Speed
  -- ability :: Ability -> a -> a
  -- hiddenAbility :: Ability -> a -> a
  gender :: Gender -> a -> a
  (%) :: Double -> (Double -> Gender) -> a -> a
  prob % gndr = gender (gndr prob)
  probabilityOfMale :: Double -> a -> a
  probabilityOfMale d = gender (ProbabilityOfMale d)
  probabilityOfFemale :: Double -> a -> a
  probabilityOfFemale d = gender (ProbabilityOfMale (1 - d))
  genderless :: a -> a
  genderless = gender Genderless
  weight :: Double -> a -> a
  height :: Double -> a -> a
  level :: Int -> a -> a
  ev :: Stat -> Int -> a -> a
  iv :: Stat -> Int -> a -> a

infixr 9 %

male, female :: Double -> Gender
male p = ProbabilityOfMale (p / 100)
female p = ProbabilityOfFemale (p / 100)

class PokemonMoveAlgebra pkmn mv where
  move1 :: mv -> pkmn -> pkmn
  move2 :: mv -> pkmn -> pkmn
  move3 :: mv -> pkmn -> pkmn
  move4 :: mv -> pkmn -> pkmn

class PokemonItemAlgebra pkmn it where
  heldItem :: it -> pkmn -> pkmn

class PokemonAbilityAlgebra pkmn ab where
  ability :: ab -> pkmn -> pkmn
  abilities :: ab -> ab -> pkmn -> pkmn
  abilities a b = ability a . ability b

class PokemonTypeAlgebra pkmn tp where
  type1 :: tp -> pkmn -> pkmn 
  type2 :: tp -> pkmn -> pkmn
  typeof :: (tp, tp) -> pkmn -> pkmn
  typeof (t,t') = type1 t . type2 t'

instance PokemonAlgebra (Pokemon typeof ability item mv) where
  name s = wrap (Name s)
  stat s i = wrap (Stat s i)
  gender d = wrap (Gender d)
  weight i = wrap (Physiology (Weight (Kilograms i)))
  height i = wrap (Physiology (Height (Meters i)))
  level l = wrap (Level l)
  ev st i = wrap (EV st i)
  iv st i = wrap (IV st i)


newtype Nature = Nature' String deriving (Eq,Show,Ord)

end :: Pokemon typeof ability item mv
end = Pokemon End
