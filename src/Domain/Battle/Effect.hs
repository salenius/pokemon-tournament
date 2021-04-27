{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}


module Domain.Battle.Effect where

import Domain.Attribute.Player
import Domain.Attribute.TypeOf
import Domain.Attribute.Ailment
import Domain.Attribute.Weather
import Domain.Attribute.HeldItem
import Domain.Battle.Id
import Control.Monad.Free

class PlayerChoiceAlgebra alg where
  chosenMove :: Player -> alg MoveId
  chosenItem :: Player -> alg HeldItem
  chosenPokemon :: Player -> alg PokemonId

class GenRandomAlgebra alg where
  randomDouble :: alg Double
  randomInt ::  (Int,Int) -> alg Int

class BattleLogAlgebra alg where
  log :: LogEvent -> alg ()

data PlayerChoiceF a =
  ChosenMove Player (MoveId -> a)
  | ChosenItem Player (HeldItem -> a)
  | ChosenPokemon Player (PokemonId -> a)
  deriving (Functor)

type PlayerChoice = Free PlayerChoiceF

data GenRandomF a =
  RandomDouble (Double -> a)
  | RandomInt (Int,Int) (Int -> a)
  deriving (Functor)

type GenRandom = Free GenRandomF

data BattleLogF a = Log LogEvent a deriving (Functor)

type BattleLog = Free BattleLogF

data LogEvent =
  PlayerChosenPokemon Player
  | PokemonUsedMove Player String
  | MoveIsCritical 
  | MoveEffectiveness TypeEffect
  | PokemonIsFlinched 
  | PokemonIsAilmented Player Ailment
  | PokemonIsAffectedByWeather Player Weather
  | PokemonFainted Player
  | PlayerIsDefeated Player
  deriving (Eq,Show)

type LogAndGenF = Coproduct BattleLogF GenRandomF

type ChooseAndLogAndGenF = Coproduct PlayerChoiceF LogAndGenF

type ChooseAndLogAndGen = Free ChooseAndLogAndGenF

type BattleEffect = ChooseAndLogAndGen

instance PlayerChoiceAlgebra ChooseAndLogAndGen where
  chosenPokemon plr = liftF . left $ ChosenPokemon plr id
  chosenItem plr = liftF . left $ ChosenItem plr id
  chosenMove plr = liftF . left $ ChosenMove plr id

instance GenRandomAlgebra ChooseAndLogAndGen where
  randomDouble = liftF . right . right $ RandomDouble id
  randomInt (a,b) = liftF . right . right $ RandomInt (a,b) id

instance BattleLogAlgebra ChooseAndLogAndGen where
  log evnt = liftF . right . left $ Log evnt ()

-----------------------

newtype Coproduct f g a = Coproduct { getCoproduct :: Either (f a) (g a) }
  deriving (Eq, Ord, Read, Show)

left :: f a -> Coproduct f g a
left = Coproduct . Left

right :: g a -> Coproduct f g a
right = Coproduct . Right

coproduct :: (f a -> b) -> (g a -> b) -> Coproduct f g a -> b
coproduct f g = either f g . getCoproduct

instance (Functor f, Functor g) => Functor (Coproduct f g) where
  fmap f = Coproduct . coproduct (Left . fmap f) (Right . fmap f)

instance (Foldable f, Foldable g) => Foldable (Coproduct f g) where
  foldMap f = coproduct (foldMap f) (foldMap f)

instance (Traversable f, Traversable g) => Traversable (Coproduct f g) where
  traverse f = coproduct
    (fmap (Coproduct . Left) . traverse f)
    (fmap (Coproduct . Right) . traverse f)
