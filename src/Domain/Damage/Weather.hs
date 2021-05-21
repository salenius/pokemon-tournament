module Domain.Damage.Weather where

import Attribute
import Domain.Common
import Types.BuiltIn
import Prelude hiding (and)
import Control.Lens

class (CounterpartyGetter m,
      PokemonAttrGetter m) => WeatherDamage m where
  moveType :: TypeOf -> m Bool
  fire :: m Bool
  fire = moveType Fire
  water :: m Bool
  water = moveType Water
  hasType :: Counterparty -> TypeOf -> m Bool
  moveIs :: DamagingCategory' -> m Bool
  targetHasType :: TypeOf -> m Bool
  targetHasType = hasType Target
  and :: m Bool -> m Bool -> m Bool
  and a b = do
    x <- a
    y <- b
    return $ x && y
  while :: m Bool -> m Bool -> m Bool
  while = and
  it's :: Weather -> m Bool
  (-->) :: m Bool -> m () -> m ()
  cond --> action = do
    c <- cond
    if c then action else return ()
  multiplyBy :: Double -> m ()

infixl 4 `and` 
infixl 2 -->

program :: WeatherDamage d => d ()
program = do
  fire  `while` it's rainy --> multiplyBy 0.5
  fire  `while` it's sunny --> multiplyBy 1.5
  water `while` it's sunny --> multiplyBy 0.5
  water `while` it's rainy --> multiplyBy 1.5
  targetHasType rock `and` moveIs special `while` it's sandstorm 
    --> multiplyBy 0.5

rock = Rock
sunny = Sunny
rainy = Rainy
sandstorm = Sandstorm
special = review _SpecialMove Special
