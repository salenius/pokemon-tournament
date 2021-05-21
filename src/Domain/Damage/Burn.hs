module Domain.Damage.Burn where

import Attribute 
import Domain.Common hiding (burned)
import Attribute.Category
import Prelude hiding (and)
import Control.Lens (review, isn't)

class (CounterpartyGetter m,
       MoldBreakerGetter m,
      PokemonAttrGetter m) => BurnDamage m where
  currentlyAilmented :: Counterparty -> (Ailment -> Bool) -> m Bool
  userIs :: (Ailment -> Bool) -> m Bool
  userIs = currentlyAilmented User
  targetIs :: (Ailment -> Bool) -> m Bool
  targetIs = currentlyAilmented Target
  moveIs :: DamagingCategory' -> m Bool
  guts :: Player -> m Bool
  marvelScale :: Player -> m Bool
  (-->) :: m Bool -> m Apply -> m Apply
  and :: m Bool -> m Bool -> m Bool
  a `and` b = do
    x <- a
    y <- b
    return $ x && y
  otherCase :: m Apply -> m Apply -> m Apply
  otherCase action alternate = do
    x <- action
    case x of
      NotApplied -> alternate
      Applied -> return x
  multiplyBy :: Double -> m Apply
  doNothing :: m Apply
  doNothing = return NotApplied

infixr 5 `and`
infixr 3 -->
infixr 1 `otherCase`

program :: BurnDamage m => m ()
program = do
  userIs burned `and` moveIs physical
    --> userHas guts
    --> multiplyBy 1.5
    `otherCase` multiplyBy 0.5
  targetIs badlyAilmented `and` moveIs physical `and` targetHas marvelScale
    --> userHas moldBreaker
    --> doNothing
    `otherCase` multiplyBy 0.5
  return ()

data Apply = NotApplied | Applied deriving (Eq,Show)

burned = \x -> _Burned `isn't` x == False
badlyAilmented = \x -> _Healthy `isn't` x
physical = review _PhysicalMove Physical
