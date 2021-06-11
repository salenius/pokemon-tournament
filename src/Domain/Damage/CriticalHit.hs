module Domain.Damage.CriticalHit where

import Attribute
import Domain.Attribute
import Types.Pokemon
import Stats.Base

class (Alternative m,
       MonadLogic m,
       GetCounterparty m,
       IgnoreAbility m,
       IgnoreItem m,
       PokemonAttribute m) => CriticalHitCalc m where
  -- Battle Armor & Shell Armor are reflected here
  abilityNonPreventing :: m PokemonAbility
  -- Scope Lens is here
  chanceIncreasingItem :: m (HeldItem, Int)
  -- Merciless ability belongs to this
  chanceIncreasingAilment :: m (PokemonAbility, Ailment, Int)
  -- If move has increased CH (like Stone Edge has), it's reflected here
  criticalHitLevel :: m Int
  genRandomDouble :: m Double

criticalHit :: CriticalHitCalc m => m Double
criticalHit = do
  lvl  <- criticalHitLevel
  trgt <- counterparty Target
  tAb  <- pokemonAbility trgt
  ab   <- abilityNonPreventing
  lvl' <- addCriticalHitLevel lvl
  rnd  <- genRandomDouble
  ab === tAb <|> ignoreTargetAbility
  guard $ critAsProb lvl' > rnd
  return 1.5

addCriticalHitLevel :: CriticalHitCalc m => Int -> m Int
addCriticalHitLevel lvl = do
  mc <- mercilessly lvl
  sc <- scopeLensly mc
  return sc

mercilessly :: CriticalHitCalc m => Int -> m Int
mercilessly lvl = do
  usr  <- counterparty User
  trgt <- counterparty Target
  ab'  <- pokemonAbility usr
  ail  <- pokemonAilment trgt
  (ab, ai, ll) <- chanceIncreasingAilment
  ab === ab' <&&> ail === ai <|> guard True
  let l' = lvl + ll
  once $ return l' <|> return lvl

scopeLensly :: CriticalHitCalc m => Int -> m Int
scopeLensly lvl = do
  usr    <- counterparty User
  it     <- pokemonHeldItem usr
  (i, l) <- chanceIncreasingItem
  i === it <&&> ableToUseItem usr <|> guard True
  let l' = lvl + l
  once $ return l' <|> return lvl

critAsProb :: Int -> Double
critAsProb 0 = 0.0625
critAsProb 1 = 0.25
critAsProb 2 = 0.333
critAsProb 3 = 0.5
critAsProb _ = 1.0
