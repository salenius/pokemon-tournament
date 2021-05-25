{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RankNTypes #-}


module Damage.CriticalHit where

import Damage.Interpreter
import Control.Lens
import Control.Applicative
import Attribute.Ability
import Attribute.Ailment
import Attribute.Item
import Attribute.Counterparty
import Helper.Parse
import Algebra.Pokemon
import Data.Maybe

data CriticalHit = NotCritical | IsCritical deriving (Eq,Show,Ord,Enum)

data CriticalHitLevel =
  NeverHits
  | Level0
  | Level1
  | Level2
  | Level3
  | Level4
  | AlwaysHits
  deriving (Eq,Show,Ord,Enum)

increaseCrit :: CriticalHitLevel -> CriticalHitLevel
increaseCrit AlwaysHits = AlwaysHits
increaseCrit x = succ x

levelToInt :: CriticalHitLevel -> Int
levelToInt = \case
  NeverHits -> -1
  Level0 -> 0
  Level1 -> 1
  Level2 -> 2
  Level3 -> 3
  Level4 -> 4
  AlwaysHits -> 5

type Prob = Double

data CriticalHitCalc m = CriticalHitCalc
  {
    _initialLevel :: CriticalHitLevel
  , _pokemonAbility :: Counterparty -> PokemonAbility
  , _pokemonHeldItem :: Counterparty -> Maybe HeldItem
  , _pokemonAilment :: Counterparty -> Ailment
  , _manipulateProb :: CriticalHitLevel -> m CriticalHitLevel
  , _drawHit :: Prob -> m CriticalHit
  }

makeLenses ''CriticalHitCalc


type MercilessD =  (UserHas Merciless, TargetHas Poisoned')

type BattleArmorD = (UserHas (Either ShellArmor BattleArmor), TargetHas NotMoldBreaker)

type ScopeLensD = UserHas ScopeLens

mkCriticalHitCalc ::
  (DamageOps m)
  => (Counterparty -> PokemonAbility)
  -> (Counterparty -> Maybe HeldItem)
  -> (Counterparty -> Ailment)
  -> CriticalHitCalc m
mkCriticalHitCalc pkmnAb pkmnIt pkmnAi =
  CriticalHitCalc Level0 pkmnAb pkmnIt pkmnAi f g
  where
    f cr = return $
      cr &
      scopeLens pkmnIt &
      merciless pkmnAi pkmnAb &
      battleArmor pkmnAb
    g pr = do
      x <- drawRandomDouble
      return $ case (x < pr) of
        True -> IsCritical
        False -> NotCritical

battleArmor :: (Counterparty -> PokemonAbility) -> CriticalHitLevel -> CriticalHitLevel
battleArmor cpf lvl = fromMaybe lvl $ do
  let usrAb = cpf User
  let trgtAb = cpf Target
  trgtAb' <- (Left <$> preview _BattleArmor trgtAb) <|> (Right <$> preview _ShellArmor trgtAb)
  usrAb'  <- notMoldBreaker usrAb
  return $ (\_ _ -> NeverHits) (trgtAb', usrAb') lvl

scopeLens :: (Counterparty -> Maybe HeldItem) -> CriticalHitLevel -> CriticalHitLevel
scopeLens cpf lvl = fromMaybe lvl $ do
  usrIt <- cpf User
  sclens <- preview _ScopeLens usrIt
  return $ (\_ x -> increaseCrit x) sclens lvl

merciless ::
  (Counterparty -> Ailment)
  -> (Counterparty -> PokemonAbility)
  ->  CriticalHitLevel
  -> CriticalHitLevel
merciless ailmf abilf lvl = fromMaybe lvl $ do
  usrAb <- abilf User & preview _Merciless
  trgtAi <- ailmf Target & preview _Poisoned
  return $ (\_ _ -> AlwaysHits) (usrAb, trgtAi) lvl
