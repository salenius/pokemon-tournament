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
