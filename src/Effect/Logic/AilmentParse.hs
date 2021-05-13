{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE RankNTypes #-}


module Effect.Logic.AilmentParse where

import Attribute.Counterparty
import Attribute.Ability
import Attribute.TypeOf
import Attribute.Weather
import Effect.Logic.AilmentData
import Control.Lens
import Control.Applicative
import Control.Monad.Reader

type LeafGuard' = (LeafGuard, Counterparty, Sunny)

leafGuard :: Env (Ability' ab)
leafGuard = do
  AilmentData{..} <- ask
  lg <- getAbility _LeafGuard
  s  <- lift $ preview _Sunny weather
  return $ IsLeafGuard (lg, affected, s)


type Comatose' = (Comatose, Counterparty)

comatose :: Env (Ability' ab)
comatose = do
  ab      <- getAbility _Comatose
  AilmentData{..} <- ask
  return $ IsComatose (ab, affected)


data Ability' ab =
  IsLeafGuard LeafGuard'
  | IsComatose Comatose'
  | Ability' ab
  deriving (Eq,Show,Read,Ord)

ability' :: Env ab -> Env (Ability' ab)
ability' g = leafGuard <|> comatose <|> Ability' <$> g


type Safeguard'  = (Safeguard, Counterparty)

safeguard :: Env Safeguard'
safeguard = do
  AilmentData{..} <- ask
  x               <- lift $ safeguardOn affected
  return $ (x, affected)

  
type MoldBreaker' ab = (MoldBreaker_, Ability' ab)

moldBreaker_ :: Env MoldBreaker
moldBreaker_ = getAbilityFor _MoldBreaker ((==) Target) opposite

teravolt_ :: Env Teravolt
teravolt_ = getAbilityFor _Teravolt ((==) Target) opposite

turboblaze_ :: Env Turboblaze
turboblaze_ = getAbilityFor _Turboblaze ((==) Target) opposite

moldBreaker' :: Env ab -> Env (MoldBreaker' ab)
moldBreaker' g = do
  ab  <- ability' g
  mb'  <- MoldBreaker_ <$> moldBreaker_ <|>
    Turboblaze_ <$> turboblaze_ <|>
    Teravolt_ <$> teravolt_
  return $ (mb', ab)

  
-- 

type PoisonOrSteelType = Either PoisonType SteelType

---- Combinations

type Synchronizing ailm = (Synchronize, ailm)



limber :: Env (Ability' Limber)
magmaArmor :: Env (Ability' MagmaArmor)
immunity :: Env (Ability' Immunity)
waterVeil :: Env (Ability' WaterVeil_)
waterVeil_ :: Env WaterVeil_
waterBubble_ :: Env WaterVeil_
insomnia :: Env (Ability' Insomnia_)
insomnia_ :: Env Insomnia_
vitalSpirit_ :: Env Insomnia_

-- Ability based immunities

limber = Ability' <$> getAbility _Limber
magmaArmor = Ability' <$> getAbility _MagmaArmor
immunity = Ability' <$> getAbility _Immunity
waterVeil = Ability' <$> (waterVeil_ <|> waterBubble_)
insomnia = Ability' <$> (insomnia_ <|> vitalSpirit_)
  
insomnia_ = Insomnia_ <$> getAbility _Insomnia
vitalSpirit_ = VitalSpirit_ <$> getAbility _VitalSpirit
waterVeil_ = WaterVeil_ <$> getAbility _WaterVeil
waterBubble_ = WaterBubble_ <$> getAbility _WaterBubble

-- ailment


getAbility :: Prism' PokemonAbility ab -> Env ab
getAbilityFor ::
  Prism' PokemonAbility ab
  -> (Counterparty -> Bool)
  -> (Counterparty -> Counterparty)
  -> Env ab


getAbility p = getAbilityFor p (\_ -> True) id

getAbilityFor p b f = do
  AilmentData{..} <- ask
  guard $ b affected
  lg <- lift $ preview p (ability $ f affected)
  return $ lg

