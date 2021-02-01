{-# LANGUAGE FlexibleContexts, GADTs #-}

module Domain.Battle.Algebra where

import Domain.Attribute.Player
import Domain.Attribute.MoveExecution
import Domain.Attribute.Weather as W
import Domain.Entity.Battle as B
import Domain.Entity.PokemonState
import Control.Monad.State
import Control.Lens
import Data.Maybe

class BattleAlgebra b where
  getUser :: b -> Maybe Player
  getCurrentPokemon :: Player -> b -> Maybe PokemonState
  -- getNextPokemon :: Player -> b -> Maybe PokemonState
  updateCurrentPokemon :: (PokemonState -> PokemonState) -> Player -> b -> b
  -- updatePlayer :: (PlayerState -> PlayerState) -> Player -> b -> b
  getTarget :: b -> Maybe Player
  getTarget battle
    | getUser battle == Just Player1 = Just Player2
    | getUser battle == Just Player2 = Just Player1
    | otherwise = Nothing
  getWeather :: b -> Maybe W.Weather
  getPlayer :: Player -> b -> Maybe PlayerState
  updatePlayer :: (PlayerState -> PlayerState) -> Player -> b -> b
  setWeather :: W.Weather -> Int -> b -> b

class ReadPreviousStrike b where
  askPrevStrike :: b -> Maybe MoveExecution

instance BattleAlgebra Battle where
  getUser = preview user
  getCurrentPokemon plr battle = preview (player plr . currentPokemon . _AbleToBattle) battle
  updateCurrentPokemon f plr battle = over (player plr . currentPokemon . _AbleToBattle) f battle
  getWeather = preview (_OnGoingBattle . battleState . weather . currentWeatherState)
  getPlayer pl = preview (player pl)
  updatePlayer f pl = over (player pl) f
  setWeather W.Normal _ = set (_OnGoingBattle . battleState . B.weather) (PermanentWeather W.Normal)
  setWeather w x = set (_OnGoingBattle . battleState . B.weather) (TemporaryWeather $ TemporaryWeather' w x)

-- Battle-datatyypi ei itsessään sisällä tietoja viimeisestä liikkeestä
instance ReadPreviousStrike Battle where
  askPrevStrike _ = Nothing

attributeSatisfies :: (Eq a, BattleAlgebra b) => (b -> Maybe a) -> (a -> Bool) -> (b -> Bool)
attributeSatisfies getter cond battle = case getter battle >>= return . cond of
  Just x -> x
  Nothing -> False

getUserPokemon f battle = getUser battle >>= flip getCurrentPokemon battle >>= return . f
getTargetPokemon f battle = getTarget battle >>= flip getCurrentPokemon battle >>= return . f

modifyCpPokemon ::
  BattleAlgebra b
  => (b -> Maybe Player)
  -> (PokemonState -> PokemonState)
  -> b
  -> b
modifyCpPokemon getter f battle = fromMaybe battle $ do
  usr <- getter battle
  let battle' = updateCurrentPokemon f usr battle
  return battle'

modifyUserPokemon ::
  BattleAlgebra b
  => (PokemonState -> PokemonState)
  -> b
  -> b
modifyUserPokemon = modifyCpPokemon getUser

modifyTargetPokemon ::
  BattleAlgebra b
  => (PokemonState -> PokemonState)
  -> b
  -> b
modifyTargetPokemon = modifyCpPokemon getTarget

modifyCp ::
  BattleAlgebra b
  => (b -> Maybe Player)
  -> (PlayerState -> PlayerState)
  -> b
  -> b
modifyCp getter f battle = fromMaybe battle $ do
  usr <- getter battle
  let battle' = updatePlayer f usr battle
  return battle'

modifyUser ::
  BattleAlgebra b
  => (PlayerState -> PlayerState)
  -> b
  -> b
modifyUser = modifyCp getUser

modifyTarget ::
  BattleAlgebra b
  => (PlayerState -> PlayerState)
  -> b
  -> b
modifyTarget = modifyCp getTarget

updateBattle :: (MonadState b m, BattleAlgebra b) => (b -> b) -> m ()
updateBattle fn = do
  battle <- get
  let battle' = fn battle
  put battle'
