{-# LANGUAGE FlexibleContexts, GADTs #-}

module Domain.Battle.Algebra where

import Domain.Attribute.Player
import Domain.Attribute.MoveExecution
import Domain.Attribute.Weather as W
import Domain.Entity.Battle as B
import Domain.Entity.PokemonState
import Domain.Entity.PlayerState
import Control.Monad.State
import Control.Lens
import Data.Maybe

getCurrentPokemon :: PlayerState -> PokemonState
getCurrentPokemon = view currentPokemon

class BattleView b where
  getBattle :: b -> BattleState

instance BattleView BattleState where
  getBattle = id

getPlayer :: BattleView b => Player -> b -> PlayerState
getPlayer plr = fn plr . getBattle
  where
    fn Player1 = view player1
    fn Player2 = view player2

getPokemon :: BattleView b => Player -> b -> PokemonState
getPokemon plr = getCurrentPokemon . getPlayer plr
