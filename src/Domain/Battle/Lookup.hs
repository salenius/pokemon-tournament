{-# LANGUAGE ConstraintKinds #-}

module Domain.Battle.Lookup where

import Domain.Attribute.Player
import Domain.Attribute.HP
import Domain.Attribute.Ailment
import Domain.Attribute.Conditions
import Domain.Attribute.ModifStat
import Domain.Attribute.MoveExecution
import Domain.Attribute.Ability
import Domain.Attribute.HeldItem
import Domain.Attribute.TypeOf
import Domain.Attribute.Weather

class Monad m => PlayerView m where
  pkmnHp :: Player -> m HP
  pkmnAilment :: Player -> m Ailment
  pkmnCondition :: PokemonCondition -> Player -> m Bool
  pkmnStatModif :: ModifStat -> Player -> m Int
  pkmnTypeOf :: Player -> m [TypeOf]
  pkmnAbility :: Player -> m Ability
  pkmnHeldItem :: Player -> m (Maybe HeldItem)
  plrConditon :: PlayerCondition -> Player -> m (Maybe Int)
  plrPokemonLeft :: Player -> m Int



class Monad m => BattleView m where
  btlWeather :: m Weather
  
  
class Monad m => StrikeFactView m where
  user :: m Player
  target :: m Player
  previousStrike :: m (Maybe MoveExecution)
  userIsFaster :: m Bool
  userIsFaster = do
    b <- previousStrike
    return $ case b of
      Nothing -> True
      _ -> False

type StrikeView m = (PlayerView m, BattleView m, StrikeFactView m)
