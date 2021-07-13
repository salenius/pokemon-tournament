{-# LANGUAGE FlexibleInstances #-}

module Domain.Entity.GenV.Item where

data ChampionsTournamentItem choice eff =
  LightBall
  | WhiteHerb
  | FocusSash
  | ChoiceScarf
  | QuickClaw
  | SitrusBerry
  | ChoiceBand
  | LifeOrb
  | KingsRock
  | ExpertBelt
  | ScopeLens
  | PowerHerb
  | OccaBerry
  | AirBalloon
  | RockyHelmet
  | Leftovers
  | RindoBerry
  | ChartiBerry
  | Potion choice eff
  deriving (Eq,Show,Ord)
