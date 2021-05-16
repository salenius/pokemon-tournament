{-# LANGUAGE RankNTypes #-}


module Entity.Pokemon.BuiltIn where

import Entity.Pokemon.Algebra
import Attribute.Ability hiding (ability)
import Types.BuiltIn

pikachu, lapras, snorlax, venusaur, charizard, blastoise :: Pokemon

type PokemonBuilder = Pokemon

data Joku = Joku deriving (Eq,Show)

pikachu = do
  name "Pikachu"
  hp 35
  attack 55
  defence 40
  sattack 50
  sdefence 50
  speed 90
  typeof Electric
  50 % male
  ability Static
  weight 6.0
  height 0.4
  end

lapras = do
  name "Lapras"
  hp 130
  attack 85
  defence 80
  sattack 85
  sdefence 95
  speed 60
  typeof (Water, Ice)
  50 % male
  abilities WaterAbsorb ShellArmor
  weight 220.0
  height 2.5
  end

snorlax = do
  name "Snorlax"
  hp 160
  attack 110
  defence 65
  sattack 65
  sdefence 110
  speed 30
  typeof Normal
  87.5 % male
  abilities Immunity ThickFat
  weight 460.0
  height 2.1
  end

venusaur = do
  name "Venusaur"
  hp 80
  attack 82
  defence 83
  sattack 100
  sdefence 100
  speed 80
  typeof (Grass, Poison)
  87.5 % male
  ability Overgrow
  hiddenAbility Chlorophyll
  weight 100.0
  height 2.0
  end

charizard = do
  name "Charizard"
  hp 78
  attack 84
  defence 78
  sattack 109
  sdefence 85
  speed 100
  typeof (Fire, Flying)
  87.5 % male
  ability Blaze
  weight 90.5
  height 1.7
  end

blastoise = do
  name "Blastoise"
  hp 79
  attack 83
  defence 100
  sattack 85
  sdefence 105
  speed 78
  typeof Water
  87.5 % male
  ability Torrent
  weight 85.5
  height 1.6
  end
