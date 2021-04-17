{-# LANGUAGE GeneralizedNewtypeDeriving, DeriveFoldable, DeriveFunctor, TemplateHaskell #-}


module Domain.Pokemon.Species (
  PokemonSpecies(..),
  speciesName,
  typeOf,
  baseHp,
  baseAttack,
  baseDefence,
  baseSAttack,
  baseSDefence,
  baseSpeed,
  weight,
  genderRatio,
  TypeOfPokemon(),
  getTypeOfPokemon,
  mkTypeOfPokemon,
  typeOfPokemonAsList,
  AsPokemonSpecies,
  asSpecies
                              ) where

import Domain.Pokemon.BuiltIn
import Domain.Attribute.TypeOf
import Domain.Attribute.Ability
import Domain.Attribute.Gender
import Data.List.NonEmpty as N
import Control.Lens
import Domain.Pokemon.TypeOf

data PokemonSpecies = PokemonSpecies
  {
  _speciesName :: String
  , _typeOf :: TypeOfPokemon
  , _baseHp :: Int
  , _baseAttack :: Int
  , _baseDefence :: Int
  , _baseSAttack :: Int
  , _baseSDefence :: Int
  , _baseSpeed :: Int
  , _weight :: Double
  , _genderRatio :: Gender -> Double
  }

makeLenses ''PokemonSpecies

class AsPokemonSpecies pkmn where
  asSpecies :: pkmn -> PokemonSpecies

mkTypeOfPokemon :: NonEmpty TypeOf -> TypeOfPokemon
mkTypeOfPokemon = TypeOfPokemon . N.take 2

typeOfPokemonAsList :: TypeOfPokemon -> [TypeOf]
typeOfPokemonAsList (TypeOfPokemon tp) = tp

instance AsPokemonSpecies PokemonSpecies where
  asSpecies = id

instance AsPokemonSpecies BuiltInPokemon where
  asSpecies Pikachu = PokemonSpecies {
    _speciesName = "Pikachu"
    , _typeOf = mkTypeOfPokemon $ Electric :| []
    , _baseHp = 35
    , _baseAttack = 55
    , _baseDefence = 40
    , _baseSAttack = 50
    , _baseSDefence = 50
    , _baseSpeed = 90
    , _weight = 6.0
    , _genderRatio = male7to1
    }

  asSpecies Lapras = PokemonSpecies {
    _speciesName = "Lapras"
    , _typeOf = mkTypeOfPokemon $ Water :| [Ice]
    , _baseHp = 130
    , _baseAttack = 85
    , _baseDefence = 80
    , _baseSAttack = 85
    , _baseSDefence = 95
    , _baseSpeed = 60
    , _weight = 220.0
    , _genderRatio = male7to1
    }

  asSpecies Snorlax = PokemonSpecies {
    _speciesName = "Snorlax"
    , _typeOf = mkTypeOfPokemon $ Normal:| []
    , _baseHp = 160
    , _baseAttack = 110
    , _baseDefence = 65
    , _baseSAttack = 65
    , _baseSDefence = 110
    , _baseSpeed = 30
    , _weight = 460.0
    , _genderRatio = male7to1
    }

  asSpecies Venusaur = PokemonSpecies {
    _speciesName = "Venusaur"
    , _typeOf = mkTypeOfPokemon $ Grass :| [Poison]
    , _baseHp = 80
    , _baseAttack = 82
    , _baseDefence = 83
    , _baseSAttack = 100
    , _baseSDefence = 100
    , _baseSpeed = 80
    , _weight = 100.0
    , _genderRatio = male7to1
    }

  asSpecies Charizard = PokemonSpecies {
    _speciesName = "Charizard"
    , _typeOf = mkTypeOfPokemon $ Fire :| [Flying]
    , _baseHp = 78
    , _baseAttack = 84
    , _baseDefence = 78
    , _baseSAttack = 109
    , _baseSDefence = 85
    , _baseSpeed = 100
    , _weight = 199.5
    , _genderRatio = male7to1
    }

  asSpecies Blastoise = PokemonSpecies {
    _speciesName = "Blastoise"
    , _typeOf = mkTypeOfPokemon $ Water :| []
    , _baseHp = 79
    , _baseAttack = 83
    , _baseDefence = 100
    , _baseSAttack = 85
    , _baseSDefence = 105
    , _baseSpeed = 78
    , _weight = 85.5
    , _genderRatio = male7to1
    }

  asSpecies Aerodactyl = PokemonSpecies {
    _speciesName = "Aerodactyl"
    , _typeOf = mkTypeOfPokemon $ Rock :| [Flying]
    , _baseHp = 80
    , _baseAttack = 105
    , _baseDefence = 65
    , _baseSAttack = 60
    , _baseSDefence = 75
    , _baseSpeed = 130
    , _weight = 59.0
    , _genderRatio = male7to1
    }

  asSpecies Machamp = PokemonSpecies {
    _speciesName = "Machamp"
    , _typeOf = mkTypeOfPokemon $ Fighting :| []
    , _baseHp = 90
    , _baseAttack = 130
    , _baseDefence = 80
    , _baseSAttack = 65
    , _baseSDefence = 85
    , _baseSpeed = 55
    , _weight = 130.0
    , _genderRatio = male7to1
    }

  asSpecies Alakazam = PokemonSpecies {
    _speciesName = "Alakazam"
    , _typeOf = mkTypeOfPokemon $ Psychic :| []
    , _baseHp = 55
    , _baseAttack = 50
    , _baseDefence = 45
    , _baseSAttack = 135
    , _baseSDefence = 95
    , _baseSpeed = 120
    , _weight = 48.0
    , _genderRatio = male7to1
    }

  asSpecies Exeggutor = PokemonSpecies {
    _speciesName = "Exeggutor"
    , _typeOf = mkTypeOfPokemon $ Grass :| [Psychic]
    , _baseHp = 95
    , _baseAttack = 95
    , _baseDefence = 85
    , _baseSAttack = 125
    , _baseSDefence = 75
    , _baseSpeed = 55
    , _weight = 120.0
    , _genderRatio = male7to1
    }

  asSpecies Arcanine = PokemonSpecies {
    _speciesName = "Arcanine"
    , _typeOf = mkTypeOfPokemon $ Fire :| []
    , _baseHp = 90
    , _baseAttack = 110
    , _baseDefence = 80
    , _baseSAttack = 100
    , _baseSDefence = 80
    , _baseSpeed = 95
    , _weight = 155.0
    , _genderRatio = male7to1
    }

  asSpecies Gyarados = PokemonSpecies {
    _speciesName = "Gyarados"
    , _typeOf = mkTypeOfPokemon $ Water :| [Flying]
    , _baseHp = 95
    , _baseAttack = 125
    , _baseDefence = 79
    , _baseSAttack = 60
    , _baseSDefence = 100
    , _baseSpeed = 81
    , _weight = 235.0
    , _genderRatio = male7to1
    }

  asSpecies Dragonite = PokemonSpecies {
    _speciesName = "Dragonite"
    , _typeOf = mkTypeOfPokemon $ Dragon :| [Flying]
    , _baseHp = 91
    , _baseAttack = 134
    , _baseDefence = 95
    , _baseSAttack = 100
    , _baseSDefence = 100
    , _baseSpeed = 80
    , _weight = 210.0
    , _genderRatio = male7to1
    }

  asSpecies Salamence = PokemonSpecies {
    _speciesName = "Salamence"
    , _typeOf = mkTypeOfPokemon $ Dragon :| [Flying]
    , _baseHp = 95
    , _baseAttack = 135
    , _baseDefence = 80
    , _baseSAttack = 110
    , _baseSDefence = 80
    , _baseSpeed = 100
    , _weight = 102.6
    , _genderRatio = male7to1
    }

  asSpecies Kingdra = PokemonSpecies {
    _speciesName = "Kingdra"
    , _typeOf = mkTypeOfPokemon $ Water :| [Dragon]
    , _baseHp = 75
    , _baseAttack = 95
    , _baseDefence = 95
    , _baseSAttack = 95
    , _baseSDefence = 95
    , _baseSpeed = 85
    , _weight = 152.0
    , _genderRatio = male7to1
    }

  asSpecies Haxorus = PokemonSpecies {
    _speciesName = "Haxorus"
    , _typeOf = mkTypeOfPokemon $ Dragon :| []
    , _baseHp = 76
    , _baseAttack = 147
    , _baseDefence = 90
    , _baseSAttack = 60
    , _baseSDefence = 70
    , _baseSpeed = 97
    , _weight = 105.5
    , _genderRatio = male7to1
    }

  asSpecies Hydreigon = PokemonSpecies {
    _speciesName = "Hydreigon"
    , _typeOf = mkTypeOfPokemon $ Dark :| [Dragon]
    , _baseHp = 92
    , _baseAttack = 105
    , _baseDefence = 90
    , _baseSAttack = 125
    , _baseSDefence = 90
    , _baseSpeed = 98
    , _weight = 160.0
    , _genderRatio = male7to1
    }

  asSpecies Flygon = PokemonSpecies {
    _speciesName = "Flygon"
    , _typeOf = mkTypeOfPokemon $ Ground :| [Dragon]
    , _baseHp = 80
    , _baseAttack = 100
    , _baseDefence = 80
    , _baseSAttack = 80
    , _baseSDefence = 80
    , _baseSpeed = 100
    , _weight = 82.0
    , _genderRatio = male7to1
    }

  asSpecies Metagross = PokemonSpecies {
    _speciesName = "Metagross"
    , _typeOf = mkTypeOfPokemon $ Steel :| [Psychic]
    , _baseHp = 80
    , _baseAttack = 135
    , _baseDefence = 130
    , _baseSAttack = 95
    , _baseSDefence = 90
    , _baseSpeed = 70
    , _weight = 550.0
    , _genderRatio = male7to1
    }

  asSpecies Aggron = PokemonSpecies {
    _speciesName = "Aggron"
    , _typeOf = mkTypeOfPokemon $ Steel :| [Rock]
    , _baseHp = 70
    , _baseAttack = 110
    , _baseDefence = 180
    , _baseSAttack = 60
    , _baseSDefence = 60
    , _baseSpeed = 50
    , _weight = 360.0
    , _genderRatio = male7to1
    }

  asSpecies Excadrill = PokemonSpecies {
    _speciesName = "Excadrill"
    , _typeOf = mkTypeOfPokemon $ Ground :| [Steel]
    , _baseHp = 110
    , _baseAttack = 135
    , _baseDefence = 60
    , _baseSAttack = 50
    , _baseSDefence = 65
    , _baseSpeed = 88
    , _weight = 40.4
    , _genderRatio = male7to1
    }

  asSpecies Archeops = PokemonSpecies {
    _speciesName = "Archeops"
    , _typeOf = mkTypeOfPokemon $ Rock :| [Flying]
    , _baseHp = 75
    , _baseAttack = 140
    , _baseDefence = 65
    , _baseSAttack = 112
    , _baseSDefence = 65
    , _baseSpeed = 110
    , _weight = 32.0
    , _genderRatio = male7to1
    }

  asSpecies Cradily = PokemonSpecies {
    _speciesName = "Cradily"
    , _typeOf = mkTypeOfPokemon $ Rock :| [Grass]
    , _baseHp = 86
    , _baseAttack = 81
    , _baseDefence = 97
    , _baseSAttack = 81
    , _baseSDefence = 107
    , _baseSpeed = 43
    , _weight = 60.4
    , _genderRatio = male7to1
    }

  asSpecies Armaldo = PokemonSpecies {
    _speciesName = "Armaldo"
    , _typeOf = mkTypeOfPokemon $ Rock :| [Bug]
    , _baseHp = 75
    , _baseAttack = 125
    , _baseDefence = 100
    , _baseSAttack = 70
    , _baseSDefence = 80
    , _baseSpeed = 45
    , _weight = 68.2
    , _genderRatio = male7to1
    }

  asSpecies Milotic = PokemonSpecies {
    _speciesName = "Milotic"
    , _typeOf = mkTypeOfPokemon $ Water :| []
    , _baseHp = 95
    , _baseAttack = 60
    , _baseDefence = 79
    , _baseSAttack = 100
    , _baseSDefence = 125
    , _baseSpeed = 81
    , _weight = 162.0
    , _genderRatio = male7to1
    }

  asSpecies Sharpedo = PokemonSpecies {
    _speciesName = "Sharpedo"
    , _typeOf = mkTypeOfPokemon $ Water :| [Dark]
    , _baseHp = 70
    , _baseAttack = 120
    , _baseDefence = 40
    , _baseSAttack = 95
    , _baseSDefence = 40
    , _baseSpeed = 95
    , _weight = 88.8
    , _genderRatio = male7to1
    }

  asSpecies Walrein = PokemonSpecies {
    _speciesName = "Walrein"
    , _typeOf = mkTypeOfPokemon $ Ice :| [Water]
    , _baseHp = 110
    , _baseAttack = 80
    , _baseDefence = 90
    , _baseSAttack = 95
    , _baseSDefence = 90
    , _baseSpeed = 65
    , _weight = 150.6
    , _genderRatio = male7to1
    }

  asSpecies Ludicolo = PokemonSpecies {
    _speciesName = "Ludicolo"
    , _typeOf = mkTypeOfPokemon $ Water :| [Grass]
    , _baseHp = 80
    , _baseAttack = 70
    , _baseDefence = 70
    , _baseSAttack = 90
    , _baseSDefence = 100
    , _baseSpeed = 70
    , _weight = 55.0
    , _genderRatio = male7to1
    }

  asSpecies Swampert = PokemonSpecies {
    _speciesName = "Swampert"
    , _typeOf = mkTypeOfPokemon $ Water :| [Ground]
    , _baseHp = 100
    , _baseAttack = 110
    , _baseDefence = 90
    , _baseSAttack = 85
    , _baseSDefence = 90
    , _baseSpeed = 60
    , _weight = 81.9
    , _genderRatio = male7to1
    }

  asSpecies Starmie = PokemonSpecies {
    _speciesName = "Starmie"
    , _typeOf = mkTypeOfPokemon $ Water :| [Psychic]
    , _baseHp = 60
    , _baseAttack = 75
    , _baseDefence = 85
    , _baseSAttack = 100
    , _baseSDefence = 85
    , _baseSpeed = 115
    , _weight = 80.0
    , _genderRatio = male7to1
    }

  asSpecies Garchomp = PokemonSpecies {
    _speciesName = "Garchomp"
    , _typeOf = mkTypeOfPokemon $ Dragon :| [Ground]
    , _baseHp = 108
    , _baseAttack = 130
    , _baseDefence = 95
    , _baseSAttack = 80
    , _baseSDefence = 85
    , _baseSpeed = 102
    , _weight = 95.0
    , _genderRatio = male7to1
    } 

  asSpecies Spiritomb = PokemonSpecies {
    _speciesName = "Spiritomb"
    , _typeOf = mkTypeOfPokemon $ Ghost :| [Dark]
    , _baseHp = 50
    , _baseAttack = 92
    , _baseDefence = 108
    , _baseSAttack = 92
    , _baseSDefence = 108
    , _baseSpeed = 35
    , _weight = 108.0
    , _genderRatio = male7to1
    }

  asSpecies Roserade = PokemonSpecies {
    _speciesName = "Roserade"
    , _typeOf = mkTypeOfPokemon $ Grass :| [Poison]
    , _baseHp = 60
    , _baseAttack = 70
    , _baseDefence = 65
    , _baseSAttack = 125
    , _baseSDefence = 105
    , _baseSpeed = 90
    , _weight = 14.5
    , _genderRatio = male7to1
    } 

  asSpecies Togekiss = PokemonSpecies {
    _speciesName = "Togekiss"
    , _typeOf = mkTypeOfPokemon $ Fairy :| [Flying]
    , _baseHp = 85
    , _baseAttack = 50
    , _baseDefence = 95
    , _baseSAttack = 120
    , _baseSDefence = 115
    , _baseSpeed = 80
    , _weight = 38.0
    , _genderRatio = male7to1
    }

  asSpecies Lucario = PokemonSpecies {
    _speciesName = "Lucario"
    , _typeOf = mkTypeOfPokemon $ Fighting :| [Steel]
    , _baseHp = 70
    , _baseAttack = 110
    , _baseDefence = 70
    , _baseSAttack = 115
    , _baseSDefence = 70
    , _baseSpeed = 90
    , _weight = 54.0
    , _genderRatio = male7to1
    }

  asSpecies Glaceon = PokemonSpecies {
    _speciesName = "Glaceon"
    , _typeOf = mkTypeOfPokemon $ Ice :| []
    , _baseHp = 65
    , _baseAttack = 60
    , _baseDefence = 110
    , _baseSAttack = 130
    , _baseSDefence = 95
    , _baseSpeed = 65
    , _weight = 25.9
    , _genderRatio = male7to1
    }

  asSpecies Volcarona = PokemonSpecies {
    _speciesName = "Volcarona"
    , _typeOf = mkTypeOfPokemon $ Bug :| [Fire]
    , _baseHp = 85
    , _baseAttack = 60
    , _baseDefence = 65
    , _baseSAttack = 135
    , _baseSDefence = 105
    , _baseSpeed = 100
    , _weight = 46.0
    , _genderRatio = male7to1
    }

  asSpecies Conkeldurr = PokemonSpecies {
    _speciesName = "Conkeldurr"
    , _typeOf = mkTypeOfPokemon $ Fighting :| []
    , _baseHp = 105
    , _baseAttack = 140
    , _baseDefence = 95
    , _baseSAttack = 55
    , _baseSDefence = 65
    , _baseSpeed = 45
    , _weight = 87.0
    , _genderRatio = male7to1
    }

  asSpecies Reuniclus = PokemonSpecies {
    _speciesName = "Reuniclus"
    , _typeOf = mkTypeOfPokemon $ Psychic :| []
    , _baseHp = 110
    , _baseAttack = 65
    , _baseDefence = 75
    , _baseSAttack = 125
    , _baseSDefence = 85
    , _baseSpeed = 30
    , _weight = 20.1
    , _genderRatio = male7to1
    }

  asSpecies Krookodile = PokemonSpecies {
    _speciesName = "Krookodile"
    , _typeOf = mkTypeOfPokemon $ Ground :| [Dark]
    , _baseHp = 95
    , _baseAttack = 117
    , _baseDefence = 80
    , _baseSAttack = 65
    , _baseSDefence = 70
    , _baseSpeed = 92
    , _weight = 96.3
    , _genderRatio = male7to1
    }

  asSpecies Chandelure = PokemonSpecies {
    _speciesName = "Chandelure"
    , _typeOf = mkTypeOfPokemon $ Ghost :| [Fire]
    , _baseHp = 60
    , _baseAttack = 55
    , _baseDefence = 90
    , _baseSAttack = 145
    , _baseSDefence = 90
    , _baseSpeed = 80
    , _weight = 34.3
    , _genderRatio = male7to1
    }

  asSpecies Braviary = PokemonSpecies {
    _speciesName = "Braviary"
    , _typeOf = mkTypeOfPokemon $ Normal :| [Flying]
    , _baseHp = 100
    , _baseAttack = 123
    , _baseDefence = 75
    , _baseSAttack = 57
    , _baseSDefence = 75
    , _baseSpeed = 80
    , _weight = 41.0
    , _genderRatio = male7to1
    }

male7to1 :: Gender -> Double
male7to1 Male = 0.875
male7to1 Female = 0.125
male7to1 Genderless = 0
