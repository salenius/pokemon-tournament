{-# LANGUAGE TemplateHaskell, GADTs #-}

module Domain.Entity.Pokemon.Species where

import Domain.Attribute.TypeOf
import Domain.Attribute.Gender
import Domain.Attribute.Ability
import Control.Lens

class (Eq pkmn, Show pkmn) => PokemonSpecies pkmn where
  baseHp :: pkmn -> Int
  baseAttack :: pkmn -> Int
  baseDefence :: pkmn -> Int
  baseSAttack :: pkmn -> Int
  baseSDefence :: pkmn -> Int
  baseSpeed :: pkmn -> Int
  weight :: pkmn -> Double
  typeOfPokemon :: pkmn -> [TypeOf]
  genderRatio :: pkmn -> GenderRatio
  possibleAbilities :: pkmn -> [Ability]
  revertToForm :: pkmn -> pkmn
  isPikachu :: pkmn -> Bool
  isPikachu _ = False

data BuiltInSpecies =
  Pikachu
  | Lapras
  | Snorlax
  | Venusaur
  | Charizard
  | Blastoise
  | Aerodactyl
  | Machamp
  | Alakazam
  | Exeggutor
  | Arcanine
  | Gyarados
  | Dragonite
  | Salamence
  | Kingdra
  | Haxorus
  | Hydreigon
  | Flygon
  | Metagross
  | Aggron
  | Excadrill
  | Archeops
  | Cradily
  | Armaldo
  | Milotic
  | Sharpedo
  | Walrein
  | Ludicolo
  | Swampert
  | Starmie
  | Garchomp
  | Spiritomb
  | Roserade
  | Togekiss
  | Lucario
  | Glaceon
  | Volcarona
  | Conkeldurr
  | Reuniclus
  | Krookodile
  | Chandelure
  | Braviary
  deriving (Eq,Show,Read,Ord,Enum)


makeLenses ''BuiltInSpecies

--

instance PokemonSpecies BuiltInSpecies where
  revertToForm = id

  baseAttack Aerodactyl = 105
  baseAttack Aggron = 110
  baseAttack Alakazam = 50
  baseAttack Arcanine = 110
  baseAttack Archeops = 140
  baseAttack Armaldo = 125
  baseAttack Blastoise = 83
  baseAttack Braviary = 123
  baseAttack Chandelure = 55
  baseAttack Charizard = 84
  baseAttack Conkeldurr = 140
  baseAttack Cradily = 81
  baseAttack Dragonite = 134
  baseAttack Excadrill = 135
  baseAttack Exeggutor = 95
  baseAttack Flygon = 100
  baseAttack Garchomp = 130
  baseAttack Glaceon = 60
  baseAttack Gyarados = 125
  baseAttack Haxorus = 147
  baseAttack Hydreigon = 105
  baseAttack Kingdra = 95
  baseAttack Krookodile = 117
  baseAttack Lapras = 85
  baseAttack Lucario = 110
  baseAttack Ludicolo = 70
  baseAttack Machamp = 130
  baseAttack Metagross = 135
  baseAttack Milotic = 60
  baseAttack Pikachu = 55
  baseAttack Reuniclus = 65
  baseAttack Roserade = 70
  baseAttack Salamence = 135
  baseAttack Sharpedo = 120
  baseAttack Snorlax = 110
  baseAttack Spiritomb = 92
  baseAttack Starmie = 75
  baseAttack Swampert = 110
  baseAttack Togekiss = 50
  baseAttack Venusaur = 82
  baseAttack Volcarona = 60
  baseAttack Walrein = 80
  
  baseDefence Aerodactyl = 65
  baseDefence Aggron = 180
  baseDefence Alakazam = 45
  baseDefence Arcanine = 80
  baseDefence Archeops = 65
  baseDefence Armaldo = 100
  baseDefence Blastoise = 100
  baseDefence Braviary = 75
  baseDefence Chandelure = 90
  baseDefence Charizard = 78
  baseDefence Conkeldurr = 95
  baseDefence Cradily = 97
  baseDefence Dragonite = 95
  baseDefence Excadrill = 60
  baseDefence Exeggutor = 85
  baseDefence Flygon = 80
  baseDefence Garchomp = 95
  baseDefence Glaceon = 110
  baseDefence Gyarados = 79
  baseDefence Haxorus = 90
  baseDefence Hydreigon = 90
  baseDefence Kingdra = 95
  baseDefence Krookodile = 80
  baseDefence Lapras = 80
  baseDefence Lucario = 70
  baseDefence Ludicolo = 70
  baseDefence Machamp = 80
  baseDefence Metagross = 130
  baseDefence Milotic = 79
  baseDefence Pikachu = 40
  baseDefence Reuniclus = 75
  baseDefence Roserade = 65
  baseDefence Salamence = 80
  baseDefence Sharpedo = 40
  baseDefence Snorlax = 65
  baseDefence Spiritomb = 108
  baseDefence Starmie = 85
  baseDefence Swampert = 90
  baseDefence Togekiss = 95
  baseDefence Venusaur = 83
  baseDefence Volcarona = 65
  baseDefence Walrein = 90
  
  baseHp Aerodactyl = 80
  baseHp Aggron = 70
  baseHp Alakazam = 55
  baseHp Arcanine = 90
  baseHp Archeops = 75
  baseHp Armaldo = 75
  baseHp Blastoise = 79
  baseHp Braviary = 100
  baseHp Chandelure = 60
  baseHp Charizard = 78
  baseHp Conkeldurr = 105
  baseHp Cradily = 86
  baseHp Dragonite = 91
  baseHp Excadrill = 110
  baseHp Exeggutor = 95
  baseHp Flygon = 80
  baseHp Garchomp = 108
  baseHp Glaceon = 65
  baseHp Gyarados = 95
  baseHp Haxorus = 76
  baseHp Hydreigon = 92
  baseHp Kingdra = 75
  baseHp Krookodile = 95
  baseHp Lapras = 130
  baseHp Lucario = 70
  baseHp Ludicolo = 80
  baseHp Machamp = 90
  baseHp Metagross = 80
  baseHp Milotic = 95
  baseHp Pikachu = 35
  baseHp Reuniclus = 110
  baseHp Roserade = 60
  baseHp Salamence = 95
  baseHp Sharpedo = 70
  baseHp Snorlax = 160
  baseHp Spiritomb = 50
  baseHp Starmie = 60
  baseHp Swampert = 100
  baseHp Togekiss = 85
  baseHp Venusaur = 80
  baseHp Volcarona = 85
  baseHp Walrein = 110
  
  baseSAttack Aerodactyl = 60
  baseSAttack Aggron = 60
  baseSAttack Alakazam = 135
  baseSAttack Arcanine = 100
  baseSAttack Archeops = 112
  baseSAttack Armaldo = 70
  baseSAttack Blastoise = 85
  baseSAttack Braviary = 57
  baseSAttack Chandelure = 145
  baseSAttack Charizard = 109
  baseSAttack Conkeldurr = 55
  baseSAttack Cradily = 81
  baseSAttack Dragonite = 100
  baseSAttack Excadrill = 50
  baseSAttack Exeggutor = 125
  baseSAttack Flygon = 80
  baseSAttack Garchomp = 80
  baseSAttack Glaceon = 130
  baseSAttack Gyarados = 60
  baseSAttack Haxorus = 60
  baseSAttack Hydreigon = 125
  baseSAttack Kingdra = 95
  baseSAttack Krookodile = 65
  baseSAttack Lapras = 85
  baseSAttack Lucario = 115
  baseSAttack Ludicolo = 90
  baseSAttack Machamp = 65
  baseSAttack Metagross = 95
  baseSAttack Milotic = 100
  baseSAttack Pikachu = 50
  baseSAttack Reuniclus = 125
  baseSAttack Roserade = 125
  baseSAttack Salamence = 110
  baseSAttack Sharpedo = 95
  baseSAttack Snorlax = 65
  baseSAttack Spiritomb = 92
  baseSAttack Starmie = 100
  baseSAttack Swampert = 85
  baseSAttack Togekiss = 120
  baseSAttack Venusaur = 100
  baseSAttack Volcarona = 135
  baseSAttack Walrein = 95
  
  baseSDefence Aerodactyl = 75
  baseSDefence Aggron = 60
  baseSDefence Alakazam = 95
  baseSDefence Arcanine = 80
  baseSDefence Archeops = 65
  baseSDefence Armaldo = 80
  baseSDefence Blastoise = 105
  baseSDefence Braviary = 75
  baseSDefence Chandelure = 90
  baseSDefence Charizard = 85
  baseSDefence Conkeldurr = 65
  baseSDefence Cradily = 107
  baseSDefence Dragonite = 100
  baseSDefence Excadrill = 65
  baseSDefence Exeggutor = 75
  baseSDefence Flygon = 80
  baseSDefence Garchomp = 85
  baseSDefence Glaceon = 95
  baseSDefence Gyarados = 100
  baseSDefence Haxorus = 70
  baseSDefence Hydreigon = 90
  baseSDefence Kingdra = 95
  baseSDefence Krookodile = 70
  baseSDefence Lapras = 95
  baseSDefence Lucario = 70
  baseSDefence Ludicolo = 100
  baseSDefence Machamp = 85
  baseSDefence Metagross = 90
  baseSDefence Milotic = 125
  baseSDefence Pikachu = 50
  baseSDefence Reuniclus = 85
  baseSDefence Roserade = 105
  baseSDefence Salamence = 80
  baseSDefence Sharpedo = 40
  baseSDefence Snorlax = 110
  baseSDefence Spiritomb = 108
  baseSDefence Starmie = 85
  baseSDefence Swampert = 90
  baseSDefence Togekiss = 115
  baseSDefence Venusaur = 100
  baseSDefence Volcarona = 105
  baseSDefence Walrein = 90
  
  baseSpeed Aerodactyl = 130
  baseSpeed Aggron = 50
  baseSpeed Alakazam = 120
  baseSpeed Arcanine = 95
  baseSpeed Archeops = 110
  baseSpeed Armaldo = 45
  baseSpeed Blastoise = 78
  baseSpeed Braviary = 80
  baseSpeed Chandelure = 80
  baseSpeed Charizard = 100
  baseSpeed Conkeldurr = 45
  baseSpeed Cradily = 43
  baseSpeed Dragonite = 80
  baseSpeed Excadrill = 88
  baseSpeed Exeggutor = 55
  baseSpeed Flygon = 100
  baseSpeed Garchomp = 102
  baseSpeed Glaceon = 65
  baseSpeed Gyarados = 81
  baseSpeed Haxorus = 97
  baseSpeed Hydreigon = 98
  baseSpeed Kingdra = 85
  baseSpeed Krookodile = 92
  baseSpeed Lapras = 60
  baseSpeed Lucario = 90
  baseSpeed Ludicolo = 70
  baseSpeed Machamp = 55
  baseSpeed Metagross = 70
  baseSpeed Milotic = 81
  baseSpeed Pikachu = 90
  baseSpeed Reuniclus = 30
  baseSpeed Roserade = 90
  baseSpeed Salamence = 100
  baseSpeed Sharpedo = 95
  baseSpeed Snorlax = 30
  baseSpeed Spiritomb = 35
  baseSpeed Starmie = 115
  baseSpeed Swampert = 60
  baseSpeed Togekiss = 80
  baseSpeed Venusaur = 80
  baseSpeed Volcarona = 100
  baseSpeed Walrein = 65
  
  genderRatio Aerodactyl = Male7to1
  genderRatio Aggron = EvenRatio
  genderRatio Alakazam = Male3to1
  genderRatio Arcanine = Male3to1
  genderRatio Archeops = Male7to1
  genderRatio Armaldo = Male7to1
  genderRatio Blastoise = Male7to1
  genderRatio Braviary = AlwaysMale
  genderRatio Chandelure = EvenRatio
  genderRatio Charizard = Male7to1
  genderRatio Conkeldurr = Male3to1
  genderRatio Cradily = Male7to1
  genderRatio Dragonite = EvenRatio
  genderRatio Excadrill = EvenRatio
  genderRatio Exeggutor = EvenRatio
  genderRatio Flygon = EvenRatio
  genderRatio Garchomp = EvenRatio
  genderRatio Glaceon = Male7to1
  genderRatio Gyarados = EvenRatio
  genderRatio Haxorus = EvenRatio
  genderRatio Hydreigon = EvenRatio
  genderRatio Kingdra = EvenRatio
  genderRatio Krookodile = EvenRatio
  genderRatio Lapras = EvenRatio
  genderRatio Lucario = Male7to1
  genderRatio Ludicolo = EvenRatio
  genderRatio Machamp = Male3to1
  genderRatio Metagross = AlwaysGenderless
  genderRatio Milotic = EvenRatio
  genderRatio Pikachu = Male7to1
  genderRatio Reuniclus = EvenRatio
  genderRatio Roserade = EvenRatio
  genderRatio Salamence = Male7to1
  genderRatio Sharpedo = Male7to1
  genderRatio Snorlax = Male7to1
  genderRatio Spiritomb = Male7to1
  genderRatio Starmie = AlwaysGenderless
  genderRatio Swampert = Male7to1
  genderRatio Togekiss = Male7to1
  genderRatio Venusaur = Male7to1
  genderRatio Volcarona = Male7to1
  genderRatio Walrein = Male7to1
  
  typeOfPokemon Aerodactyl = [Rock,Flying]
  typeOfPokemon Aggron = [Steel,Rock]
  typeOfPokemon Alakazam = [Psychic]
  typeOfPokemon Arcanine = [Fire]
  typeOfPokemon Archeops = [Rock,Flying]
  typeOfPokemon Armaldo = [Rock,Bug]
  typeOfPokemon Blastoise = [Water]
  typeOfPokemon Braviary = [Normal,Flying]
  typeOfPokemon Chandelure = [Ghost,Fire]
  typeOfPokemon Charizard = [Fire,Flying]
  typeOfPokemon Conkeldurr = [Fighting]
  typeOfPokemon Cradily = [Rock,Grass]
  typeOfPokemon Dragonite = [Dragon,Flying]
  typeOfPokemon Excadrill = [Ground,Steel]
  typeOfPokemon Exeggutor = [Grass,Psychic]
  typeOfPokemon Flygon = [Ground,Dragon]
  typeOfPokemon Garchomp = [Dragon,Ground]
  typeOfPokemon Glaceon = [Ice]
  typeOfPokemon Gyarados = [Water,Flying]
  typeOfPokemon Haxorus = [Dragon]
  typeOfPokemon Hydreigon = [Dark,Dragon]
  typeOfPokemon Kingdra = [Water,Dragon]
  typeOfPokemon Krookodile = [Ground,Dark]
  typeOfPokemon Lapras = [Water,Ice]
  typeOfPokemon Lucario = [Fighting,Steel]
  typeOfPokemon Ludicolo = [Water,Grass]
  typeOfPokemon Machamp = [Fighting]
  typeOfPokemon Metagross = [Steel,Psychic]
  typeOfPokemon Milotic = [Water]
  typeOfPokemon Pikachu = [Electric]
  typeOfPokemon Reuniclus = [Psychic]
  typeOfPokemon Roserade = [Grass,Poison]
  typeOfPokemon Salamence = [Dragon,Flying]
  typeOfPokemon Sharpedo = [Water,Dark]
  typeOfPokemon Snorlax = [Normal]
  typeOfPokemon Spiritomb = [Ghost,Dark]
  typeOfPokemon Starmie = [Water,Psychic]
  typeOfPokemon Swampert = [Water,Ground]
  typeOfPokemon Togekiss = [Fairy,Flying]
  typeOfPokemon Venusaur = [Grass,Poison]
  typeOfPokemon Volcarona = [Bug,Fire]
  typeOfPokemon Walrein = [Ice,Water]
  
  weight Aerodactyl = 59.0
  weight Aggron = 360.0
  weight Alakazam = 48.0
  weight Arcanine = 155.0
  weight Archeops = 32.0
  weight Armaldo = 68.2
  weight Blastoise = 85.5
  weight Braviary = 41.0
  weight Chandelure = 34.3
  weight Charizard = 199.5
  weight Conkeldurr = 87.0
  weight Cradily = 60.4
  weight Dragonite = 210.0
  weight Excadrill = 40.4
  weight Exeggutor = 120.0
  weight Flygon = 82.0
  weight Garchomp = 95.0
  weight Glaceon = 25.9
  weight Gyarados = 235.0
  weight Haxorus = 105.5
  weight Hydreigon = 160.0
  weight Kingdra = 152.0
  weight Krookodile = 96.3
  weight Lapras = 220.0
  weight Lucario = 54.0
  weight Ludicolo = 55.0
  weight Machamp = 130.0
  weight Metagross = 550.0
  weight Milotic = 162.0
  weight Pikachu = 6.0
  weight Reuniclus = 20.1
  weight Roserade = 14.5
  weight Salamence = 102.6
  weight Sharpedo = 88.8
  weight Snorlax = 460.0
  weight Spiritomb = 108.0
  weight Starmie = 80.0
  weight Swampert = 81.9
  weight Togekiss = 38.0
  weight Venusaur = 100.0
  weight Volcarona = 46.0
  weight Walrein = 150.6
  
  possibleAbilities Pikachu = [Static]
  possibleAbilities Lapras = [WaterAbsorb,ShellArmor]
  possibleAbilities Snorlax = [Immunity,ThickFat]
  possibleAbilities Venusaur = [Overgrow]
  possibleAbilities Charizard = [Blaze]
  possibleAbilities Blastoise = [Torrent]
  possibleAbilities Aerodactyl = [RockHead,Pressure]
  possibleAbilities Machamp = [Guts,NoGuard]
  possibleAbilities Alakazam = [Synchronize,InnerFocus]
  possibleAbilities Exeggutor = [Chlorophyll]
  possibleAbilities Arcanine = [Intimidate,FlashFire]
  possibleAbilities Gyarados = [Intimidate]
  possibleAbilities Dragonite = [InnerFocus]
  possibleAbilities Salamence = [Intimidate]
  possibleAbilities Kingdra = [SwiftSwim,Sniper]
  possibleAbilities Haxorus = [Rivalry,MoldBreaker]
  possibleAbilities Hydreigon = [Levitate]
  possibleAbilities Flygon = [Levitate]
  possibleAbilities Metagross = [ClearBody]
  possibleAbilities Aggron = [Sturdy,RockHead]
  possibleAbilities Excadrill = [SandRush,SandForce]
  possibleAbilities Archeops = [Defeatist]
  possibleAbilities Cradily = [SuctionCups]
  possibleAbilities Armaldo = [BattleArmor]
  possibleAbilities Milotic = [MarvelScale]
  possibleAbilities Sharpedo = [RoughSkin]
  possibleAbilities Walrein = [ThickFat,IceBody]
  possibleAbilities Ludicolo = [SwiftSwim,RainDish]
  possibleAbilities Swampert = [Torrent]
  possibleAbilities Starmie = [Illuminate,NaturalCure]
  possibleAbilities Garchomp = [SandVeil]
  possibleAbilities Spiritomb = [Pressure]
  possibleAbilities Roserade = [NaturalCure,PoisonPoint]
  possibleAbilities Togekiss = [Hustle,SereneGrace]
  possibleAbilities Lucario = [Steadfast,InnerFocus]
  possibleAbilities Glaceon = [SnowCloak]
  possibleAbilities Volcarona = [FlameBody]
  possibleAbilities Conkeldurr = [Guts,SheerForce]
  possibleAbilities Reuniclus = [Overcoat,MagicGuard]
  possibleAbilities Krookodile = [Intimidate,Moxie]
  possibleAbilities Chandelure = [FlashFire,FlameBody]
  possibleAbilities Braviary = [KeenEye,SheerForce]
  
  isPikachu Pikachu = True
  isPikachu _ = False
  
