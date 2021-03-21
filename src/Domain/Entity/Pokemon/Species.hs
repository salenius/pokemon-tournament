{-# LANGUAGE TemplateHaskell, GADTs, ViewPatterns, PatternSynonyms #-}

module Domain.Entity.Pokemon.Species where

import Domain.Attribute.TypeOf
import Domain.Attribute.Gender
import Domain.Attribute.Ability
import Domain.Attribute.Statistic
import Control.Lens
import Data.Char

class PokemonSpecies pkmn where
  baseStat :: BaseStat -> pkmn -> Int
  weight :: pkmn -> Double
  typeOfPokemon :: pkmn -> [TypeOf]
  ability :: pkmn -> Ability
  genderRatio :: pkmn -> Gender -> Double
  

data Pikachu = Pikachu'withStatic deriving (Eq,Show,Read)

data Lapras =
  Lapras'withWaterAbsorb
  | Lapras'withShellArmor
  deriving (Eq,Show,Read)

data Snorlax =
  Snorlax'withImmunity
  | Snorlax'withThickFat
  | GigantamaxSnorlax
  deriving (Eq,Show,Read)

data Venusaur =
  Venusaur'withOvergrow
  | MegaVenusaur
  deriving (Eq,Show,Read)

data Charizard =
  Charizard'withBlaze
  | MegaCharizardX
  | MegaCharizardY
  deriving (Eq,Show,Read)

data Blastoise =
  Blastoise'withTorrent
  | MegaBlastoise
  deriving (Eq,Show,Read)

--

instance PokemonSpecies Pikachu where
  baseStat BaseHP _ = 35
  baseStat BaseAttack _ = 55
  baseStat BaseDefence _ = 40
  baseStat BaseSAttack  _ = 50
  baseStat BaseSDefence _ = 50
  baseStat BaseSpeed _ = 90
  ability _ = Static
  weight _ = 6.0
  typeOfPokemon _ = [Electric]
  genderRatio _ = male7to1

instance PokemonSpecies Lapras where
  baseStat BaseHP _ = 130
  baseStat BaseAttack _ = 85
  baseStat BaseDefence _ = 80
  baseStat BaseSAttack _ = 85
  baseStat BaseSdefence _ = 95
  baseStat BaseSpeed _ = 60
  weight _ = 220.0
  typeOfPokemon = [Water,Ice]
  ability Lapras'withWaterAbsorb = WaterAbsorb
  ability Lapras'withShellArmor = ShellArmor
  genderRatio = evenRatio

instance PokemonSpecies Snorlax where
  baseStat BaseAttack _ = 110
  baseStat BaseDefence _ = 65
  baseStat BaseHP _ = 160
  baseStat BaseSAttack _ = 65
  baseStat BaseSDefence _ = 110
  baseStat BaseSpeed _ = 30
  genderRatio _ = male7to1
  ability Snorlax'withImmunity = Immunity
  ability Snorlax'withThickFat = ThickFat
  typeOfPokemon _ = [Normal]
  weight _ = 460.0

instance PokemonSpecies Venusaur where
  baseStat BaseAttack _ = 82
  baseStat BaseDefence _ = 83
  baseStat BaseHP _ = 80
  baseStat BaseSAttack _ = 100
  baseStat BaseSDefence _ = 100
  baseStat BaseSpeed _ = 80
  genderRatio _ = male7to1
  ability _ = Overgrow
  typeOfPokemon _ = [Grass,Poison]
  weight _ = 100.0

instance PokemonSpecies Charizard where
  baseStat BaseAttack _ = 84
  baseStat BaseDefence _ = 78
  baseStat BaseHP _ = 78
  baseStat BaseSAttack _ = 109
  baseStat BaseSDefence _ = 85
  baseStat BaseSpeed _ = 100
  genderRatio _ = male7to1
  ability _ = Blaze
  typeOfPokemon _ = [Fire,Flying]
  weight _ = 90.5

instance PokemonSpecies Blastoise where
  baseStat BaseAttack _ = 83
  baseStat BaseDefence _ = 100
  baseStat BaseDefence Lapras = 80
  baseStat BaseHP _ = 79
  baseStat BaseSAttack _ = 85
  baseStat BaseSDefence _ = 105
  baseStat BaseSpeed _ = 78
  genderRatio _ = male7to1
  ability _ = Torrent
  typeOfPokemon _ = [Water]
  weight _ = 85.5

-----

instance PokemonAttributes BuiltInPokemon where
  baseStat stat (RedPkmn pkmn) = baseStat stat pkmn
  baseStat stat (BluePkmn pkmn) = baseStat stat pkmn
  baseStat stat (LancePkmn pkmn) = baseStat stat pkmn
  baseStat stat (StevenPkmn pkmn) = baseStat stat pkmn
  baseStat stat (WallacePkmn pkmn) = baseStat stat pkmn
  baseStat stat (CynthiaPkmn pkmn) = baseStat stat pkmn
  baseStat stat (AlderPkmn pkmn) = baseStat stat pkmn
  typeOfPokemon (RedPkmn pkmn) = typeOfPokemon pkmn
  typeOfPokemon (BluePkmn pkmn) = typeOfPokemon pkmn
  typeOfPokemon (LancePkmn pkmn) = typeOfPokemon pkmn
  typeOfPokemon (StevenPkmn pkmn) = typeOfPokemon pkmn
  typeOfPokemon (WallacePkmn pkmn) = typeOfPokemon pkmn
  typeOfPokemon (CynthiaPkmn pkmn) = typeOfPokemon pkmn
  typeOfPokemon (AlderPkmn pkmn) = typeOfPokemon pkmn
  genderRatio (RedPkmn pkmn) = genderRatio pkmn
  genderRatio (BluePkmn pkmn) = genderRatio pkmn
  genderRatio (LancePkmn pkmn) = genderRatio pkmn
  genderRatio (StevenPkmn pkmn) = genderRatio pkmn
  genderRatio (WallacePkmn pkmn) = genderRatio pkmn
  genderRatio (CynthiaPkmn pkmn) = genderRatio pkmn
  genderRatio (AlderPkmn pkmn) = genderRatio pkmn
  possibleAbilities (RedPkmn pkmn) = possibleAbilities pkmn
  possibleAbilities (BluePkmn pkmn) = possibleAbilities pkmn
  possibleAbilities (LancePkmn pkmn) = possibleAbilities pkmn
  possibleAbilities (StevenPkmn pkmn) = possibleAbilities pkmn
  possibleAbilities (WallacePkmn pkmn) = possibleAbilities pkmn
  possibleAbilities (CynthiaPkmn pkmn) = possibleAbilities pkmn
  possibleAbilities (AlderPkmn pkmn) = possibleAbilities pkmn
  weight (RedPkmn pkmn) = weight pkmn
  weight (BluePkmn pkmn) = weight pkmn
  weight (LancePkmn pkmn) = weight pkmn
  weight (StevenPkmn pkmn) = weight pkmn
  weight (WallacePkmn pkmn) = weight pkmn
  weight (CynthiaPkmn pkmn) = weight pkmn
  weight (AlderPkmn pkmn) = weight pkmn



instance PokemonAttributes Blue'sPokemon where
  baseStat BaseAttack Aerodactyl = 105
  baseStat BaseAttack Alakazam = 50
  baseStat BaseAttack Arcanine = 110
  baseStat BaseAttack Exeggutor = 95
  baseStat BaseAttack Gyarados = 125
  baseStat BaseAttack Machamp = 130
  baseStat BaseDefence Aerodactyl = 65
  baseStat BaseDefence Alakazam = 45
  baseStat BaseDefence Arcanine = 80
  baseStat BaseDefence Exeggutor = 85
  baseStat BaseDefence Gyarados = 79
  baseStat BaseDefence Machamp = 80
  baseStat BaseHP Aerodactyl = 80
  baseStat BaseHP Alakazam = 55
  baseStat BaseHP Arcanine = 90
  baseStat BaseHP Exeggutor = 95
  baseStat BaseHP Gyarados = 95
  baseStat BaseHP Machamp = 90
  baseStat BaseSAttack Aerodactyl = 60
  baseStat BaseSAttack Alakazam = 135
  baseStat BaseSAttack Arcanine = 100
  baseStat BaseSAttack Exeggutor = 125
  baseStat BaseSAttack Gyarados = 60
  baseStat BaseSAttack Machamp = 65
  baseStat BaseSDefence Aerodactyl = 75
  baseStat BaseSDefence Alakazam = 95
  baseStat BaseSDefence Arcanine = 80
  baseStat BaseSDefence Exeggutor = 75
  baseStat BaseSDefence Gyarados = 100
  baseStat BaseSDefence Machamp = 85
  baseStat BaseSpeed Aerodactyl = 130
  baseStat BaseSpeed Alakazam = 120
  baseStat BaseSpeed Arcanine = 95
  baseStat BaseSpeed Exeggutor = 55
  baseStat BaseSpeed Gyarados = 81
  baseStat BaseSpeed Machamp = 55
  genderRatio Aerodactyl = Male7to1
  genderRatio Alakazam = Male3to1
  genderRatio Arcanine = Male3to1
  genderRatio Exeggutor = EvenRatio
  genderRatio Gyarados = EvenRatio
  genderRatio Machamp = Male3to1
  possibleAbilities Aerodactyl = [RockHead,Pressure]
  possibleAbilities Alakazam = [Synchronize,InnerFocus]
  possibleAbilities Arcanine = [Intimidate,FlashFire]
  possibleAbilities Exeggutor = [Chlorophyll]
  possibleAbilities Gyarados = [Intimidate]
  possibleAbilities Machamp = [Guts,NoGuard]
  typeOfPokemon Aerodactyl = [Rock,Flying]
  typeOfPokemon Alakazam = [Psychic]
  typeOfPokemon Arcanine = [Fire]
  typeOfPokemon Exeggutor = [Grass,Psychic]
  typeOfPokemon Gyarados = [Water,Flying]
  typeOfPokemon Machamp = [Fighting]
  weight Aerodactyl = 59.0
  weight Alakazam = 48.0
  weight Arcanine = 155.0
  weight Exeggutor = 120.0
  weight Gyarados = 235.0
  weight Machamp = 130.0

instance PokemonAttributes Lance'sPokemon where
  baseStat BaseAttack Dragonite = 134
  baseStat BaseAttack Flygon = 100
  baseStat BaseAttack Haxorus = 147
  baseStat BaseAttack Hydreigon = 105
  baseStat BaseAttack Kingdra = 95
  baseStat BaseAttack Salamence = 135
  baseStat BaseDefence Dragonite = 95
  baseStat BaseDefence Flygon = 80
  baseStat BaseDefence Haxorus = 90
  baseStat BaseDefence Hydreigon = 90
  baseStat BaseDefence Kingdra = 95
  baseStat BaseDefence Salamence = 80
  baseStat BaseHP Dragonite = 91
  baseStat BaseHP Flygon = 80
  baseStat BaseHP Haxorus = 76
  baseStat BaseHP Hydreigon = 92
  baseStat BaseHP Kingdra = 75
  baseStat BaseHP Salamence = 95
  baseStat BaseSAttack Dragonite = 100
  baseStat BaseSAttack Flygon = 80
  baseStat BaseSAttack Haxorus = 60
  baseStat BaseSAttack Hydreigon = 125
  baseStat BaseSAttack Kingdra = 95
  baseStat BaseSAttack Salamence = 110
  baseStat BaseSDefence Dragonite = 100
  baseStat BaseSDefence Flygon = 80
  baseStat BaseSDefence Haxorus = 70
  baseStat BaseSDefence Hydreigon = 90
  baseStat BaseSDefence Kingdra = 95
  baseStat BaseSDefence Salamence = 80
  baseStat BaseSpeed Dragonite = 80
  baseStat BaseSpeed Flygon = 100
  baseStat BaseSpeed Haxorus = 97
  baseStat BaseSpeed Hydreigon = 98
  baseStat BaseSpeed Kingdra = 85
  baseStat BaseSpeed Salamence = 100
  genderRatio Dragonite = EvenRatio
  genderRatio Flygon = EvenRatio
  genderRatio Haxorus = EvenRatio
  genderRatio Hydreigon = EvenRatio
  genderRatio Kingdra = EvenRatio
  genderRatio Salamence = Male7to1
  possibleAbilities Dragonite = [InnerFocus]
  possibleAbilities Flygon = [Levitate]
  possibleAbilities Haxorus = [Rivalry,MoldBreaker]
  possibleAbilities Hydreigon = [Levitate]
  possibleAbilities Kingdra = [SwiftSwim,Sniper]
  possibleAbilities Salamence = [Intimidate]
  typeOfPokemon Dragonite = [Dragon,Flying]
  typeOfPokemon Flygon = [Ground,Dragon]
  typeOfPokemon Haxorus = [Dragon]
  typeOfPokemon Hydreigon = [Dark,Dragon]
  typeOfPokemon Kingdra = [Water,Dragon]
  typeOfPokemon Salamence = [Dragon,Flying]
  weight Dragonite = 210.0
  weight Flygon = 82.0
  weight Haxorus = 105.5
  weight Hydreigon = 160.0
  weight Kingdra = 152.0
  weight Salamence = 102.6

instance PokemonAttributes Steven'sPokemon where
  baseStat BaseAttack Aggron = 110
  baseStat BaseAttack Archeops = 140
  baseStat BaseAttack Armaldo = 125
  baseStat BaseAttack Cradily = 81
  baseStat BaseAttack Excadrill = 135
  baseStat BaseAttack Metagross = 135
  baseStat BaseDefence Aggron = 180
  baseStat BaseDefence Archeops = 65
  baseStat BaseDefence Armaldo = 100
  baseStat BaseDefence Cradily = 97
  baseStat BaseDefence Excadrill = 60
  baseStat BaseDefence Metagross = 130
  baseStat BaseHP Aggron = 70
  baseStat BaseHP Archeops = 75
  baseStat BaseHP Armaldo = 75
  baseStat BaseHP Cradily = 86
  baseStat BaseHP Excadrill = 110
  baseStat BaseHP Metagross = 80
  baseStat BaseSAttack Aggron = 60
  baseStat BaseSAttack Archeops = 112
  baseStat BaseSAttack Armaldo = 70
  baseStat BaseSAttack Cradily = 81
  baseStat BaseSAttack Excadrill = 50
  baseStat BaseSAttack Metagross = 95
  baseStat BaseSDefence Aggron = 60
  baseStat BaseSDefence Archeops = 65
  baseStat BaseSDefence Armaldo = 80
  baseStat BaseSDefence Cradily = 107
  baseStat BaseSDefence Excadrill = 65
  baseStat BaseSDefence Metagross = 90
  baseStat BaseSpeed Aggron = 50
  baseStat BaseSpeed Archeops = 110
  baseStat BaseSpeed Armaldo = 45
  baseStat BaseSpeed Cradily = 43
  baseStat BaseSpeed Excadrill = 88
  baseStat BaseSpeed Metagross = 70
  genderRatio Aggron = EvenRatio
  genderRatio Archeops = Male7to1
  genderRatio Armaldo = Male7to1
  genderRatio Cradily = Male7to1
  genderRatio Excadrill = EvenRatio
  genderRatio Metagross = AlwaysGenderless
  possibleAbilities Aggron = [Sturdy,RockHead]
  possibleAbilities Archeops = [Defeatist]
  possibleAbilities Armaldo = [BattleArmor]
  possibleAbilities Cradily = [SuctionCups]
  possibleAbilities Excadrill = [SandRush,SandForce]
  possibleAbilities Metagross = [ClearBody]
  typeOfPokemon Aggron = [Steel,Rock]
  typeOfPokemon Archeops = [Rock,Flying]
  typeOfPokemon Armaldo = [Rock,Bug]
  typeOfPokemon Cradily = [Rock,Grass]
  typeOfPokemon Excadrill = [Ground,Steel]
  typeOfPokemon Metagross = [Steel,Psychic]
  weight Aggron = 360.0
  weight Archeops = 32.0
  weight Armaldo = 68.2
  weight Cradily = 60.4
  weight Excadrill = 40.4
  weight Metagross = 550.0

instance PokemonAttributes Wallace'sPokemon where
  baseStat BaseAttack Ludicolo = 70
  baseStat BaseAttack Milotic = 60
  baseStat BaseAttack Sharpedo = 120
  baseStat BaseAttack Starmie = 75
  baseStat BaseAttack Swampert = 110
  baseStat BaseAttack Walrein = 80
  baseStat BaseDefence Ludicolo = 70
  baseStat BaseDefence Milotic = 79
  baseStat BaseDefence Sharpedo = 40
  baseStat BaseDefence Starmie = 85
  baseStat BaseDefence Swampert = 90
  baseStat BaseDefence Walrein = 90
  baseStat BaseHP Ludicolo = 80
  baseStat BaseHP Milotic = 95
  baseStat BaseHP Sharpedo = 70
  baseStat BaseHP Starmie = 60
  baseStat BaseHP Swampert = 100
  baseStat BaseHP Walrein = 110
  baseStat BaseSAttack Ludicolo = 90
  baseStat BaseSAttack Milotic = 100
  baseStat BaseSAttack Sharpedo = 95
  baseStat BaseSAttack Starmie = 100
  baseStat BaseSAttack Swampert = 85
  baseStat BaseSAttack Walrein = 95
  baseStat BaseSDefence Ludicolo = 100
  baseStat BaseSDefence Milotic = 125
  baseStat BaseSDefence Sharpedo = 40
  baseStat BaseSDefence Starmie = 85
  baseStat BaseSDefence Swampert = 90
  baseStat BaseSDefence Walrein = 90
  baseStat BaseSpeed Ludicolo = 70
  baseStat BaseSpeed Milotic = 81
  baseStat BaseSpeed Sharpedo = 95
  baseStat BaseSpeed Starmie = 115
  baseStat BaseSpeed Swampert = 60
  baseStat BaseSpeed Walrein = 65
  genderRatio Ludicolo = EvenRatio
  genderRatio Milotic = EvenRatio
  genderRatio Sharpedo = Male7to1
  genderRatio Starmie = AlwaysGenderless
  genderRatio Swampert = Male7to1
  genderRatio Walrein = Male7to1
  possibleAbilities Ludicolo = [SwiftSwim,RainDish]
  possibleAbilities Milotic = [MarvelScale]
  possibleAbilities Sharpedo = [RoughSkin]
  possibleAbilities Starmie = [Illuminate,NaturalCure]
  possibleAbilities Swampert = [Torrent]
  possibleAbilities Walrein = [ThickFat,IceBody]
  typeOfPokemon Ludicolo = [Water,Grass]
  typeOfPokemon Milotic = [Water]
  typeOfPokemon Sharpedo = [Water,Dark]
  typeOfPokemon Starmie = [Water,Psychic]
  typeOfPokemon Swampert = [Water,Ground]
  typeOfPokemon Walrein = [Ice,Water]
  weight Ludicolo = 55.0
  weight Milotic = 162.0
  weight Sharpedo = 88.8
  weight Starmie = 80.0
  weight Swampert = 81.9
  weight Walrein = 150.6

instance PokemonAttributes Cynthia'sPokemon where
  baseStat BaseAttack Garchomp = 130
  baseStat BaseAttack Glaceon = 60
  baseStat BaseAttack Lucario = 110
  baseStat BaseAttack Roserade = 70
  baseStat BaseAttack Spiritomb = 92
  baseStat BaseAttack Togekiss = 50
  baseStat BaseDefence Garchomp = 95
  baseStat BaseDefence Glaceon = 110
  baseStat BaseDefence Lucario = 70
  baseStat BaseDefence Roserade = 65
  baseStat BaseDefence Spiritomb = 108
  baseStat BaseDefence Togekiss = 95
  baseStat BaseHP Garchomp = 108
  baseStat BaseHP Glaceon = 65
  baseStat BaseHP Lucario = 70
  baseStat BaseHP Roserade = 60
  baseStat BaseHP Spiritomb = 50
  baseStat BaseHP Togekiss = 85
  baseStat BaseSAttack Garchomp = 80
  baseStat BaseSAttack Glaceon = 130
  baseStat BaseSAttack Lucario = 115
  baseStat BaseSAttack Roserade = 125
  baseStat BaseSAttack Spiritomb = 92
  baseStat BaseSAttack Togekiss = 120
  baseStat BaseSDefence Garchomp = 85
  baseStat BaseSDefence Glaceon = 95
  baseStat BaseSDefence Lucario = 70
  baseStat BaseSDefence Roserade = 105
  baseStat BaseSDefence Spiritomb = 108
  baseStat BaseSDefence Togekiss = 115
  baseStat BaseSpeed Garchomp = 102
  baseStat BaseSpeed Glaceon = 65
  baseStat BaseSpeed Lucario = 90
  baseStat BaseSpeed Roserade = 90
  baseStat BaseSpeed Spiritomb = 35
  baseStat BaseSpeed Togekiss = 80
  genderRatio Garchomp = EvenRatio
  genderRatio Glaceon = Male7to1
  genderRatio Lucario = Male7to1
  genderRatio Roserade = EvenRatio
  genderRatio Spiritomb = Male7to1
  genderRatio Togekiss = Male7to1
  possibleAbilities Garchomp = [SandVeil]
  possibleAbilities Glaceon = [SnowCloak]
  possibleAbilities Lucario = [Steadfast,InnerFocus]
  possibleAbilities Roserade = [NaturalCure,PoisonPoint]
  possibleAbilities Spiritomb = [Pressure]
  possibleAbilities Togekiss = [Hustle,SereneGrace]
  typeOfPokemon Garchomp = [Dragon,Ground]
  typeOfPokemon Glaceon = [Ice]
  typeOfPokemon Lucario = [Fighting,Steel]
  typeOfPokemon Roserade = [Grass,Poison]
  typeOfPokemon Spiritomb = [Ghost,Dark]
  typeOfPokemon Togekiss = [Fairy,Flying]
  weight Garchomp = 95.0
  weight Glaceon = 25.9
  weight Lucario = 54.0
  weight Roserade = 14.5
  weight Spiritomb = 108.0
  weight Togekiss = 38.0

instance PokemonAttributes Alder'sPokemon where
  baseStat BaseAttack Braviary = 123
  baseStat BaseAttack Chandelure = 55
  baseStat BaseAttack Conkeldurr = 140
  baseStat BaseAttack Krookodile = 117
  baseStat BaseAttack Reuniclus = 65
  baseStat BaseAttack Volcarona = 60
  baseStat BaseDefence Braviary = 75
  baseStat BaseDefence Chandelure = 90
  baseStat BaseDefence Conkeldurr = 95
  baseStat BaseDefence Krookodile = 80
  baseStat BaseDefence Reuniclus = 75
  baseStat BaseDefence Volcarona = 65
  baseStat BaseHP Braviary = 100
  baseStat BaseHP Chandelure = 60
  baseStat BaseHP Conkeldurr = 105
  baseStat BaseHP Krookodile = 95
  baseStat BaseHP Reuniclus = 110
  baseStat BaseHP Volcarona = 85
  baseStat BaseSAttack Braviary = 57
  baseStat BaseSAttack Chandelure = 145
  baseStat BaseSAttack Conkeldurr = 55
  baseStat BaseSAttack Krookodile = 65
  baseStat BaseSAttack Reuniclus = 125
  baseStat BaseSAttack Volcarona = 135
  baseStat BaseSDefence Braviary = 75
  baseStat BaseSDefence Chandelure = 90
  baseStat BaseSDefence Conkeldurr = 65
  baseStat BaseSDefence Krookodile = 70
  baseStat BaseSDefence Reuniclus = 85
  baseStat BaseSDefence Volcarona = 105
  baseStat BaseSpeed Braviary = 80
  baseStat BaseSpeed Chandelure = 80
  baseStat BaseSpeed Conkeldurr = 45
  baseStat BaseSpeed Krookodile = 92
  baseStat BaseSpeed Reuniclus = 30
  baseStat BaseSpeed Volcarona = 100
  genderRatio Braviary = AlwaysMale
  genderRatio Chandelure = EvenRatio
  genderRatio Conkeldurr = Male3to1
  genderRatio Krookodile = EvenRatio
  genderRatio Reuniclus = EvenRatio
  genderRatio Volcarona = Male7to1
  possibleAbilities Braviary = [KeenEye,SheerForce]
  possibleAbilities Chandelure = [FlashFire,FlameBody]
  possibleAbilities Conkeldurr = [Guts,SheerForce]
  possibleAbilities Krookodile = [Intimidate,Moxie]
  possibleAbilities Reuniclus = [Overcoat,MagicGuard]
  possibleAbilities Volcarona = [FlameBody]
  typeOfPokemon Braviary = [Normal,Flying]
  typeOfPokemon Chandelure = [Ghost,Fire]
  typeOfPokemon Conkeldurr = [Fighting]
  typeOfPokemon Krookodile = [Ground,Dark]
  typeOfPokemon Reuniclus = [Psychic]
  typeOfPokemon Volcarona = [Bug,Fire]
  weight Braviary = 41.0
  weight Chandelure = 34.3
  weight Conkeldurr = 87.0
  weight Krookodile = 96.3
  weight Reuniclus = 20.1
  weight Volcarona = 46.0
