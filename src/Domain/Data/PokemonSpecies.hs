module Domain.Data.PokemonSpecies where

import Domain.Entity.Pokemon.Species
import Domain.Attribute.TypeOf

pikachu = PokemonSpecies {
  _speciesName = "Pikachu"
  , _typeOf = [Electric]
  , _baseHp = 35
  , _baseAttack = 55
  , _baseDefence = 40
  , _baseSAttack = 50
  , _baseSDefence = 50
  , _baseSpeed = 90
  , _weight = 6.0
  , _genderRatio = male7to1
  }

lapras = PokemonSpecies {
  _speciesName = "Lapras"
  , _typeOf = [Water,Ice]
  , _baseHp = 130
  , _baseAttack = 85
  , _baseDefence = 80
  , _baseSAttack = 85
  , _baseSDefence = 95
  , _baseSpeed = 60
  , _weight = 220.0
  , _genderRatio = male7to1
  }

snorlax = PokemonSpecies {
  _speciesName = "Snorlax"
  , _typeOf = [Normal]
  , _baseHp = 160
  , _baseAttack = 110
  , _baseDefence = 65
  , _baseSAttack = 65
  , _baseSDefence = 110
  , _baseSpeed = 30
  , _weight = 460.0
  , _genderRatio = male7to1
  }

venusaur = PokemonSpecies {
  _speciesName = "Venusaur"
  , _typeOf = [Grass,Poison]
  , _baseHp = 80
  , _baseAttack = 82
  , _baseDefence = 83
  , _baseSAttack = 100
  , _baseSDefence = 100
  , _baseSpeed = 80
  , _weight = 100.0
  , _genderRatio = male7to1
  }

charizard = PokemonSpecies {
  _speciesName = "Charizard"
  , _typeOf = [Fire,Flying]
  , _baseHp = 78
  , _baseAttack = 84
  , _baseDefence = 78
  , _baseSAttack = 109
  , _baseSDefence = 85
  , _baseSpeed = 100
  , _weight = 199.5
  , _genderRatio = male7to1
  }

blastoise = PokemonSpecies {
  _speciesName = "Blastoise"
  , _typeOf = [Water]
  , _baseHp = 79
  , _baseAttack = 83
  , _baseDefence = 100
  , _baseSAttack = 85
  , _baseSDefence = 105
  , _baseSpeed = 78
  , _weight = 85.5
  , _genderRatio = male7to1
  }

aerodactyl = PokemonSpecies {
  _speciesName = "Aerodactyl"
  , _typeOf = [Rock,Flying]
  , _baseHp = 80
  , _baseAttack = 105
  , _baseDefence = 65
  , _baseSAttack = 60
  , _baseSDefence = 75
  , _baseSpeed = 130
  , _weight = 59.0
  , _genderRatio = male7to1
  }

machamp = PokemonSpecies {
  _speciesName = "Machamp"
  , _typeOf = [Fighting]
  , _baseHp = 90
  , _baseAttack = 130
  , _baseDefence = 80
  , _baseSAttack = 65
  , _baseSDefence = 85
  , _baseSpeed = 55
  , _weight = 130.0
  , _genderRatio = male7to1
  }

alakazam = PokemonSpecies {
  _speciesName = "Alakazam"
  , _typeOf = [Psychic]
  , _baseHp = 55
  , _baseAttack = 50
  , _baseDefence = 45
  , _baseSAttack = 135
  , _baseSDefence = 95
  , _baseSpeed = 120
  , _weight = 48.0
  , _genderRatio = male7to1
  }

exeggutor = PokemonSpecies {
  _speciesName = "Exeggutor"
  , _typeOf = [Grass,Psychic]
  , _baseHp = 95
  , _baseAttack = 95
  , _baseDefence = 85
  , _baseSAttack = 125
  , _baseSDefence = 75
  , _baseSpeed = 55
  , _weight = 120.0
  , _genderRatio = male7to1
  }

arcanine = PokemonSpecies {
  _speciesName = "Arcanine"
  , _typeOf = [Fire]
  , _baseHp = 90
  , _baseAttack = 110
  , _baseDefence = 80
  , _baseSAttack = 100
  , _baseSDefence = 80
  , _baseSpeed = 95
  , _weight = 155.0
  , _genderRatio = male7to1
  }

gyarados = PokemonSpecies {
  _speciesName = "Gyarados"
  , _typeOf = [Water,Flying]
  , _baseHp = 95
  , _baseAttack = 125
  , _baseDefence = 79
  , _baseSAttack = 60
  , _baseSDefence = 100
  , _baseSpeed = 81
  , _weight = 235.0
  , _genderRatio = male7to1
  }

dragonite = PokemonSpecies {
  _speciesName = "Dragonite"
  , _typeOf = [Dragon,Flying]
  , _baseHp = 91
  , _baseAttack = 134
  , _baseDefence = 95
  , _baseSAttack = 100
  , _baseSDefence = 100
  , _baseSpeed = 80
  , _weight = 210.0
  , _genderRatio = male7to1
  }

salamence = PokemonSpecies {
  _speciesName = "Salamence"
  , _typeOf = [Dragon,Flying]
  , _baseHp = 95
  , _baseAttack = 135
  , _baseDefence = 80
  , _baseSAttack = 110
  , _baseSDefence = 80
  , _baseSpeed = 100
  , _weight = 102.6
  , _genderRatio = male7to1
  }

kingdra = PokemonSpecies {
  _speciesName = "Kingdra"
  , _typeOf = [Water,Dragon]
  , _baseHp = 75
  , _baseAttack = 95
  , _baseDefence = 95
  , _baseSAttack = 95
  , _baseSDefence = 95
  , _baseSpeed = 85
  , _weight = 152.0
  , _genderRatio = male7to1
  }

haxorus = PokemonSpecies {
  _speciesName = "Haxorus"
  , _typeOf = [Dragon]
  , _baseHp = 76
  , _baseAttack = 147
  , _baseDefence = 90
  , _baseSAttack = 60
  , _baseSDefence = 70
  , _baseSpeed = 97
  , _weight = 105.5
  , _genderRatio = male7to1
  }

hydreigon = PokemonSpecies {
  _speciesName = "Hydreigon"
  , _typeOf = [Dark,Dragon]
  , _baseHp = 92
  , _baseAttack = 105
  , _baseDefence = 90
  , _baseSAttack = 125
  , _baseSDefence = 90
  , _baseSpeed = 98
  , _weight = 160.0
  , _genderRatio = male7to1
  }

flygon = PokemonSpecies {
  _speciesName = "Flygon"
  , _typeOf = [Ground,Dragon]
  , _baseHp = 80
  , _baseAttack = 100
  , _baseDefence = 80
  , _baseSAttack = 80
  , _baseSDefence = 80
  , _baseSpeed = 100
  , _weight = 82.0
  , _genderRatio = male7to1
  }

metagross = PokemonSpecies {
  _speciesName = "Metagross"
  , _typeOf = [Steel,Psychic]
  , _baseHp = 80
  , _baseAttack = 135
  , _baseDefence = 130
  , _baseSAttack = 95
  , _baseSDefence = 90
  , _baseSpeed = 70
  , _weight = 550.0
  , _genderRatio = male7to1
  }

aggron = PokemonSpecies {
  _speciesName = "Aggron"
  , _typeOf = [Steel,Rock]
  , _baseHp = 70
  , _baseAttack = 110
  , _baseDefence = 180
  , _baseSAttack = 60
  , _baseSDefence = 60
  , _baseSpeed = 50
  , _weight = 360.0
  , _genderRatio = male7to1
  }

excadrill = PokemonSpecies {
  _speciesName = "Excadrill"
  , _typeOf = [Ground,Steel]
  , _baseHp = 110
  , _baseAttack = 135
  , _baseDefence = 60
  , _baseSAttack = 50
  , _baseSDefence = 65
  , _baseSpeed = 88
  , _weight = 40.4
  , _genderRatio = male7to1
  }

archeops = PokemonSpecies {
  _speciesName = "Archeops"
  , _typeOf = [Rock,Flying]
  , _baseHp = 75
  , _baseAttack = 140
  , _baseDefence = 65
  , _baseSAttack = 112
  , _baseSDefence = 65
  , _baseSpeed = 110
  , _weight = 32.0
  , _genderRatio = male7to1
  }

cradily = PokemonSpecies {
  _speciesName = "Cradily"
  , _typeOf = [Rock,Grass]
  , _baseHp = 86
  , _baseAttack = 81
  , _baseDefence = 97
  , _baseSAttack = 81
  , _baseSDefence = 107
  , _baseSpeed = 43
  , _weight = 60.4
  , _genderRatio = male7to1
  }

armaldo = PokemonSpecies {
  _speciesName = "Armaldo"
  , _typeOf = [Rock,Bug]
  , _baseHp = 75
  , _baseAttack = 125
  , _baseDefence = 100
  , _baseSAttack = 70
  , _baseSDefence = 80
  , _baseSpeed = 45
  , _weight = 68.2
  , _genderRatio = male7to1
  }

milotic = PokemonSpecies {
  _speciesName = "Milotic"
  , _typeOf = [Water]
  , _baseHp = 95
  , _baseAttack = 60
  , _baseDefence = 79
  , _baseSAttack = 100
  , _baseSDefence = 125
  , _baseSpeed = 81
  , _weight = 162.0
  , _genderRatio = male7to1
  }

sharpedo = PokemonSpecies {
  _speciesName = "Sharpedo"
  , _typeOf = [Water,Dark]
  , _baseHp = 70
  , _baseAttack = 120
  , _baseDefence = 40
  , _baseSAttack = 95
  , _baseSDefence = 40
  , _baseSpeed = 95
  , _weight = 88.8
  , _genderRatio = male7to1
  }

walrein = PokemonSpecies {
  _speciesName = "Walrein"
  , _typeOf = [Ice,Water]
  , _baseHp = 110
  , _baseAttack = 80
  , _baseDefence = 90
  , _baseSAttack = 95
  , _baseSDefence = 90
  , _baseSpeed = 65
  , _weight = 150.6
  , _genderRatio = male7to1
  }

ludicolo = PokemonSpecies {
  _speciesName = "Ludicolo"
  , _typeOf = [Water,Grass]
  , _baseHp = 80
  , _baseAttack = 70
  , _baseDefence = 70
  , _baseSAttack = 90
  , _baseSDefence = 100
  , _baseSpeed = 70
  , _weight = 55.0
  , _genderRatio = male7to1
  }

swampert = PokemonSpecies {
  _speciesName = "Swampert"
  , _typeOf = [Water,Ground]
  , _baseHp = 100
  , _baseAttack = 110
  , _baseDefence = 90
  , _baseSAttack = 85
  , _baseSDefence = 90
  , _baseSpeed = 60
  , _weight = 81.9
  , _genderRatio = male7to1
  }

starmie = PokemonSpecies {
  _speciesName = "Starmie"
  , _typeOf = [Water,Psychic]
  , _baseHp = 60
  , _baseAttack = 75
  , _baseDefence = 85
  , _baseSAttack = 100
  , _baseSDefence = 85
  , _baseSpeed = 115
  , _weight = 80.0
  , _genderRatio = male7to1
  }

garchomp = PokemonSpecies {
  _speciesName = "Garchomp"
  , _typeOf = [Dragon,Ground]
  , _baseHp = 108
  , _baseAttack = 130
  , _baseDefence = 95
  , _baseSAttack = 80
  , _baseSDefence = 85
  , _baseSpeed = 102
  , _weight = 95.0
  , _genderRatio = male7to1
  } 

spiritomb = PokemonSpecies {
  _speciesName = "Spiritomb"
  , _typeOf = [Ghost,Dark]
  , _baseHp = 50
  , _baseAttack = 92
  , _baseDefence = 108
  , _baseSAttack = 92
  , _baseSDefence = 108
  , _baseSpeed = 35
  , _weight = 108.0
  , _genderRatio = male7to1
  }

roserade = PokemonSpecies {
  _speciesName = "Roserade"
  , _typeOf = [Grass,Poison]
  , _baseHp = 60
  , _baseAttack = 70
  , _baseDefence = 65
  , _baseSAttack = 125
  , _baseSDefence = 105
  , _baseSpeed = 90
  , _weight = 14.5
  , _genderRatio = male7to1
  } 

togekiss = PokemonSpecies {
  _speciesName = "Togekiss"
  , _typeOf = [Fairy,Flying]
  , _baseHp = 85
  , _baseAttack = 50
  , _baseDefence = 95
  , _baseSAttack = 120
  , _baseSDefence = 115
  , _baseSpeed = 80
  , _weight = 38.0
  , _genderRatio = male7to1
  }

lucario = PokemonSpecies {
  _speciesName = "Lucario"
  , _typeOf = [Fighting,Steel]
  , _baseHp = 70
  , _baseAttack = 110
  , _baseDefence = 70
  , _baseSAttack = 115
  , _baseSDefence = 70
  , _baseSpeed = 90
  , _weight = 54.0
  , _genderRatio = male7to1
  }

glaceon = PokemonSpecies {
  _speciesName = "Glaceon"
  , _typeOf = [Ice]
  , _baseHp = 65
  , _baseAttack = 60
  , _baseDefence = 110
  , _baseSAttack = 130
  , _baseSDefence = 95
  , _baseSpeed = 65
  , _weight = 25.9
  , _genderRatio = male7to1
  }

volcarona = PokemonSpecies {
  _speciesName = "Volcarona"
  , _typeOf = [Bug,Fire]
  , _baseHp = 85
  , _baseAttack = 60
  , _baseDefence = 65
  , _baseSAttack = 135
  , _baseSDefence = 105
  , _baseSpeed = 100
  , _weight = 46.0
  , _genderRatio = male7to1
  }

conkeldurr = PokemonSpecies {
  _speciesName = "Conkeldurr"
  , _typeOf = [Fighting]
  , _baseHp = 105
  , _baseAttack = 140
  , _baseDefence = 95
  , _baseSAttack = 55
  , _baseSDefence = 65
  , _baseSpeed = 45
  , _weight = 87.0
  , _genderRatio = male7to1
  }

reuniclus = PokemonSpecies {
  _speciesName = "Reuniclus"
  , _typeOf = [Psychic]
  , _baseHp = 110
  , _baseAttack = 65
  , _baseDefence = 75
  , _baseSAttack = 125
  , _baseSDefence = 85
  , _baseSpeed = 30
  , _weight = 20.1
  , _genderRatio = male7to1
  }

krookodile = PokemonSpecies {
  _speciesName = "Krookodile"
  , _typeOf = [Ground,Dark]
  , _baseHp = 95
  , _baseAttack = 117
  , _baseDefence = 80
  , _baseSAttack = 65
  , _baseSDefence = 70
  , _baseSpeed = 92
  , _weight = 96.3
  , _genderRatio = male7to1
  }

chandelure = PokemonSpecies {
  _speciesName = "Chandelure"
  , _typeOf = [Ghost,Fire]
  , _baseHp = 60
  , _baseAttack = 55
  , _baseDefence = 90
  , _baseSAttack = 145
  , _baseSDefence = 90
  , _baseSpeed = 80
  , _weight = 34.3
  , _genderRatio = male7to1
  }

braviary = PokemonSpecies {
  _speciesName = "Braviary"
  , _typeOf = [Normal,Flying]
  , _baseHp = 100
  , _baseAttack = 123
  , _baseDefence = 75
  , _baseSAttack = 57
  , _baseSDefence = 75
  , _baseSpeed = 80
  , _weight = 41.0
  , _genderRatio = male7to1
  }

