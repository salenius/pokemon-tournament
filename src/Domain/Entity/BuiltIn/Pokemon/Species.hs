{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE RankNTypes #-}


module Domain.Entity.BuiltIn.Pokemon.Species where

import GHC.Generics
import Control.Lens
import Domain.Entity.BuiltIn.Pokemon.Ability
import Domain.Entity.BuiltIn.Pokemon.Gender
import Domain.Entity.Stats.Pokemon
import Domain.Attribute.Statistic
import Domain.Attribute.Ability
import Domain.Attribute.TypeOf
import Domain.Attribute.Physiology
import Domain.Attribute.Gender


data Pikachu =
  Pikachu { fromPikachu :: Species' Male1to1}
  | PartnerPikachu { fromPikachu :: Species' Male1to1}
  deriving (Eq,Show,Read,Generic)

data Lapras =
  Lapras (Species Lapras'sAbility Male1to1)
  deriving (Eq,Show,Read,Generic)

data Snorlax =
  Snorlax (Species Snorlax'sAbility Male7to1)
  deriving (Eq,Show,Read,Generic)

data Venusaur =
  Venusaur { fromVenusaur :: Species' Male7to1}
  | MegaVenusaur { fromVenusaur :: Species' Male7to1}
  deriving (Eq,Show,Read,Generic)

data Charizard =
  Charizard { fromCharizard :: Species' Male7to1}
  | MegaCharizardX { fromCharizard :: Species' Male7to1}
  | MegaCharizardY { fromCharizard :: Species' Male7to1}
  deriving (Eq,Show,Read,Generic)

data Blastoise =
  Blastoise (Species' Male7to1)
  | MegaBlastoise (Species' Male7to1)
  deriving (Eq,Show,Read,Generic)

data Aerodactyl =
  Aerodactyl (Species Aerodactyl'sAbility Male7to1)
  | MegaAerodactyl (Species' Male7to1)
  deriving (Eq,Show,Read,Generic)

data Machamp =
  Machamp (Species Machamp'sAbility Male3to1)
  deriving (Eq,Show,Read,Generic)

data Alakazam =
  Alakazam (Species Alakazam'sAbility Male3to1)
  | MegaAlakazam (Species' Male3to1)
  deriving (Eq,Show,Read,Generic)

data Exeggutor =
  Exeggutor (Species' Male1to1)
  | AlolanExeggutor (Species' Male1to1)
  deriving (Eq,Show,Read,Generic)

data Arcanine =
  Arcanine (Species Arcanine'sAbility Male3to1)
  deriving (Eq,Show,Read,Generic)

data Gyarados =
  Gyarados (Species' Male1to1)
  | MegaGyarados (Species' Male1to1)
  deriving (Eq,Show,Read,Generic)

data Dragonite =
  Dragonite (Species' Male1to1)
  deriving (Eq,Show,Read,Generic)

data Salamence =
  Salamence (Species' Male1to1)
  | MegaSalamence (Species' Male1to1)
  deriving (Eq,Show,Read,Generic)

data Kingdra =
  Kingdra (Species Kingdra'sAbility Male1to1)
  deriving (Eq,Show,Read,Generic)

data Haxorus =
  Haxorus (Species Haxorus'sAbility Male1to1)
  deriving (Eq,Show,Read,Generic)

data Hydreigon =
  Hydreigon (Species' Male1to1)
  deriving (Eq,Show,Read,Generic)

data Flygon =
  Flygon (Species' Male1to1)
  deriving (Eq,Show,Read,Generic)

data Metagross =
  Metagross GenderlessSpecies'
  | MegaMetagross GenderlessSpecies'
  deriving (Eq,Show,Read,Generic)

data Aggron =
  Aggron (Species Aggron'sAbility Male1to1)
  | MegaAggron (Species' Male1to1)
  deriving (Eq,Show,Read,Generic)

data Excadrill =
  Excadrill (Species Excadrill'sAbility Male1to1)
  deriving (Eq,Show,Read,Generic)

data Archeops =
  Archeops (Species' Male7to1)
  deriving (Eq,Show,Read,Generic)

data Cradily =
  Cradily (Species' Male7to1)
  deriving (Eq,Show,Read,Generic)

data Armaldo =
  Armaldo (Species' Male7to1)
  deriving (Eq,Show,Read,Generic)

data Milotic =
  Milotic (Species' Male1to1)
  deriving (Eq,Show,Read,Generic)

data Sharpedo =
  Sharpedo (Species' Male1to1)
  | MegaSharpedo (Species' Male1to1)
  deriving (Eq,Show,Read,Generic)

data Walrein =
  Walrein (Species Walrein'sAbility Male1to1)
  deriving (Eq,Show,Read,Generic)

data Ludicolo =
  Ludicolo (Species Ludicolo'sAbility Male1to1)
  deriving (Eq,Show,Read,Generic)

data Swampert =
  Swampert (Species' Male7to1)
  | MegaSwampert (Species' Male7to1)
  deriving (Eq,Show,Read,Generic)

data Starmie =
  Starmie (GenderlessSpecies Starmie'sAbility)
  deriving (Eq,Show,Read,Generic)

data Garchomp =
  Garchomp (Species' Male1to1)
  | MegaGarchomp (Species' Male1to1)
  deriving (Eq,Show,Read,Generic)

data Spiritomb =
  Spiritomb (Species' Male1to1)
  deriving (Eq,Show,Read,Generic)

data Roserade =
  Roserade (Species Roserade'sAbility Male1to1)
  deriving (Eq,Show,Read,Generic)

data Togekiss =
  Togekiss (Species Togekiss'sAbility Male1to1)
  deriving (Eq,Show,Read,Generic)

data Lucario =
  Lucario (Species Lucario'sAbility Male7to1)
  | MegaLucario (Species' Male7to1)
  deriving (Eq,Show,Read,Generic)

data Glaceon =
  Glaceon (Species' Male1to1)
  deriving (Eq,Show,Read,Generic)

data Volcarona =
  Volcarona (Species' Male1to1)
  deriving (Eq,Show,Read,Generic)

data Conkeldurr =
  Conkeldurr (Species Conkeldurr'sAbility Male7to1)
  deriving (Eq,Show,Read,Generic)

data Reuniclus =
  Reuniclus (Species Reuniclus'sAbility Male1to1)
  deriving (Eq,Show,Read,Generic)

data Krookodile =
  Krookodile (Species Krookodile'sAbility Male1to1)
  deriving (Eq,Show,Read,Generic)

data Chandelure =
  Chandelure (Species Chandelure'sAbility Male1to1)
  deriving (Eq,Show,Read,Generic)

data Braviary =
  Braviary (Species Braviary'sAbility Male1to1)
  deriving (Eq,Show,Read,Generic)

----------------------

instance PokemonStat Pikachu where
  baseStat s (Pikachu _) = case s of
    BaseHP -> 35
    BaseAttack -> 55
    BaseDefence -> 40
    BaseSAttack -> 50
    BaseSDefence -> 50
    BaseSpeed -> 90
  baseStat s (PartnerPikachu _) = case s of
    BaseHP -> 45
    BaseAttack -> 80
    BaseDefence -> 50
    BaseSAttack -> 75
    BaseSDefence -> 60
    BaseSpeed -> 120

instance PokemonStat Lapras where
  baseStat s _ = case s of
    BaseHP -> 130
    BaseAttack -> 85
    BaseDefence -> 80
    BaseSAttack -> 85
    BaseSDefence -> 95
    BaseSpeed -> 60

instance PokemonStat Snorlax where
  baseStat s _ = case s of
    BaseHP -> 160
    BaseAttack -> 110
    BaseDefence -> 65
    BaseSAttack -> 65
    BaseSDefence -> 110
    BaseSpeed -> 30

instance PokemonStat Venusaur where
  baseStat s (Venusaur _) = case s of
    BaseHP -> 80
    BaseAttack -> 82
    BaseDefence -> 83
    BaseSAttack -> 100
    BaseSDefence -> 100
    BaseSpeed -> 80
  baseStat s (MegaVenusaur _) = case s of
    BaseHP -> 80
    BaseAttack -> 100
    BaseDefence -> 123
    BaseSAttack -> 122
    BaseSDefence -> 120
    BaseSpeed -> 80

instance PokemonStat Charizard where
  baseStat s (Charizard _) = case s of
    BaseHP -> 78
    BaseAttack -> 84
    BaseDefence -> 78
    BaseSAttack -> 109
    BaseSDefence -> 85
    BaseSpeed -> 100
  baseStat s (MegaCharizardX _) = case s of
    BaseHP -> 78
    BaseAttack -> 130
    BaseDefence -> 111
    BaseSAttack -> 130
    BaseSDefence -> 85
    BaseSpeed -> 100
  baseStat s (MegaCharizardY _) = case s of
    BaseHP -> 78
    BaseAttack -> 104
    BaseDefence -> 78
    BaseSAttack -> 159
    BaseSDefence -> 115
    BaseSpeed -> 100

instance PokemonStat Blastoise where
  baseStat s (Blastoise _) = case s of
    BaseHP -> 79
    BaseAttack -> 83
    BaseDefence -> 100
    BaseSAttack -> 85
    BaseSDefence -> 105
    BaseSpeed -> 78
  baseStat s (MegaBlastoise _) = case s of
    BaseHP -> 79
    BaseAttack -> 103
    BaseDefence -> 120
    BaseSAttack -> 135
    BaseSDefence -> 115
    BaseSpeed -> 78

instance PokemonStat Aerodactyl where
  baseStat s (Aerodactyl _) = case s of
    BaseHP -> 80
    BaseAttack -> 105
    BaseDefence -> 65
    BaseSAttack -> 60
    BaseSDefence -> 75
    BaseSpeed -> 130
  baseStat s (MegaAerodactyl _) = case s of
    BaseHP -> 80
    BaseAttack -> 135
    BaseDefence -> 85
    BaseSAttack -> 70
    BaseSDefence -> 95
    BaseSpeed -> 150

instance PokemonStat Machamp where
  baseStat s (Machamp _) = case s of
    BaseHP -> 90
    BaseAttack -> 130
    BaseDefence -> 80
    BaseSAttack -> 65
    BaseSDefence -> 85
    BaseSpeed -> 55

instance PokemonStat Alakazam where
  baseStat s (Alakazam _) = case s of
    BaseHP -> 55
    BaseAttack -> 50
    BaseDefence -> 45
    BaseSAttack -> 135
    BaseSDefence -> 95
    BaseSpeed -> 120
  baseStat s (MegaAlakazam _) = case s of
    BaseHP -> 55
    BaseAttack -> 50
    BaseDefence -> 65
    BaseSAttack -> 175
    BaseSDefence -> 105
    BaseSpeed -> 150

instance PokemonStat Exeggutor where
  baseStat s (Exeggutor _) = case s of
    BaseHP -> 95
    BaseAttack -> 95
    BaseDefence -> 85
    BaseSAttack -> 125
    BaseSDefence -> 75
    BaseSpeed -> 55
  baseStat s (AlolanExeggutor _) = case s of
    BaseHP -> 95
    BaseAttack -> 105
    BaseDefence -> 85
    BaseSAttack -> 125
    BaseSDefence -> 75
    BaseSpeed -> 45

instance PokemonStat Arcanine where
  baseStat s (Arcanine _) = case s of
    BaseHP -> 90
    BaseAttack -> 110
    BaseDefence -> 80
    BaseSAttack -> 100
    BaseSDefence -> 80
    BaseSpeed -> 95

instance PokemonStat Gyarados where
  baseStat s (Gyarados _) = case s of
    BaseHP -> 95
    BaseAttack -> 125
    BaseDefence -> 79
    BaseSAttack -> 60
    BaseSDefence -> 100
    BaseSpeed -> 81
  baseStat s (MegaGyarados _) = case s of
    BaseHP -> 95
    BaseAttack -> 155
    BaseDefence -> 109
    BaseSAttack -> 70
    BaseSDefence -> 130
    BaseSpeed -> 81

instance PokemonStat Dragonite where 
  baseStat s (Dragonite _) = case s of
    BaseHP -> 91
    BaseAttack -> 134
    BaseDefence -> 95
    BaseSAttack -> 100
    BaseSDefence -> 100
    BaseSpeed -> 80

instance PokemonStat Salamence where 
  baseStat s (Salamence _) = case s of
    BaseHP -> 95
    BaseAttack -> 135
    BaseDefence -> 80
    BaseSAttack -> 110
    BaseSDefence -> 80
    BaseSpeed -> 100
  baseStat s (MegaSalamence _) = case s of
    BaseHP -> 95
    BaseAttack -> 145
    BaseDefence -> 130
    BaseSAttack -> 120
    BaseSDefence -> 90
    BaseSpeed -> 120

instance PokemonStat Kingdra where 
  baseStat s (Kingdra _) = case s of
    BaseHP -> 75
    BaseAttack -> 95
    BaseDefence -> 95
    BaseSAttack -> 95
    BaseSDefence -> 95
    BaseSpeed -> 85

instance PokemonStat Haxorus where 
  baseStat s (Haxorus _) = case s of
    BaseHP -> 76
    BaseAttack -> 147
    BaseDefence -> 90
    BaseSAttack -> 60
    BaseSDefence -> 70
    BaseSpeed -> 97

instance PokemonStat Hydreigon where 
  baseStat s (Hydreigon _) = case s of
    BaseHP -> 92
    BaseAttack -> 105
    BaseDefence -> 90
    BaseSAttack -> 125
    BaseSDefence -> 90
    BaseSpeed -> 98

instance PokemonStat Flygon where 
  baseStat s (Flygon _) = case s of
    BaseHP -> 80
    BaseAttack -> 100
    BaseDefence -> 80
    BaseSAttack -> 80
    BaseSDefence -> 80
    BaseSpeed -> 100

instance PokemonStat Metagross where 
  baseStat s (Metagross _) = case s of
    BaseHP -> 80
    BaseAttack -> 135
    BaseDefence -> 130
    BaseSAttack -> 95
    BaseSDefence -> 90
    BaseSpeed -> 70
  baseStat s (MegaMetagross _) = case s of
    BaseHP -> 80
    BaseAttack -> 145
    BaseDefence -> 150
    BaseSAttack -> 105
    BaseSDefence -> 110
    BaseSpeed -> 110

instance PokemonStat Aggron where 
  baseStat s (Aggron _) = case s of
    BaseHP -> 70
    BaseAttack -> 110
    BaseDefence -> 180
    BaseSAttack -> 60
    BaseSDefence -> 60
    BaseSpeed -> 50
  baseStat s (MegaAggron _) = case s of
    BaseHP -> 70
    BaseAttack -> 140
    BaseDefence -> 230
    BaseSAttack -> 60
    BaseSDefence -> 80
    BaseSpeed -> 50

instance PokemonStat Excadrill where 
  baseStat s (Excadrill _) = case s of
    BaseHP -> 110
    BaseAttack -> 135
    BaseDefence -> 60
    BaseSAttack -> 50
    BaseSDefence -> 65
    BaseSpeed -> 88

instance PokemonStat Archeops where 
  baseStat s (Archeops _) = case s of
    BaseHP -> 75
    BaseAttack -> 140
    BaseDefence -> 65
    BaseSAttack -> 112
    BaseSDefence -> 65
    BaseSpeed -> 110

instance PokemonStat Cradily where 
  baseStat s (Cradily _) = case s of
    BaseHP -> 86
    BaseAttack -> 81
    BaseDefence -> 97
    BaseSAttack -> 81
    BaseSDefence -> 107
    BaseSpeed -> 43

instance PokemonStat Armaldo where 
  baseStat s (Armaldo _) = case s of
    BaseHP -> 75
    BaseAttack -> 125
    BaseDefence -> 100
    BaseSAttack -> 70
    BaseSDefence -> 80
    BaseSpeed -> 45

instance PokemonStat Milotic where 
  baseStat s (Milotic _) = case s of
    BaseHP -> 95
    BaseAttack -> 60
    BaseDefence -> 79
    BaseSAttack -> 100
    BaseSDefence -> 125
    BaseSpeed -> 81

instance PokemonStat Sharpedo where 
  baseStat s (Sharpedo _) = case s of
    BaseHP -> 70
    BaseAttack -> 120
    BaseDefence -> 40
    BaseSAttack -> 95
    BaseSDefence -> 40
    BaseSpeed -> 95
  baseStat s (MegaSharpedo _) = case s of
    BaseHP -> 70
    BaseAttack -> 140
    BaseDefence -> 70
    BaseSAttack -> 110
    BaseSDefence -> 65
    BaseSpeed -> 105

instance PokemonStat Walrein where 
  baseStat s (Walrein _) = case s of
    BaseHP -> 110
    BaseAttack -> 80
    BaseDefence -> 90
    BaseSAttack -> 95
    BaseSDefence -> 90
    BaseSpeed -> 65

instance PokemonStat Ludicolo where 
  baseStat s (Ludicolo _) = case s of
    BaseHP -> 80
    BaseAttack -> 70
    BaseDefence -> 70
    BaseSAttack -> 90
    BaseSDefence -> 100
    BaseSpeed -> 70

instance PokemonStat Swampert where 
  baseStat s (Swampert _) = case s of
    BaseHP -> 100
    BaseAttack -> 110
    BaseDefence -> 90
    BaseSAttack -> 85
    BaseSDefence -> 90
    BaseSpeed -> 60
  baseStat s (MegaSwampert _) = case s of
    BaseHP -> 100
    BaseAttack -> 150
    BaseDefence -> 110
    BaseSAttack -> 95
    BaseSDefence -> 110
    BaseSpeed -> 70

instance PokemonStat Starmie where 
  baseStat s (Starmie _) = case s of
    BaseHP -> 60
    BaseAttack -> 75
    BaseDefence -> 85
    BaseSAttack -> 100
    BaseSDefence -> 85
    BaseSpeed -> 115

instance PokemonStat Garchomp where 
  baseStat s (Garchomp _) = case s of
    BaseHP -> 108
    BaseAttack -> 130
    BaseDefence -> 95
    BaseSAttack -> 80
    BaseSDefence -> 85
    BaseSpeed -> 102
  baseStat s (MegaGarchomp _) = case s of
    BaseHP -> 108
    BaseAttack -> 170
    BaseDefence -> 115
    BaseSAttack -> 120
    BaseSDefence -> 95
    BaseSpeed -> 92

instance PokemonStat Spiritomb where 
  baseStat s (Spiritomb _) = case s of
    BaseHP -> 50
    BaseAttack -> 92
    BaseDefence -> 108
    BaseSAttack -> 92
    BaseSDefence -> 108
    BaseSpeed -> 35

instance PokemonStat Roserade where 
  baseStat s (Roserade _) = case s of
    BaseHP -> 60
    BaseAttack -> 70
    BaseDefence -> 65
    BaseSAttack -> 125
    BaseSDefence -> 105
    BaseSpeed -> 90

instance PokemonStat Togekiss where 
  baseStat s (Togekiss _) = case s of
    BaseHP -> 85
    BaseAttack -> 50
    BaseDefence -> 95
    BaseSAttack -> 120
    BaseSDefence -> 115
    BaseSpeed -> 80

instance PokemonStat Lucario where 
  baseStat s (Lucario _) = case s of
    BaseHP -> 70
    BaseAttack -> 110
    BaseDefence -> 70
    BaseSAttack -> 115
    BaseSDefence -> 70
    BaseSpeed -> 90
  baseStat s (MegaLucario _) = case s of
    BaseHP -> 70
    BaseAttack -> 145
    BaseDefence -> 88
    BaseSAttack -> 140
    BaseSDefence -> 70
    BaseSpeed -> 112

instance PokemonStat Glaceon where 
  baseStat s (Glaceon _) = case s of
    BaseHP -> 65
    BaseAttack -> 60
    BaseDefence -> 110
    BaseSAttack -> 130
    BaseSDefence -> 95
    BaseSpeed -> 65

instance PokemonStat Volcarona where 
  baseStat s (Volcarona _) = case s of
    BaseHP -> 85
    BaseAttack -> 60
    BaseDefence -> 65
    BaseSAttack -> 135
    BaseSDefence -> 105
    BaseSpeed -> 100

instance PokemonStat Conkeldurr where 
  baseStat s (Conkeldurr _) = case s of
    BaseHP -> 105
    BaseAttack -> 140
    BaseDefence -> 95
    BaseSAttack -> 55
    BaseSDefence -> 65
    BaseSpeed -> 45

instance PokemonStat Reuniclus where 
  baseStat s (Reuniclus _) = case s of
    BaseHP -> 110
    BaseAttack -> 65
    BaseDefence -> 75
    BaseSAttack -> 125
    BaseSDefence -> 85
    BaseSpeed -> 30

instance PokemonStat Krookodile where 
  baseStat s (Krookodile _) = case s of
    BaseHP -> 95
    BaseAttack -> 117
    BaseDefence -> 80
    BaseSAttack -> 65
    BaseSDefence -> 70
    BaseSpeed -> 92

instance PokemonStat Chandelure where 
  baseStat s (Chandelure _) = case s of
    BaseHP -> 60
    BaseAttack -> 55
    BaseDefence -> 90
    BaseSAttack -> 145
    BaseSDefence -> 90
    BaseSpeed -> 80

instance PokemonStat Braviary where 
  baseStat s (Braviary _) = case s of
    BaseHP -> 100
    BaseAttack -> 123
    BaseDefence -> 75
    BaseSAttack -> 57
    BaseSDefence -> 75
    BaseSpeed -> 80

--

instance PokemonAttribute Pikachu where
  abilityIs (Pikachu _) = Static
  abilityIs (PartnerPikachu _) = NoAbility
  typeIs _ = ListOf1 Electric
  genderIs (Pikachu x) = getGender x
  genderIs (PartnerPikachu x) = getGender x

instance PokemonAttribute Lapras where
  abilityIs (Lapras x) = getAbility _Lapras'sAbility x
  typeIs _ = ListOf2 Water Ice
  genderIs (Lapras x) = getGender x

instance PokemonAttribute Snorlax where
  abilityIs (Snorlax x) = getAbility _Snorlax'sAbility x
  typeIs _ = ListOf1 Normal
  genderIs (Snorlax x) = getGender x

instance PokemonAttribute Venusaur where
  abilityIs (Venusaur _) = Overgrow
  abilityIs (MegaVenusaur _) = ThickFat
  typeIs _ = ListOf2 Grass Poison
  genderIs (Venusaur x) = getGender x
  genderIs (MegaVenusaur x) = getGender x

instance PokemonAttribute Charizard where
  abilityIs (Charizard _) = Blaze
  abilityIs (MegaCharizardX _) = ToughClaws
  abilityIs (MegaCharizardY _) = Drought
  typeIs (MegaCharizardX _) = ListOf2 Fire Dragon
  typeIs _ = ListOf2 Fire Flying
  genderIs (Charizard x) = getGender x
  genderIs (MegaCharizardX x) = getGender x
  genderIs (MegaCharizardY x) = getGender x

instance PokemonAttribute Blastoise where
  abilityIs (Blastoise _) = Torrent
  abilityIs (MegaBlastoise _) = MegaLauncher
  typeIs _ = ListOf1 Water
  genderIs (Blastoise x) = getGender x
  genderIs (MegaBlastoise x) = getGender x

instance PokemonAttribute Aerodactyl where
  abilityIs (Aerodactyl x) = getAbility _Aerodactyl'sAbility x
  abilityIs (MegaAerodactyl x) = ToughClaws
  typeIs _ = ListOf2 Rock Flying
  genderIs (Aerodactyl x) = getGender x
  genderIs (MegaAerodactyl x) = getGender x

instance PokemonAttribute Machamp where
  abilityIs (Machamp x) = getAbility _Machamp'sAbility x
  typeIs _ = ListOf1 Fighting
  genderIs (Machamp x) = getGender x

instance PokemonAttribute Alakazam where
  abilityIs (Alakazam x) = getAbility _Alakazam'sAbility x
  abilityIs (MegaAlakazam _) = Trace
  typeIs _ = ListOf1 Psychic
  genderIs (Alakazam x) = getGender x
  genderIs (MegaAlakazam x) = getGender x

instance PokemonAttribute Exeggutor where
  abilityIs (Exeggutor _) = Chlorophyll
  abilityIs (AlolanExeggutor _) = Frisk
  typeIs (Exeggutor _) = ListOf2 Grass Psychic
  typeIs (AlolanExeggutor _) = ListOf2 Grass Dragon
  genderIs (Exeggutor x) = getGender x
  genderIs (AlolanExeggutor x) = getGender x

instance PokemonAttribute Arcanine where
  abilityIs (Arcanine x) = getAbility _Arcanine'sAbility x
  typeIs _ = ListOf1 Fire
  genderIs (Arcanine x) = getGender x

instance PokemonAttribute Gyarados where
  abilityIs (Gyarados _) = Intimidate
  abilityIs (MegaGyarados _) = MoldBreaker
  typeIs _ = ListOf2 Water Flying
  genderIs (Gyarados x) = getGender x
  genderIs (MegaGyarados x) = getGender x

instance PokemonAttribute Dragonite where
  abilityIs (Dragonite _) = InnerFocus
  typeIs _ = ListOf2 Dragon Flying
  genderIs (Dragonite x) = getGender x

instance PokemonAttribute Salamence where
  abilityIs (Salamence _) = Intimidate
  abilityIs (MegaSalamence _) = Aerilate
  typeIs _ = ListOf2 Dragon Flying
  genderIs (Salamence x) = getGender x
  genderIs (MegaSalamence x) = getGender x

instance PokemonAttribute Kingdra where
  abilityIs (Kingdra x) = getAbility _Kingdra'sAbility x
  typeIs _ = ListOf2 Water Dragon
  genderIs (Kingdra x) = getGender x

instance PokemonAttribute Haxorus where
  abilityIs (Haxorus x) = getAbility _Haxorus'sAbility x
  typeIs _ = ListOf1 Dragon
  genderIs (Haxorus x) = getGender x

instance PokemonAttribute Hydreigon where
  abilityIs (Hydreigon _) = Levitate
  typeIs _ = ListOf2 Dark Dragon
  genderIs (Hydreigon x) = getGender x

instance PokemonAttribute Flygon where
  abilityIs (Flygon _) = Levitate
  typeIs _ = ListOf2 Ground Dragon
  genderIs (Flygon x) = getGender x

instance PokemonAttribute Metagross where
  abilityIs (Metagross _) = ClearBody
  abilityIs (MegaMetagross _) = ToughClaws
  typeIs _ = ListOf2 Steel Psychic
  genderIs (Metagross _) = Genderless
  genderIs (MegaMetagross _) = Genderless

instance PokemonAttribute Aggron where
  abilityIs (Aggron x) = getAbility _Aggron'sAbility x
  abilityIs (MegaAggron _) = Filter
  typeIs (Aggron _) = ListOf2 Steel Rock
  typeIs (MegaAggron _) = ListOf1 Steel
  genderIs (Aggron x) = getGender x
  genderIs (MegaAggron x) = getGender x

instance PokemonAttribute Excadrill where
  abilityIs (Excadrill x) = getAbility _Excadrill'sAbility x
  typeIs _ = ListOf2 Ground Steel
  genderIs (Excadrill x) = getGender x

instance PokemonAttribute Archeops where
  abilityIs (Archeops _) = Defeatist
  typeIs _ = ListOf2 Rock Flying
  genderIs (Archeops x) = getGender x

instance PokemonAttribute Cradily where
  abilityIs (Cradily _) = SuctionCups
  typeIs _ = ListOf2 Rock Grass
  genderIs (Cradily x) = getGender x

instance PokemonAttribute Armaldo where
  abilityIs (Armaldo _) = BattleArmor
  typeIs _ = ListOf2 Rock Bug
  genderIs (Armaldo x) = getGender x

instance PokemonAttribute Milotic where
  abilityIs (Milotic _) = MarvelScale
  typeIs _ = ListOf1 Water
  genderIs (Milotic x) = getGender x

instance PokemonAttribute Sharpedo where
  abilityIs (Sharpedo _) = RoughSkin
  abilityIs (MegaSharpedo _) = StrongJaw
  typeIs _ = ListOf2 Water Dark
  genderIs (Sharpedo x) = getGender x
  genderIs (MegaSharpedo x) = getGender x

instance PokemonAttribute Walrein where
  abilityIs (Walrein x) = getAbility _Walrein'sAbility x
  typeIs _ = ListOf2 Ice Water
  genderIs (Walrein x) = getGender x

instance PokemonAttribute Ludicolo where
  abilityIs (Ludicolo x) = getAbility _Ludicolo'sAbility x
  typeIs _ = ListOf2 Water Grass
  genderIs (Ludicolo x) = getGender x

instance PokemonAttribute Swampert where
  abilityIs (Swampert x) = Torrent
  abilityIs (MegaSwampert x) = SwiftSwim
  typeIs _ = ListOf2 Water Ground
  genderIs (Swampert x) = getGender x
  genderIs (MegaSwampert x) = getGender x

instance PokemonAttribute Starmie where
  abilityIs (Starmie x) = getAbility _Starmie'sAbility x
  typeIs _ = ListOf2 Water Psychic
  genderIs (Starmie _) = Genderless

instance PokemonAttribute Garchomp where
  abilityIs (Garchomp x) = SandVeil
  abilityIs (MegaGarchomp x) = SandForce
  typeIs _ = ListOf2 Dragon Ground
  genderIs (Garchomp x) = getGender x
  genderIs (MegaGarchomp x) = getGender x

instance PokemonAttribute Spiritomb where
  abilityIs (Spiritomb x) = Pressure
  typeIs _ = ListOf2 Ghost Dark
  genderIs (Spiritomb x) = getGender x

instance PokemonAttribute Roserade where
  abilityIs (Roserade x) = getAbility _Roserade'sAbility x
  typeIs _ = ListOf2 Grass Poison
  genderIs (Roserade x) = getGender x

instance PokemonAttribute Togekiss where
  abilityIs (Togekiss x) = getAbility _Togekiss'sAbility x
  typeIs _ = ListOf2 Fairy Flying 
  genderIs (Togekiss x) = getGender x

instance PokemonAttribute Lucario where
  abilityIs (Lucario x) = getAbility _Lucario'sAbility x
  abilityIs (MegaLucario x) = Adaptability
  typeIs _ = ListOf2 Fighting Steel
  genderIs (Lucario x) = getGender x
  genderIs (MegaLucario x) = getGender x

instance PokemonAttribute Glaceon where
  abilityIs (Glaceon x) = SnowCloak
  typeIs _ = ListOf1 Ice
  genderIs (Glaceon x) = getGender x

instance PokemonAttribute Volcarona where
  abilityIs (Volcarona x) = FlameBody
  typeIs _ = ListOf2 Bug Fire
  genderIs (Volcarona x) = getGender x

instance PokemonAttribute Conkeldurr where
  abilityIs (Conkeldurr x) = getAbility _Conkeldurr'sAbility x
  typeIs _ = ListOf1 Fighting
  genderIs (Conkeldurr x) = getGender x

instance PokemonAttribute Reuniclus where
  abilityIs (Reuniclus x) = getAbility _Reuniclus'sAbility x
  typeIs _ = ListOf1 Psychic
  genderIs (Reuniclus x) = getGender x

instance PokemonAttribute Krookodile where
  abilityIs (Krookodile x) = getAbility _Krookodile'sAbility x
  typeIs _ = ListOf2 Ground Dark
  genderIs (Krookodile x) = getGender x

instance PokemonAttribute Chandelure where
  abilityIs (Chandelure x) = getAbility _Chandelure'sAbility x
  typeIs _ = ListOf2 Ghost Fire
  genderIs (Chandelure x) = getGender x

instance PokemonAttribute Braviary where
  abilityIs (Braviary x) = getAbility _Braviary'sAbility x
  typeIs _ = ListOf2 Normal Flying
  genderIs (Braviary _) = Male

--

instance Ord Pikachu where
  compare = statCompare

instance Ord Lapras where
  compare = statCompare

instance Ord Snorlax where
  compare = statCompare

instance Ord Venusaur where
  compare = statCompare

instance Ord Charizard where
  compare = statCompare
  
instance Ord Blastoise where
  compare = statCompare
  
instance Ord Aerodactyl where
  compare = statCompare
  
instance Ord Machamp where
  compare = statCompare
  
instance Ord Alakazam where
  compare = statCompare
  
instance Ord Exeggutor where
  compare = statCompare
  
instance Ord Arcanine where
  compare = statCompare
  
instance Ord Gyarados where
  compare = statCompare
  
instance Ord Dragonite where
  compare = statCompare
  
instance Ord Salamence where
  compare = statCompare
  
instance Ord Kingdra where
  compare = statCompare
  
instance Ord Haxorus where
  compare = statCompare
  
instance Ord Hydreigon where
  compare = statCompare
  
instance Ord Flygon where
  compare = statCompare
  
instance Ord Metagross where
  compare = statCompare
  
instance Ord Aggron where
  compare = statCompare
  
instance Ord Excadrill where
  compare = statCompare
  
instance Ord Archeops where
  compare = statCompare
  
instance Ord Cradily where
  compare = statCompare
  
instance Ord Armaldo where
  compare = statCompare
  
instance Ord Milotic where
  compare = statCompare
  
instance Ord Sharpedo where
  compare = statCompare
  
instance Ord Walrein where
  compare = statCompare
  
instance Ord Ludicolo where
  compare = statCompare
  
instance Ord Swampert where
  compare = statCompare
  
instance Ord Starmie where
  compare = statCompare
  
instance Ord Garchomp where
  compare = statCompare
  
instance Ord Spiritomb where
  compare = statCompare
  
instance Ord Roserade where
  compare = statCompare
  
instance Ord Togekiss where
  compare = statCompare
  
instance Ord Lucario where
  compare = statCompare
  
instance Ord Glaceon where
  compare = statCompare
  
instance Ord Volcarona where
  compare = statCompare
  
instance Ord Conkeldurr where
  compare = statCompare
  
instance Ord Reuniclus where
  compare = statCompare
  
instance Ord Krookodile where
  compare = statCompare
  
instance Ord Chandelure where
  compare = statCompare
  
instance Ord Braviary where
  compare = statCompare

---

instance Semigroup Pikachu where
  (<>) = flip const

instance Semigroup Lapras where
  (<>) = flip const

instance Semigroup Snorlax where
  (<>) = flip const

instance Semigroup Venusaur where
  (<>) = flip const

instance Semigroup Charizard where
  (<>) = flip const
  
instance Semigroup Blastoise where
  (<>) = flip const
  
instance Semigroup Aerodactyl where
  (<>) = flip const
  
instance Semigroup Machamp where
  (<>) = flip const
  
instance Semigroup Alakazam where
  (<>) = flip const
  
instance Semigroup Exeggutor where
  (<>) = flip const
  
instance Semigroup Arcanine where
  (<>) = flip const
  
instance Semigroup Gyarados where
  (<>) = flip const
  
instance Semigroup Dragonite where
  (<>) = flip const
  
instance Semigroup Salamence where
  (<>) = flip const
  
instance Semigroup Kingdra where
  (<>) = flip const
  
instance Semigroup Haxorus where
  (<>) = flip const
  
instance Semigroup Hydreigon where
  (<>) = flip const
  
instance Semigroup Flygon where
  (<>) = flip const
  
instance Semigroup Metagross where
  (<>) = flip const
  
instance Semigroup Aggron where
  (<>) = flip const
  
instance Semigroup Excadrill where
  (<>) = flip const
  
instance Semigroup Archeops where
  (<>) = flip const
  
instance Semigroup Cradily where
  (<>) = flip const
  
instance Semigroup Armaldo where
  (<>) = flip const
  
instance Semigroup Milotic where
  (<>) = flip const
  
instance Semigroup Sharpedo where
  (<>) = flip const
  
instance Semigroup Walrein where
  (<>) = flip const
  
instance Semigroup Ludicolo where
  (<>) = flip const
  
instance Semigroup Swampert where
  (<>) = flip const
  
instance Semigroup Starmie where
  (<>) = flip const
  
instance Semigroup Garchomp where
  (<>) = flip const
  
instance Semigroup Spiritomb where
  (<>) = flip const
  
instance Semigroup Roserade where
  (<>) = flip const
  
instance Semigroup Togekiss where
  (<>) = flip const
  
instance Semigroup Lucario where
  (<>) = flip const
  
instance Semigroup Glaceon where
  (<>) = flip const
  
instance Semigroup Volcarona where
  (<>) = flip const
  
instance Semigroup Conkeldurr where
  (<>) = flip const
  
instance Semigroup Reuniclus where
  (<>) = flip const
  
instance Semigroup Krookodile where
  (<>) = flip const
  
instance Semigroup Chandelure where
  (<>) = flip const
  
instance Semigroup Braviary where
  (<>) = flip const

---

instance Monoid Pikachu where
  mempty = Pikachu mempty

instance Monoid Lapras where
  mempty = Lapras mempty

instance Monoid Snorlax where
  mempty = Snorlax mempty

instance Monoid Venusaur where
  mempty = Venusaur mempty

instance Monoid Charizard where
  mempty = Charizard mempty
  
instance Monoid Blastoise where
  mempty = Blastoise mempty
  
instance Monoid Aerodactyl where
  mempty = Aerodactyl mempty
  
instance Monoid Machamp where
  mempty = Machamp mempty
  
instance Monoid Alakazam where
  mempty = Alakazam mempty
  
instance Monoid Exeggutor where
  mempty = Exeggutor mempty
  
instance Monoid Arcanine where
  mempty = Arcanine mempty
  
instance Monoid Gyarados where
  mempty = Gyarados mempty
  
instance Monoid Dragonite where
  mempty = Dragonite mempty
  
instance Monoid Salamence where
  mempty = Salamence mempty
  
instance Monoid Kingdra where
  mempty = Kingdra mempty
  
instance Monoid Haxorus where
  mempty = Haxorus mempty
  
instance Monoid Hydreigon where
  mempty = Hydreigon mempty
  
instance Monoid Flygon where
  mempty = Flygon mempty
  
instance Monoid Metagross where
  mempty = Metagross mempty
  
instance Monoid Aggron where
  mempty = Aggron mempty
  
instance Monoid Excadrill where
  mempty = Excadrill mempty
  
instance Monoid Archeops where
  mempty = Archeops mempty
  
instance Monoid Cradily where
  mempty = Cradily mempty
  
instance Monoid Armaldo where
  mempty = Armaldo mempty
  
instance Monoid Milotic where
  mempty = Milotic mempty
  
instance Monoid Sharpedo where
  mempty = Sharpedo mempty
  
instance Monoid Walrein where
  mempty = Walrein mempty
  
instance Monoid Ludicolo where
  mempty = Ludicolo mempty
  
instance Monoid Swampert where
  mempty = Swampert mempty
  
instance Monoid Starmie where
  mempty = Starmie mempty
  
instance Monoid Garchomp where
  mempty = Garchomp mempty
  
instance Monoid Spiritomb where
  mempty = Spiritomb mempty
  
instance Monoid Roserade where
  mempty = Roserade mempty
  
instance Monoid Togekiss where
  mempty = Togekiss mempty
  
instance Monoid Lucario where
  mempty = Lucario mempty
  
instance Monoid Glaceon where
  mempty = Glaceon mempty
  
instance Monoid Volcarona where
  mempty = Volcarona mempty
  
instance Monoid Conkeldurr where
  mempty = Conkeldurr mempty
  
instance Monoid Reuniclus where
  mempty = Reuniclus mempty
  
instance Monoid Krookodile where
  mempty = Krookodile mempty
  
instance Monoid Chandelure where
  mempty = Chandelure mempty
  
instance Monoid Braviary where
  mempty = Braviary mempty

--

instance PokemonPhysiology Pikachu where 
  weight _ = Kilograms 6.0
  height _ = Meters 0.4

instance PokemonPhysiology Lapras where 
  weight _ = Kilograms 220.0
  height _ = Meters 2.5

instance PokemonPhysiology Snorlax where 
  weight _ = Kilograms 460.0
  height _ = Meters 2.1

instance PokemonPhysiology Venusaur where 
  weight (Venusaur _) = Kilograms 100.0
  weight (MegaVenusaur _) = Kilograms 155.5
  height (Venusaur _) = Meters 2.0
  height (MegaVenusaur _) = Meters 2.4

instance PokemonPhysiology Charizard where 
  weight (Charizard _) = Kilograms 90.5
  weight (MegaCharizardX _) = Kilograms 110.5
  weight (MegaCharizardY _) = Kilograms 100.5
  height _ = Meters 1.7

instance PokemonPhysiology Blastoise where 
  weight (Blastoise _) = Kilograms 85.5
  weight (MegaBlastoise _) = Kilograms 101.1
  height _ = Meters 1.6

instance PokemonPhysiology Aerodactyl where 
  weight (Aerodactyl _) = Kilograms 59.0
  weight (MegaAerodactyl _) = Kilograms 79.0
  height (Aerodactyl _) = Meters 1.8
  height (MegaAerodactyl _) = Meters 2.1

instance PokemonPhysiology Machamp where 
  weight _ = Kilograms 130.0
  height _ = Meters 1.6

instance PokemonPhysiology Alakazam where 
  weight _ = Kilograms 48.0
  height (Alakazam _) = Meters 1.5
  height (MegaAlakazam _) = Meters 1.2

instance PokemonPhysiology Exeggutor where 
  weight (Exeggutor _) = Kilograms 120.0
  weight (AlolanExeggutor _) = Kilograms 415.6
  height (Exeggutor _) = Meters 2.0
  height (AlolanExeggutor _) = Meters 10.9

instance PokemonPhysiology Arcanine where 
  weight _ = Kilograms 155.0
  height _ = Meters 1.9

instance PokemonPhysiology Gyarados where 
  weight (Gyarados _) = Kilograms 235.0
  weight (MegaGyarados _) = Kilograms 305.0
  height (Gyarados _) = Meters 6.0
  height (MegaGyarados _) = Meters 6.5

instance PokemonPhysiology Dragonite where 
  weight _ = Kilograms 210.0
  height _ = Meters 2.2

instance PokemonPhysiology Salamence where 
  weight (Salamence _) = Kilograms 102.6
  weight (MegaSalamence _) = Kilograms 112.6
  height (Salamence _) = Meters 1.5
  height (MegaSalamence _) = Meters 1.8

instance PokemonPhysiology Kingdra where 
  weight _ = Kilograms 152.0
  height _ = Meters 1.8

instance PokemonPhysiology Haxorus where 
  weight _ = Kilograms 105.5
  height _ = Meters 1.8

instance PokemonPhysiology Hydreigon where 
  weight _ = Kilograms 160.0
  height _ = Meters 1.8

instance PokemonPhysiology Flygon where 
  weight _ = Kilograms 82.0
  height _ = Meters 2.0

instance PokemonPhysiology Metagross where 
  weight (Metagross _) = Kilograms 550.0
  weight (MegaMetagross _) = Kilograms 942.9
  height (Metagross _) = Meters 1.6
  height (MegaMetagross _) = Meters 2.5

instance PokemonPhysiology Aggron where 
  height (Aggron _) = Meters 2.1
  height (MegaAggron _) = Meters 2.2
  weight (Aggron _) = Kilograms 360.0
  weight (MegaAggron _) = Kilograms 395.0

instance PokemonPhysiology Excadrill where 
  weight _ = Kilograms 40.4
  height _ = Meters 0.7

instance PokemonPhysiology Archeops where 
  weight _ = Kilograms 32.0
  height _ = Meters 1.4

instance PokemonPhysiology Cradily where 
  weight _ = Kilograms 60.4
  height _ = Meters 1.5

instance PokemonPhysiology Armaldo where 
  weight _ = Kilograms 68.2
  height _ = Meters 1.5

instance PokemonPhysiology Milotic where 
  weight _ = Kilograms 162.0
  height _ = Meters 6.2

instance PokemonPhysiology Sharpedo where 
  weight (Sharpedo _) = Kilograms 88.8
  weight (MegaSharpedo _) = Kilograms 130.3
  height (Sharpedo _) = Meters 1.8
  height (MegaSharpedo _) = Meters 2.5

instance PokemonPhysiology Walrein where 
  weight _ = Kilograms 150.6
  height _ = Meters 1.4

instance PokemonPhysiology Ludicolo where 
  weight _ = Kilograms 55.0
  height _ = Meters 1.5

instance PokemonPhysiology Swampert where 
  weight (Swampert _) = Kilograms 81.9
  weight (MegaSwampert _) = Kilograms 102.0
  height (Swampert _) = Meters 1.5
  height (MegaSwampert _) = Meters 1.9

instance PokemonPhysiology Starmie where 
  weight _ = Kilograms 80.0
  height _ = Meters 1.1

instance PokemonPhysiology Garchomp where 
  weight _ = Kilograms 95.0
  height _ = Meters 1.9

instance PokemonPhysiology Spiritomb where 
  weight _ = Kilograms 108.0
  height _ = Meters 1.0

instance PokemonPhysiology Roserade where 
  weight _ = Kilograms 14.5
  height _ = Meters 0.9

instance PokemonPhysiology Togekiss where 
  weight _ = Kilograms 38.0
  height _ = Meters 1.5

instance PokemonPhysiology Lucario where 
  weight (Lucario _) = Kilograms 54.0
  weight (MegaLucario _) = Kilograms 57.5
  height (Lucario _) = Meters 1.2
  height (MegaLucario _) = Meters 1.3

instance PokemonPhysiology Glaceon where 
  weight _ = Kilograms 25.9
  height _ = Meters 0.8

instance PokemonPhysiology Volcarona where 
  weight _ = Kilograms 46.0
  height _ = Meters 1.6

instance PokemonPhysiology Conkeldurr where 
  weight _ = Kilograms 87.0
  height _ = Meters 1.4

instance PokemonPhysiology Reuniclus where 
  weight _ = Kilograms 20.1
  height _ = Meters 1.0

instance PokemonPhysiology Krookodile where 
  weight _ = Kilograms 96.3
  height _ = Meters 1.5

instance PokemonPhysiology Chandelure where 
  weight _ = Kilograms 34.3
  height _ = Meters 1.0

instance PokemonPhysiology Braviary where 
  weight _ = Kilograms 41.0
  height _ = Meters 1.5

  
---

data Species ab gndr = Species
  {
    _speciesAbility :: ab
  , _speciesGender :: gndr
  } deriving (Eq,Show,Read,Generic)


type Species' = Species ()

type GenderlessSpecies ab = Species ab ()

type GenderlessSpecies' = GenderlessSpecies ()

instance Semigroup (Species ab gndr) where
  a <> b = b

instance (Monoid ab, Monoid gndr) => Monoid (Species ab gndr) where
  mempty = Species mempty mempty


speciesAbility :: Lens' (Species ab gndr) ab
speciesAbility = lens _speciesAbility (\m n -> m {_speciesAbility = n})

speciesGender :: Lens' (Species ab gndr) gndr
speciesGender = lens _speciesGender (\m n -> m {_speciesGender = n})

getGender :: GenderParser g => Species a g -> Gender
getGender = review _Gender . view speciesGender

getAbility :: Prism' Ability a -> Species a g -> Ability
getAbility pr = review pr . view speciesAbility
