module Domain.Data.Trainer where

import Domain.Data.Pokemon
import Domain.Entity.Trainer
import Domain.Attribute.TrainerFactors
  
trainerRed =
  Trainer "Red" Kanto Champion $
  PartyOf6 red'sPikachu red'sLapras red'sSnorlax red'sVenusaur red'sCharizard red'sBlastoise

trainerBlue =
  Trainer "Blue" Kanto Champion $
  PartyOf6 blue'sAerodactyl blue'sMachamp blue'sAlakazam blue'sExeggutor blue'sArcanine blue'sGyarados

trainerLance =
  Trainer "Lance" Johto Champion $
  PartyOf6 lance'sDragonite lance'sSalamence lance'sKingdra lance'sHaxorus lance'sHydreigon lance'sFlygon

trainerSteven =
  Trainer "Steven" Hoenn Champion $
  PartyOf6 steven'sMetagross steven'sAggron steven'sExcadrill steven'sArcheops steven'sCradily steven'sArmaldo

trainerWallace =
  Trainer "Wallace" Hoenn Champion $
  PartyOf6 wallace'sMilotic wallace'sSharpedo wallace'sWalrein wallace'sLudicolo wallace'sSwampert wallace'sStarmie

trainerCynthia =
  Trainer "Cynthia" Sinnoh Champion $
  PartyOf6 cynthia'sGarchomp cynthia'sSpiritomb cynthia'sRoserade cynthia'sTogekiss cynthia'sLucario cynthia'sGlaceon

trainerAlder =
  Trainer "Alder" Unova Champion $
  PartyOf6 alder'sVolcarona alder'sConkeldurr alder'sReuniclus alder'sKrookodile alder'sChandelure alder'sBraviary
