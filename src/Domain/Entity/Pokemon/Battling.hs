module Domain.Entity.Pokemon.Battling where

import Domain.Entity.Pokemon.Species
import Domain.Attribute.Ability
import Domain.Attribute.HeldItem
import Domain.Attribute.Gender
import qualified Domain.Attribute.Statistic as S
import qualified Domain.Attribute.PokemonFactors as F
import Domain.Attribute.Nature as N

data Pokemon m = Pokemon
  {
    pokemonName :: String
  , species :: PokemonSpecies
  , moves :: [m]
  , ability :: Ability
  , heldItem :: HeldItem
  , gender :: Gender
  , level :: S.Level
  , ivs :: F.IVs
  , evs :: F.EVs
  , nature :: N.Nature
  } deriving (Eq,Show)

basePokemon :: PokemonSpecies -> [m] -> Pokemon m
basePokemon spec mvs = Pokemon
  {
    pokemonName = "???"
  , species = spec
  , moves = mvs
  , ability = NoAbility
  , heldItem = SitrusBerry
  , gender = Male
  , level = 50
  , ivs = F.IVs 31 31 31 31 31 31
  , evs = F.EVs 85 85 85 85 85 85
  , nature = Timid
  }
