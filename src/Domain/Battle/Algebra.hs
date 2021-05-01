{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE FlexibleInstances #-}


module Domain.Battle.Algebra where

import Control.Lens
import Data.List.NonEmpty
import Domain.Attribute.Quadruple
import Domain.Battle.Op
import Domain.Battle.State
import Domain.Battle.State.Pokemon
import Domain.Battle.Effect
import Domain.Entity.BuiltIn.Move
import Domain.Entity.Stats.Pokemon
import Domain.Entity.BuiltIn.Pokemon
import Domain.Entity.Pokemon
import qualified Domain.Entity.BuiltIn.Move.Status as Status
import qualified Domain.Entity.BuiltIn.Move.Attacking as Attacking

class AsMoveOp mv where
  toMoveOp :: mv -> MoveOp BattleEffect BattleState 

class AsPokemonSpecies pkmn where
  toPokemonSpecies :: pkmn -> (PokemonSpecies, Quadruple (MoveOp BattleEffect BattleState))

class FromTrainer trnr where
  fromTrainer :: trnr -> NonEmpty (PokemonSpecies, Quadruple (MoveOp BattleEffect BattleState))


---

-- Nämä pitää korjata niin, että eri liikkeiden vaikutukset implementoidaan kunnolla

instance AsMoveOp Status.Move where
  toMoveOp _ = MoveOp return $ randomInt (1,10)

instance AsMoveOp Attacking.Move where
  toMoveOp _ = MoveOp return $ randomInt (1,10)

instance AsMoveOp Move where
  toMoveOp (preview _StatusMove -> Just mv) = toMoveOp mv
  toMoveOp (preview _DamagingMove -> Just mv) = toMoveOp mv

instance AsPokemonSpecies (Pokemon Red'sPokemon) where
  toPokemonSpecies = pokemonTuple

instance AsPokemonSpecies (Pokemon Blue'sPokemon) where
  toPokemonSpecies = pokemonTuple

instance AsPokemonSpecies (Pokemon Lance'sPokemon) where
  toPokemonSpecies = pokemonTuple

instance AsPokemonSpecies (Pokemon Steven'sPokemon) where
  toPokemonSpecies = pokemonTuple

instance AsPokemonSpecies (Pokemon Wallace'sPokemon) where
  toPokemonSpecies = pokemonTuple

instance AsPokemonSpecies (Pokemon Cynthia'sPokemon) where
  toPokemonSpecies = pokemonTuple

instance AsPokemonSpecies (Pokemon Alder'sPokemon) where
  toPokemonSpecies = pokemonTuple

pokemonTuple pkmn = (mkPokemonSpecies pkmn, fmap toMoveOp . moves . view pokemonSpecies $ pkmn)
