{-# LANGUAGE MultiParamTypeClasses, FlexibleContexts, FlexibleInstances, TemplateHaskell, UndecidableInstances#-}

module Domain.Battle.Damage where

import Domain.Attribute.Damage as D
import Domain.Attribute.CriticalHit
import Domain.Attribute.Category
import Domain.Attribute.TypeOf
import Domain.Attribute.Counterparty
import Domain.Attribute.Player
import qualified Control.Lens as L
import qualified Data.Map as Map
import Domain.Entity.Battle
import Domain.Entity.Move
import Domain.Entity.PlayerState
import Control.Monad.Reader
import Control.Monad.State

type DamageCalc rm a = StateT (StrikePhase PassivePlayerState) rm a

data DamageEnv m = DamageEnv
  {
    getRandomInteger :: m Int
  , getRandomDouble :: m Double
  , logInformation :: String -> m ()
  } 


totalDamage :: Monad m => DamageMove -> DamageCalc (ReaderT (DamageEnv m) m) Damage
totalDamage mv = do
  env <- ask
  crprob <- getRandomDouble env
  cr <- isCriticalHit mv crprob
  case cr of
    IsCritical -> logInformation "The hit is critical"
    _ -> pure ()
  typad <- typeAdvantage mv 
  case typad of
