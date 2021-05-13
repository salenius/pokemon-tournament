{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE PatternSynonyms #-}


module Effect.Logic.Ailment where

import Attribute.Weather
import Attribute.Ability
import Attribute.Counterparty
import Attribute.TypeOf
import Types.Pokemon
import Effect.Data
import Effect.Logic.AilmentDecision
import Effect.Logic.AilmentParse
import Effect.Logic.AilmentData
import Control.Monad.Reader
import Control.Lens
import Control.Applicative
import Data.Maybe


---

parseAilment :: Env (AilmentCondition a m s t l) -> AilmentData -> AilmentCondition a m s t l
parseAilment eff ad =
  fromMaybe Basecase $
  flip runReaderT ad $
  eff

targetPoisoned :: AilmentData -> TargetPoisoned
targetPoisoned ad =
  fromMaybe Basecase $
  flip runReaderT ad $ do
  alreadyAilment <|>
    safeguardBlocks <|>
    typeImmunity <|>
    synchronizeKicks <|>
    moldBreaker' immunity <|>
    ability' immunity

userPoisoned :: AilmentData -> UserPoisoned
userPoisoned ad = undefined

targetBurned :: AilmentData -> TargetBurned
targetBurned ad = undefined

userBurned :: AilmentData -> UserBurned
userBurned ad = undefined

targetParalyzed :: AilmentData -> TargetParalyzed
targetParalyzed ad = undefined

userParalyzed :: AilmentData -> UserParalyzed
userParalyzed ad = undefined


--

----------------
  








 

--

