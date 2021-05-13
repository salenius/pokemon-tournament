module Effect.Logic.Flinch where

import Attribute.Ability
import Attribute.Item
import Attribute.Counterparty
import Effect.Data as Eff
import Control.Lens
import Data.Maybe
import Control.Applicative

data Condition =
  Basecase
  | Steadfast' (TargetHas Steadfast)
  | MoldBreaker' (UserHas MoldBreaker) Condition
  | InnerFocus' (TargetHas InnerFocus)
  deriving (Eq,Show,Ord)

data Flinch =
  FlinchTarget
  | DontFlinch
  | RaiseSpeed Int
  deriving (Eq,Show,Ord)

decide :: Condition -> Effect Flinch
decide Basecase = pure FlinchTarget
decide (Steadfast' _) = pure FlinchTarget `chain` pure (RaiseSpeed 1) 
decide (MoldBreaker' _ _) = doNothing
decide (InnerFocus' _) = pure FlinchTarget

data FlinchData = FlinchData
  {
    userAbility :: PokemonAbility
  , targetAbility :: PokemonAbility
  } deriving (Eq,Show,Ord)

class AsFlinchData b where
  asFlinchData :: Lens' b FlinchData

steadfast' :: FlinchData -> Maybe Condition
steadfast' fdata = do
  ab <- preview _Steadfast $ targetAbility fdata
  return $ Steadfast' $ TargetHas ab

moldBreaker' :: FlinchData -> Maybe Condition
moldBreaker' fdata = do
  uab <- preview _MoldBreaker $ userAbility fdata
  tab <- innerFocus' fdata
  return $ MoldBreaker' (UserHas uab) tab

innerFocus' :: FlinchData -> Maybe Condition
innerFocus' fdata = do
  ab <- preview _InnerFocus $ targetAbility fdata
  return $ InnerFocus' $ TargetHas ab

getCondition :: FlinchData -> Condition
getCondition fdata = fromMaybe Basecase $
  steadfast' fdata <|>
  moldBreaker' fdata <|>
  innerFocus' fdata
  
