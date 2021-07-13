{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE ScopedTypeVariables #-}


module Domain.Entity.Structured.Move where

import Control.Lens
import qualified Domain.Entity.Move as M
import Domain.Entity.Common hiding ((.))


import qualified Domain.Entity.GenV.Move as Mv

data Move rest typeof choice succ hit damage eff = Move
  {
    _moveName :: String
  , _moveChoice :: choice
  , _moveType :: typeof
  , _movePP :: Int
  , _moveSuccess :: AST succ
  , _moveHit :: AST (M.Accuracy hit)
  , _moveDamage :: Damage damage
  , _movePriority :: Int
  , _moveEff :: AST eff
  , _moveMakesContact :: Bool
  , _moveRest :: rest
  } deriving (Eq,Show,Ord)

data Damage damage =
  StatusMove
  | Damage (DamageCalc damage)
  deriving (Eq,Show,Ord)

data DamageCalc damage = DamageCalc
  {
    _damageCalcBasepower :: Maybe Int
  , _damageCalcCategory :: DamageCategory
  , _damageCalcFormula :: AST damage
  } deriving (Eq,Show,Ord)

data DamageCategory = Physical | Special deriving (Eq,Show,Ord,Enum)

makeLenses ''Move

---

filterElem :: (a -> Bool) -> AST a -> [a]
filterElem f mv = view astAsList (over astAsList (filter f) mv)

unambiguous :: (a -> Bool) -> (a -> b) -> s -> AST a -> Either s b
unambiguous g f s as = case length . filterElem g $ as of
                     ((>) 1 -> True) -> Left s
                     0 -> Left s
                     _ -> Right (f . head . filterElem g $ as)

combineAll :: (a -> Bool) -> (a -> b) -> AST a -> AST b
combineAll g f = view listAsAst . map f . filterElem g 

eitherToMaybe :: Either a b -> Maybe b
eitherToMaybe (Left x) = Nothing
eitherToMaybe (Right x) = Just x

---

parseName :: M.Move typeof choice succ hit damage eff -> Either String String
parseName =
  unambiguous (\case {(M.Name x) -> True; _ -> False} ) (\(M.Name x) -> x) "Name must appear only once."

-- parsePP :: M.Move typeof choice succ hit damage eff -> Either String Int
parsePP =
  unambiguous (\case {(M.PP x) -> True; _ -> False}) (\case {(M.PP x) -> x}) "PP must appear only once."

parseType =
  unambiguous (\case {(M.TypeOf x) -> True; _ -> False}) (\case {(M.TypeOf x) -> x}) "Type must appear only once."

parseSuccess = combineAll (\case {(M.MoveSucceeds x) -> True; _ -> False}) (\(M.MoveSucceeds x) -> x)

parseDamage = combineAll (\case {(M.DamageDealing x) -> True; _ -> False}) (\(M.DamageDealing x) -> x)

parseBasepower = eitherToMaybe . unambiguous (\case {(M.Basepower x) -> True; _ -> False}) (\(M.Basepower x) -> x) ""

parseCategory = unambiguous f g "Category has to be either Special or Physical for damaging moves."
  where
    f (M.Category M.Status) = False
    f (M.Category _ ) = True
    f _ = False
    g (M.Category M.Physical) = Physical
    g (M.Category M.Special) = Special

parseAccuracy = f . combineAll  (\case {(M.Accuracy x) -> True; _ -> False}) (\(M.Accuracy x) -> x)
  where
    f End = Branch (M.HitProbability 1.00) End
    f x = x

parsePriority = f .  combineAll  (\case {(M.Priority x) -> True; _ -> False}) (\(M.Priority x) -> x) 
  where
    f End = Right 0
    f (Branch x End) = Right x
    f (Branch x y) = Left "too many priotity levels given, unable to pick right one."

parseSideEffect = combineAll (\case {(M.SideEffect x) -> True; _ -> False}) (\(M.SideEffect x) -> x)

parseContact = f . combineAll   (\case {M.MakesContact -> True; _ -> False}) (\M.MakesContact -> True)
  where
    f End = Right False
    f _ = Right True

-- parseStatus :: M.Move typeof choice success prob damage effect -> Either String (Damage damage)
parseStatus = unambiguous f g "Category has to be Status, if no damage done."
  where
    f (M.Category M.Status) = True
    f _ = False
    g (M.Category M.Status) = StatusMove


parseDamageCalc ast =
  DamageCalc (parseBasepower ast) <$> parseCategory ast <*> (pure $ parseDamage ast)

parseDamageCalcOrStatus ast =
  case parseDamageCalc ast of
    Right x -> Right (Damage x)
    Left x -> parseStatus ast

parseMove ast = Move <$>
  parseName ast <*>
  return () <*>
  parseType ast <*>
  parsePP ast <*>
  return (parseSuccess ast) <*>
  return (parseAccuracy ast) <*>
  parseDamageCalcOrStatus ast <*>
  parsePriority ast <*>
  return (parseSideEffect ast) <*>
  parseContact ast <*>
  return ()
