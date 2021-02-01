module Domain.Match.TypeEffect where

import Domain.Attribute.TypeOf
import Domain.Attribute.HeldItem
import Domain.Match.Validation
import Domain.Attribute.Counterparty

data TypeEffect =
  NoEffect
  | NotVeryEffective
  | NormalEffect
  | SuperEffective
  deriving (Eq,Show,Ord)

effectAsDouble :: TypeEffect -> Double
effectAsDouble NoEffect = 0
effectAsDouble NotVeryEffective = 0.5
effectAsDouble NormalEffect = 1
effectAsDouble SuperEffective = 2

modifyEffect :: (TypeOf, TypeOf, TypeEffect) -> (TypeOf -> TypeOf -> TypeEffect) -> (TypeOf -> TypeOf -> TypeEffect)
modifyEffect (mvType',trgtType',newEffect) orig mvType trgtType
  | mvType == mvType' && trgtType' == trgtType = newEffect
  | otherwise = orig mvType trgtType

typeEffect :: TypeOf -> TypeOf -> TypeEffect
typeEffect Normal x
  | x == Rock && x == Steel = NotVeryEffective
  | x == Ghost = NoEffect
typeEffect Fighting x
  | x `elem` [Rock,Steel,Ice,Dark,Normal] = SuperEffective
  | x `elem` [Poison,Flying,Psychic,Bug,Fairy] = NotVeryEffective
  | x == Ghost = NoEffect
typeEffect Flying x
  | x `elem` [Grass,Fighting,Bug] = SuperEffective
  | x `elem` [Electric,Rock,Steel] = NotVeryEffective
typeEffect Water x
  | x `elem` [Fire,Ground,Rock] = SuperEffective
  | x `elem` [Water,Grass,Dragon] = NotVeryEffective
typeEffect Fire x
  | x `elem` [Grass,Ice,Bug,Steel] = SuperEffective
  | x `elem` [Fire,Water,Rock,Dragon] = NotVeryEffective
typeEffect Grass x
  | x `elem` [Water,Ground,Rock] = SuperEffective
  | x `elem` [Fire,Grass,Poison,Flying,Bug,Dragon,Steel] = NotVeryEffective
typeEffect Electric x
  | x `elem` [Water,Flying] = SuperEffective
  | x `elem` [Electric,Grass,Dragon] = NotVeryEffective
  | x == Ground = NoEffect
typeEffect Ground x
  | x `elem` [Fire,Electric,Poison,Rock,Steel] = SuperEffective
  | x `elem` [Grass,Bug] = NotVeryEffective
  | x == Flying = NoEffect
typeEffect Rock x
  | x `elem` [Fire,Ice,Flying,Bug] = SuperEffective
  | x `elem` [Fighting,Ground,Steel] = NotVeryEffective
typeEffect Poison x
  | x `elem` [Grass,Fairy] = SuperEffective
  | x `elem` [Poison,Ground,Rock,Ghost] = NotVeryEffective
  | x == Steel = NoEffect
typeEffect Bug x
  | x `elem` [Grass,Psychic,Dark] = SuperEffective
  | x `elem` [Fire,Fighting,Poison,Flying,Ghost,Steel,Fairy] = NotVeryEffective
typeEffect Ice x
  | x `elem` [Grass,Ground,Flying,Dragon] = SuperEffective
  | x `elem` [Fire,Water,Ice,Steel] = NotVeryEffective
typeEffect Steel x
  | x `elem` [Ice,Rock,Fairy] = SuperEffective
  | x `elem` [Fire,Water,Electric,Steel] = NotVeryEffective
typeEffect Psychic x
  | x `elem` [Fighting,Poison] = SuperEffective
  | x `elem` [Psychic,Steel] = NotVeryEffective
  | x == Dark = NoEffect
typeEffect Dark x
  | x `elem` [Psychic,Ghost] = SuperEffective
  | x `elem` [Fighting,Dark,Fairy] = NotVeryEffective
typeEffect Ghost x
  | x `elem` [Psychic,Ghost] = SuperEffective
  | x `elem` [Dark] = NotVeryEffective
  | x == Normal = NoEffect
typeEffect Dragon x
  | x == Dragon = SuperEffective
  | x == Steel = NotVeryEffective
  | x == Fairy = NoEffect
typeEffect Fairy x
  | x `elem` [Fighting,Dragon,Dark] = SuperEffective
  | x `elem` [Fire,Poison,Steel] = NotVeryEffective
typeEffect x y = NormalEffect  

totalTypeEffect :: (TypeOf -> TypeOf -> TypeEffect) -> TypeOf -> [TypeOf] -> Double
totalTypeEffect eff mvType pkmnType = foldr (*) 1 . map effectAsDouble . map (eff mvType) $ pkmnType

totalTypeEffect' = totalTypeEffect typeEffect

berry' :: (TypeOf -> [TypeOf] -> Double) -> TypeOf -> [TypeOf] -> HeldItem -> Double
berry' eff mvType pkmnType item
  | item == ChilanBerry && mvType == Normal = 0.5
  | eff mvType pkmnType <= 1 = 1
  | item == OccaBerry && mvType == Fire = 0.5
  | item == ChartiBerry && mvType == Rock = 0.5
berry' _ _ _ _ = 1

-- berry :: (TypeOf -> [TypeOf] -> Double) -> TypeOf -> [TypeOf] -> Maybe HeldItem -> Double
-- berry eff a b Nothing = 1
-- berry eff a b (Just x) = berry' eff a b x

berry :: TypeOf -> (TypeOf -> [TypeOf] -> Double) -> Validation
berry mvtp tfn =
  berryIs' 0 ChilanBerry Normal `And`
  berryIs OccaBerry Fire `And`
  berryIs ChartiBerry Rock
  where
    berryIs' r b t' = 
      Target `heldItemIs` (\it -> it == Just b) `And`
      (Target `typeIs` (\t -> tfn mvtp t > r && (t' == mvtp)))
    berryIs = berryIs' 1
