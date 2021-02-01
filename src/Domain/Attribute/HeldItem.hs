module Domain.Attribute.HeldItem where

data HeldItem =
    AirBalloon
    | ChartiBerry
    | ChoiceBand
    | ChoiceScarf
    | ExpertBelt
    | FlyingGem
    | FocusSash
    | KingsRock
    | Leftovers
    | LifeOrb
    | LightBall
    | OccaBerry
    | PowerHerb
    | QuickClaw
    | RazorClaw
    | RindoBerry
    | RockyHelmet
    | ScopeLens
    | SitrusBerry
    | WhiteHerb
    | ChilanBerry
    | ColburBerry
    | HabanBerry
    | WacanBerry
    | RoseliBerry
    | ChopleBerry
    | CobaBerry
    | KasibBerry
    | ShucaBerry
    | YacheBerry
    | KebiaBerry
    | BabiriBerry
    | PasshoBerry
    | TangaBerry
    | PayapaBerry
   deriving (Eq, Show, Read, Enum, Ord, Bounded)

allHeldItems :: [HeldItem]
allHeldItems = enumFrom minHeldItem
  where
    minHeldItem = minBound :: HeldItem

