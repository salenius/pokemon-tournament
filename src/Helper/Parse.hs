module Helper.Parse where

import Data.Maybe

parseAndAct :: (a -> Maybe b) -> (b -> c -> c) -> (c -> c) -> a -> c -> c
parseAndAct prser action alter dta val = fromMaybe (alter val) $ do
  x <- prser dta
  return $ action x val

parseAndAct' prser action = parseAndAct prser action id
