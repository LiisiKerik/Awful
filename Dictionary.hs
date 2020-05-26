--------------------------------------------------------------------------------------------------------------------------------
{-# OPTIONS_GHC -Wall #-}
module Dictionary (Dictionary, Entry, Dictionary.insert, Dictionary.lookup) where -- Dictionary.delete
  import Data.Map (Map, insert, lookup) -- delete
  import Errors (Err', Error' (..), L (..))
  type Dictionary = Map String
  type Entry = (,) String
  insert :: String -> t -> Dictionary t -> Either t (Dictionary t)
  insert name x dictionary =
    case Data.Map.lookup name dictionary of
      Nothing -> Right (Data.Map.insert name x dictionary)
      Just y -> Left y
-- todo: move undefined error to some other module?
  lookup :: String -> L String -> Dictionary t -> Err' (Entry t)
  lookup typ (L line_and_char name) dictionary =
    case Data.Map.lookup name dictionary of
      Nothing -> Left (Undefined typ name line_and_char)
      Just x -> Right (name, x)
--------------------------------------------------------------------------------------------------------------------------------