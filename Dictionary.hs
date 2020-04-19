--------------------------------------------------------------------------------------------------------------------------------
{-# OPTIONS_GHC -Wall #-}
module Dictionary (Dictionary, Entry, Name (..), Dictionary.insert, Dictionary.lookup) where -- Dictionary.delete
  import Data.Map (Map, insert, lookup) -- delete
  import Errors (Err', Error' (..), Line_and_char)
  type Dictionary = Map String
  type Entry = (,) String
  data Name = Name Line_and_char String
      deriving Show
  insert :: String -> t -> Dictionary t -> Either t (Dictionary t)
  insert name x dictionary =
    case Data.Map.lookup name dictionary of
      Nothing -> Right (Data.Map.insert name x dictionary)
      Just y -> Left y
  lookup :: String -> Name -> Dictionary t -> Err' (Entry t)
  lookup typ (Name line_and_char name) dictionary =
    case Data.Map.lookup name dictionary of
      Nothing -> Left (Undefined typ name line_and_char)
      Just x -> Right (name, x)
--------------------------------------------------------------------------------------------------------------------------------