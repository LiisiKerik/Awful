--------------------------------------------------------------------------------------------------------------------------------
{-# OPTIONS_GHC -Wall #-}
module Dictionary (Dictionary, delete_keys, search) where
  import Data.Map
  import Errors
  import Parser
  type Dictionary = Map String
  delete_keys :: Ord t => Map t u -> [t] -> Map t u
  delete_keys = Prelude.foldr delete
  search :: String -> Name -> Dictionary t -> Err (String, t)
  search typ (Name line_and_char name) dictionary =
    case Data.Map.lookup name dictionary of
      Nothing -> Left (Undefined typ name line_and_char)
      Just x -> Right (name, x)
--------------------------------------------------------------------------------------------------------------------------------