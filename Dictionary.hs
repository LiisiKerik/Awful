--------------------------------------------------------------------------------------------------------------------------------
{-# OPTIONS_GHC -Wall #-}
module Dictionary (
  Dictionary,
  Dictionary',
  Entry,
  New_dictionary,
  New_dictionary',
  New_entry,
  Status (..),
  add,
  all_entries,
  public_entries,
  search) where
  import Data.Map
  import Errors
  type Dictionary = Map String
  type Dictionary' = Map (String, String)
  type Entry = (,) String
  type New_dictionary t = Dictionary (t, Status)
  type New_dictionary' t = Dictionary' (t, Status)
  type New_entry t = Entry (t, Status)
  data Status = Private | Public deriving (Eq, Ord, Show)
  all_entries :: Map t (u, Status) -> Map t u
  all_entries = fmap fst
  add :: Ord t => t -> u -> Map t u -> Either u (Map t u)
  add x y z =
    case Data.Map.lookup x z of
      Nothing -> Right (insert x y z)
      Just a -> Left a
  public :: (t, Status) -> Bool
  public (_, status) = Public == status
  public_entries :: Map t (u, Status) -> Map t u
  public_entries dictionary = all_entries (Data.Map.filter public dictionary)
  search :: String -> String -> Dictionary t -> Name -> Err t
  search kind file_name x (Name line_and_char y) =
    case Data.Map.lookup y x of
      Nothing -> Left (Undefined kind y file_name line_and_char)
      Just z -> Right z
--------------------------------------------------------------------------------------------------------------------------------