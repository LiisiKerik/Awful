--------------------------------------------------------------------------------------------------------------------------------
module Dictionary (Dictionary, Dictionary', Status (..), add, search) where
  import Data.Map
  import Errors
  type Dictionary = Map String
  type Dictionary' = Map (String, String)
  data Status = Private | Public deriving (Eq, Show)
  add :: Ord t => t -> u -> Map t u -> Either u (Map t u)
  add x y z =
    case Data.Map.lookup x z of
      Nothing -> Right (insert x y z)
      Just a -> Left a
  search :: String -> String -> Dictionary t -> Name -> Err t
  search kind file_name x (Name line_and_char y) =
    case Data.Map.lookup y x of
      Nothing -> Left (Undefined kind y file_name line_and_char)
      Just z -> Right z
--------------------------------------------------------------------------------------------------------------------------------