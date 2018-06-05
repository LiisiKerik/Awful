-----------------------------------------------------------------------------------------------------------------------------
{-# OPTIONS_GHC -Wall #-}
module Standard where
  import Data.Bifunctor
  import Data.Map
  import Tokenise
  import Tree
  data Def_1 =
    Basic_def_1 Name [(Name, Kind_0)] [Constraint_0] Type_7 Expression_0 |
    Instance_1 Location_0 Name Name [Kind_0] [Pattern_1] [Constraint_0] [(Name, Expression_0)]
      deriving Show
  type Map' t = Map String t
  data Tree_2 = Tree_2 [Data_0] [Class_0] [Def_1] deriving Show
  data Tree_3 = Tree_3 [Name] Tree_2 deriving Show
  pop :: (t -> t -> t, String -> t) -> [(Op, t)] -> t -> Op -> [(Op, t)]
  pop f x expr (Op pr assoc name) =
    let
      t = (Op pr assoc name, expr) : x
    in
      case x of
        [] -> t
        (Op pr' assoc' name', expr') : x' ->
          case pr' < pr || pr' == pr && assoc' == Lft of
            False -> t
            True -> pop' pop f x' name' expr' expr (Op pr assoc name)
  pop' ::
    ((t -> t -> t, String -> t) -> [(Op, t)] -> t -> u) -> (t -> t -> t, String -> t) -> [(Op, t)] -> String -> t -> t -> u
  pop' h (f, g) x' name expr' expr = h (f, g) x' (f (f (g name) expr') expr)
  pop_all :: (t -> t -> t, String -> t) -> [(Op, t)] -> t -> t
  pop_all f x expr =
    case x of
      [] -> expr
      (Op _ _ name, expr') : x' -> pop' pop_all f x' name expr' expr
  shunting_yard ::
    (Location_0 -> Location_1) -> (t -> t -> t, String -> t) -> Map' Op -> [(Op, t)] -> t -> [(Name, t)] -> Err t
  shunting_yard a f ops x expr y =
    case y of
      [] -> Right (pop_all f x expr)
      (Name l op, expr') : y' -> und_err op ops "operator" (a l) (\op' -> shunting_yard a f ops (pop f x expr op') expr' y')
  standard :: (Location_0 -> Location_1) -> String -> Err Tree_3
  standard a b = standard_0 <$> parse_tree a b
  standard_0 :: Tree_1 -> Tree_3
  standard_0 (Tree_1 a b) = Tree_3 a (standard_1 b)
  standard_1 :: Tree_0 -> Tree_2
  standard_1 (Tree_0 a b c) = Tree_2 a b (standard_defs c)
  standard_argument :: (Pat, Type_7) -> (Type_7, Expression_0) -> (Type_7, Expression_0)
  standard_argument (a, b) = bimap (standard_type b) (Function_expression_0 a)
  standard_arguments :: (Type_7, Expression_0) -> [(Pat, Type_7)] -> (Type_7, Expression_0)
  standard_arguments = Prelude.foldr standard_argument
  standard_def :: Def_0 -> Def_1
  standard_def a =
    case a of
      Basic_def_0 b c g d e f -> uncurry (Basic_def_1 b c g) (standard_arguments (e, f) d)
      Instance_def_0 b c d h f g e -> Instance_1 b c d h f g (second (uncurry standard_patterns) <$> e)
  standard_defs :: [Def_0] -> [Def_1]
  standard_defs = (<$>) standard_def
  standard_patterns :: [Pat] -> Expression_0 -> Expression_0
  standard_patterns a b = Prelude.foldr Function_expression_0 b a
  standard_type :: Type_7 -> Type_7 -> Type_7
  standard_type (Type_7 a b) (Type_7 _ c) =
    Type_7 a (Application_type_0 (Application_type_0 (Name_type_0 (Name a "Function") []) b) c)
  und_err :: String -> Map' t -> String -> Location_1 -> (t -> Err u) -> Err u
  und_err a b c d f =
    case Data.Map.lookup a b of
      Just e -> f e
      Nothing -> Left ("Undefined " ++ c ++ " " ++ a ++ location' d)
-----------------------------------------------------------------------------------------------------------------------------