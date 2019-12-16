--------------------------------------------------------------------------------------------------------------------------------
{-# OPTIONS_GHC -Wall #-}
module Typing ((!->), (!=>), Arrow_3 (..), Term_3 (..), Term_pattern_3 (..), Type (..), type_term) where
  import Data.Bifunctor
  import Data.Map
  import Dictionary
  import Errors
  import Naming
  import Transf
  infixr 0 !->
  infixr 5 !=>
  data Arrow_3 = Arrow_3 Term_pattern_3 Term_3 deriving Show
  data Term_3 =
    Add_Int_term_3 Term_3 Term_3 |
    Application_term_3 Term_3 Term_3 |
    Arrow_term_3 Arrow_3 |
    Compare_Int_term_3 Term_3 Term_3 |
    Div_term_3 Term_3 Term_3 |
    EQ_term_3 |
    GT_term_3 |
    Int_term_3 Integer |
    LT_term_3 |
    Match_term_3 Term_3 [Arrow_3] |
    Mod_term_3 Term_3 Term_3 |
    Name_term_3 String |
    Times_Int_term_3 Term_3 Term_3
      deriving Show
  data Term_pattern_3 = Int_term_pattern_3 Integer | Name_term_pattern_3 String | Struct_term_pattern_3 String deriving Show
  data Type = Arrow_type Type Type | Int_type | Ordering_type | Variable_type Integer deriving Show
  type Typechecker = Transf () Integer Err
  (!->) :: Term_pattern_3 -> Term_3 -> Term_3
  term_pattern !-> term = Arrow_term_3 (Arrow_3 term_pattern term)
  (!=>) :: Type -> Type -> Type
  (!=>) = Arrow_type
  fold_type :: (t -> t -> t) -> t -> t -> (Integer -> t) -> Type -> t
  fold_type f_Arrow f_Int f_Ordering f_variable typ =
    let
      f = fold_type f_Arrow f_Int f_Ordering f_variable
    in
      case typ of
        Arrow_type typ_0 typ_1 -> f_Arrow (f typ_0) (f typ_1)
        Int_type -> f_Int
        Ordering_type -> f_Ordering
        Variable_type type_variable -> f_variable type_variable
  new_type_variable :: Typechecker Type
  new_type_variable =
    do
      type_variable_counter <- get_state
      set_state (1 + type_variable_counter)
      return (Variable_type type_variable_counter)
-- parandada nimesid; eriti kuna pärast tulevad analoogsed funktsioonid [(Kind, Kind)] võrrandite jaoks
  replace :: Integer -> Type -> [(Type, Type)] -> Bool
  replace type_variable typ equations =
    case type_variable_occurs_in_type type_variable typ of
      False -> replace' type_variable typ equations
      True -> False
  replace' :: Integer -> Type -> [(Type, Type)] -> Bool
  replace' type_variable typ equations =
    let
      f = replace_type type_variable typ
    in
      solve_type_equations (bimap f f <$> equations)
  replace_type :: Integer -> Type -> Type -> Type
  replace_type type_variable typ =
    fold_type
      Arrow_type
      Int_type
      Ordering_type
      (\type_variable' ->
        case type_variable == type_variable' of
          False -> Variable_type type_variable'
          True -> typ)
  solve_type_equations :: [(Type, Type)] -> Bool
  solve_type_equations equations =
    case equations of
      [] -> True
      equation : equations' ->
        case equation of
          (Arrow_type typ_0 typ_1, Arrow_type typ_2 typ_3) ->
            solve_type_equations ((typ_0, typ_2) : (typ_1, typ_3) : equations')
          (Int_type, Int_type) -> solve_type_equations equations'
          (Ordering_type, Ordering_type) -> solve_type_equations equations'
          (Variable_type type_variable_0, Variable_type type_variable_1) ->
            replace' type_variable_0 (Variable_type type_variable_1) equations'
          (Variable_type type_variable, typ) -> replace type_variable typ equations'
          (typ, Variable_type type_variable) -> replace type_variable typ equations'
          _ -> False
  type_arrow :: Dictionary Type -> Type -> Type -> Arrow_2 -> Typechecker ([(Type, Type)], Arrow_3)
  type_arrow terms typ_0 typ_1 (Arrow_2 term_pattern term) =
    do
      (terms', equations_0, term_pattern') <- type_term_pattern terms typ_0 term_pattern
      (\(equations_1, term') -> (equations_0 ++ equations_1, Arrow_3 term_pattern' term')) <$> type_term' terms' typ_1 term
  type_arrows :: Dictionary Type -> Type -> Type -> [Arrow_2] -> Typechecker ([(Type, Type)], [Arrow_3])
  type_arrows terms typ_0 typ_1 arrows =
    (
      (\equations_and_arrows -> (equations_and_arrows >>= fst, snd <$> equations_and_arrows)) <$>
      traverse (type_arrow terms typ_0 typ_1) arrows)
  type_term :: Dictionary Type -> Term_2 -> Err Term_3
  type_term terms term =
    do
      (equations, term') <- run (type_term' terms (Variable_type 0) term) () 1
      case solve_type_equations equations of
        False -> Left Type_mismatch
        True -> Right term'
  type_term' :: Dictionary Type -> Type -> Term_2 -> Typechecker ([(Type, Type)], Term_3)
  type_term' terms typ term =
    case term of
      Application_term_2 term_0 term_1 ->
{-
        do 
          typ' <- new_type_variable
          (
            (\(equations_0, term_2) -> \(equations_1, term_3) ->
              (equations_0 ++ equations_1, Application_term_3 term_2 term_3)) <$>
            type_term' terms (Arrow_type typ' typ) term_0 <*>
            type_term' terms typ' term_1)
-}
        do 
          typ' <- new_type_variable
          (equations_0, term_2) <- type_term' terms (Arrow_type typ' typ) term_0 
          (equations_1, term_3) <- type_term' terms typ' term_1
          return (equations_0 ++ equations_1, Application_term_3 term_2 term_3)
      Arrow_term_2 arrow ->
{-
        do
          typ_0 <- new_type_variable
          typ_1 <- new_type_variable
          (
            (\(equations, arrow') -> ((typ, Arrow_type typ_0 typ_1) : equations, Arrow_term_3 arrow')) <$>
            type_arrow terms typ_0 typ_1 arrow)
-}
        do
          typ_0 <- new_type_variable
          typ_1 <- new_type_variable
          (equations, arrow') <- type_arrow terms typ_0 typ_1 arrow
          return ((typ, Arrow_type typ_0 typ_1) : equations, Arrow_term_3 arrow')
      Int_term_2 int -> return ([(typ, Int_type)], Int_term_3 int)
      Match_term_2 term' arrows ->
{-
        do
          typ' <- new_type_variable
          (
            (\(equations_0, term'') -> \(equations_1, arrows') -> (equations_0 ++ equations_1, Match_term_3 term'' arrows')) <$>
            type_term' terms typ' term' <*>
            type_arrows terms typ' typ arrows)
-}
        do
          typ' <- new_type_variable
          (equations_0, term'') <- type_term' terms typ' term'
          (equations_1, arrows') <- type_arrows terms typ' typ arrows
          return (equations_0 ++ equations_1, Match_term_3 term'' arrows')
      Name_term_2 name ->
        case search "constructor" name terms of
          Left err -> return_result (Left err)
          Right (name', typ') -> return ([(typ, typ')], Name_term_3 name')
  type_term_pattern ::
    Dictionary Type -> Type -> Term_pattern_2 -> Typechecker (Dictionary Type, [(Type, Type)], Term_pattern_3)
  type_term_pattern terms typ term_pattern =
    case term_pattern of
      Int_term_pattern_2 int -> return (terms, [(typ, Int_type)], Int_term_pattern_3 int)
      Name_term_pattern_2 name -> return (insert name typ terms, [], Name_term_pattern_3 name)
      Struct_term_pattern_2 name ->
        case search "constructor" name terms of
          Left err -> return_result (Left err)
          Right (name', typ') -> return (terms, [(typ, typ')], Struct_term_pattern_3 name')
  type_variable_occurs_in_type :: Integer -> Type -> Bool
  type_variable_occurs_in_type type_variable = fold_type (||) False False ((==) type_variable)
--------------------------------------------------------------------------------------------------------------------------------