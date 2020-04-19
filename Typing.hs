--------------------------------------------------------------------------------------------------------------------------------
{-# OPTIONS_GHC -Wall #-}
module Typing (
  (!->),
  (!=>),
  Typing.Arrow (..),
  Typing.Term (..),
  Typing.Term_pattern (..),
  Typing.Test (..),
  Typing.Type (..),
  type_file,
  type_term,
  type_tests) where
  import Control.Monad.Except (MonadError (..))
  import Control.Monad.State.Strict (MonadState (..))
  import Control.Monad.Trans.State.Strict (StateT, evalStateT)
  import Control.Monad.Writer.Strict (MonadWriter (..))
  import Data.Bifunctor (bimap)
  import Data.Map (fromList, insert, union)
  import Data.Set (Set, empty, delete, singleton, union)
  import Dictionary (Dictionary, Entry, lookup)
  import Errors (
    Err,
    Err',
    Error (..),
    Error' (..),
    Line_and_char,
    Type_err,
    Type_error (..),
    Type_error_location (..),
    add_file_name)
  import Naming (Arrow (..), Def_or_instance (..), File (..), Term (..), Term_pattern (..), Test (..))
  import Parser (Type (..))
  import Transf (SWT, evalSWT)
  data Arrow = Arrow Typing.Term_pattern Typing.Term
      deriving Show
  data Def_or_instance' = Def' Line_and_char String Typing.Type Naming.Term
      deriving Show
  data Term =
    Add_Int_term Typing.Term Typing.Term |
    Application_term Typing.Term Typing.Term |
    Arrow_term Typing.Arrow |
    -- Compare_Int_term Typing.Term Typing.Term |
    Crash_term |
    Div_term Typing.Term Typing.Term |
    -- EQ_term |
    -- GT_term |
    Int_term Integer |
    -- LT_term |
    Match_term Typing.Term [Typing.Arrow] |
    Mod_term Typing.Term Typing.Term |
    Name_term String |
    Times_Int_term Typing.Term Typing.Term
      deriving Show
  data Term_pattern = Blank_term_pattern | Int_term_pattern Integer | Name_term_pattern String
      deriving Show
  data Test = Test Line_and_char Typing.Term Typing.Term
      deriving Show
  data Type = Arrow_type Typing.Type Typing.Type | Int_type | Variable_type Integer
      deriving Show
  data Type_equation = Type_equation Typing.Type Typing.Type
      deriving Show
  type Type_equation_maker = SWT Integer Type_equations Err'
  type Type_equation_solver = StateT Type_equations Type_err
  data Type_equations = Type_equations (Set Integer) [Type_equation]
      deriving Show
  infixr 0 !->
  (!->) :: Typing.Term_pattern -> Typing.Term -> Typing.Term
  term_pattern !-> term = Typing.Arrow_term (Typing.Arrow term_pattern term)
  infixr 5 !=>
  (!=>) :: Typing.Type -> Typing.Type -> Typing.Type
  (!=>) = Typing.Arrow_type
  instance Monoid Type_equations where
    mempty = Type_equations empty []
  instance Semigroup Type_equations where
    Type_equations type_variables_0 equations_0 <> Type_equations type_variables_1 equations_1 =
      Type_equations (Data.Set.union type_variables_0 type_variables_1) (equations_0 ++ equations_1)
  add_type_equation :: Typing.Type -> Typing.Type -> Type_equation_solver ()
  add_type_equation typ_0 typ_1 =
    do
      Type_equations type_variables equations <- get
      put (Type_equations type_variables (Type_equation typ_0 typ_1 : equations))
  delete_type_variable :: Integer -> Type_equation_solver ()
  delete_type_variable type_variable =
    do
      Type_equations type_variables equations <- get
      put (Type_equations (delete type_variable type_variables) equations)
  fold_type :: (t -> t -> t) -> t -> (Integer -> t) -> Typing.Type -> t
  fold_type f_Arrow f_Int f_variable typ =
    let
      f = fold_type f_Arrow f_Int f_variable
    in
      case typ of
        Typing.Arrow_type typ_0 typ_1 -> f_Arrow (f typ_0) (f typ_1)
        Typing.Int_type -> f_Int
        Variable_type type_variable -> f_variable type_variable
  get_type_equations :: Type_equation_solver [Type_equation]
  get_type_equations =
    do
      Type_equations _ equations <- get
      return equations
  get_type_variables :: Type_equation_solver (Set Integer)
  get_type_variables =
    do
      Type_equations type_variables _ <- get
      return type_variables
  increment_type_variable_counter :: Type_equation_maker ()
  increment_type_variable_counter =
    do
      type_variable_counter <- get
      put (1 + type_variable_counter)
  make_type_equations_arrow ::
    Dictionary Typing.Type -> Typing.Type -> Typing.Type -> Naming.Arrow -> Type_equation_maker Typing.Arrow
  make_type_equations_arrow term_types typ_0 typ_1 (Naming.Arrow term_pattern term) =
    do
      (term_types', term_pattern') <- make_type_equations_term_pattern term_types typ_0 term_pattern
      term' <- make_type_equations_term term_types' typ_1 term
      return (Typing.Arrow term_pattern' term')
  make_type_equations_term :: Dictionary Typing.Type -> Typing.Type -> Naming.Term -> Type_equation_maker Typing.Term
  make_type_equations_term term_types typ term =
    case term of
      Naming.Application_term term_0 term_1 ->
        do
          typ' <- new_type_variable
          term_0' <- make_type_equations_term term_types (typ' !=> typ) term_0
          term_1' <- make_type_equations_term term_types typ' term_1
          return (Typing.Application_term term_0' term_1')
      Naming.Arrow_term arrow ->
        do
          typ_0 <- new_type_variable
          typ_1 <- new_type_variable
          tell_type_equation typ (typ_0 !=> typ_1)
          arrow' <- make_type_equations_arrow term_types typ_0 typ_1 arrow
          return (Typing.Arrow_term arrow')
      Naming.Int_term i ->
        do
          tell_type_equation typ Typing.Int_type
          return (Typing.Int_term i)
      Naming.Match_term term' arrows ->
        do
          typ' <- new_type_variable
          term'' <- make_type_equations_term term_types typ' term'
          arrows' <- traverse (make_type_equations_arrow term_types typ' typ) arrows
          return (Typing.Match_term term'' arrows')
      Naming.Name_term name ->
        case Dictionary.lookup "term" name term_types of
          Left err -> throwError err
          Right (name', typ') ->
            do
              tell_type_equation typ typ'
              return (Typing.Name_term name')
  make_type_equations_term_pattern ::
    (
      Dictionary Typing.Type ->
      Typing.Type ->
      Naming.Term_pattern ->
      Type_equation_maker (Dictionary Typing.Type, Typing.Term_pattern))
  make_type_equations_term_pattern term_types typ term_pattern =
    case term_pattern of
      Naming.Blank_term_pattern -> return (term_types, Typing.Blank_term_pattern)
      Naming.Int_term_pattern i ->
        do
          tell_type_equation typ Typing.Int_type
          return (term_types, Typing.Int_term_pattern i)
      Naming.Name_term_pattern name -> return (insert name typ term_types, Typing.Name_term_pattern name)
{-
      Naming.Struct_term_pattern name ->
        case search "constructor" name terms of
          Left err -> return_result (Left err)
          Right (name', typ') -> return (terms, [(typ, typ')], Typing.Struct_term_pattern name')
-}
  make_type_equations_test :: Dictionary Typing.Type -> Typing.Type -> Naming.Test -> Type_equation_maker Typing.Test
  make_type_equations_test term_types typ (Naming.Test line_and_char term_0 term_1) =
    (
      Typing.Test line_and_char <$>
      make_type_equations_term term_types typ term_0 <*>
      make_type_equations_term term_types typ term_1)
  new_type_variable :: Type_equation_maker Typing.Type
  new_type_variable =
    do
      type_variable_counter <- get
      increment_type_variable_counter
      tell_new_type_variable type_variable_counter
      return (Variable_type type_variable_counter)
  put_type_equations :: [Type_equation] -> Type_equation_solver ()
  put_type_equations equations =
    do
      Type_equations type_variables _ <- get
      put (Type_equations type_variables equations)
  replace_type_variable_in_equation :: Integer -> Typing.Type -> Type_equation -> Type_equation
  replace_type_variable_in_equation type_variable typ (Type_equation typ_0 typ_1) =
    let
      f = replace_type_variable_in_type type_variable typ
    in
      Type_equation (f typ_0) (f typ_1)
  replace_type_variable_in_equations :: Integer -> Typing.Type -> Type_equation_solver ()
  replace_type_variable_in_equations type_variable typ =
    do
      Type_equations type_variables equations <- get
      put (Type_equations type_variables (replace_type_variable_in_equation type_variable typ <$> equations))
  replace_type_variable_in_type :: Integer -> Typing.Type -> Typing.Type -> Typing.Type
  replace_type_variable_in_type type_variable typ =
    fold_type
      Typing.Arrow_type
      Typing.Int_type
      (\type_variable' ->
        case type_variable == type_variable' of
          False -> Variable_type type_variable'
          True -> typ)
  solve_type_equations :: Type_equation_solver ()
  solve_type_equations =
    do
      equations <- get_type_equations
      case equations of
        [] ->
          do
            type_variables <- get_type_variables
            case null type_variables of
              False -> throwError Unresolved_type_variables
              True -> return ()
        equation : equations' ->
          do
            put_type_equations equations'
            case equation of
              Type_equation (Typing.Arrow_type typ_0 typ_1) (Typing.Arrow_type typ_2 typ_3) ->
                do
                  add_type_equation typ_0 typ_2
                  add_type_equation typ_1 typ_3
              Type_equation Typing.Int_type Typing.Int_type -> return ()
              Type_equation (Variable_type type_variable_0) (Variable_type type_variable_1) ->
                substitute_type' type_variable_0 (Variable_type type_variable_1)
              Type_equation (Variable_type type_variable) typ -> substitute_type type_variable typ
              Type_equation typ (Variable_type type_variable) -> substitute_type type_variable typ
              _ -> throwError Type_mismatch
            solve_type_equations
  substitute_type :: Integer -> Typing.Type -> Type_equation_solver ()
  substitute_type type_variable typ =
    case variable_occurs_in_type type_variable typ of
      False -> substitute_type' type_variable typ
      True -> throwError Type_mismatch
  substitute_type' :: Integer -> Typing.Type -> Type_equation_solver ()
  substitute_type' type_variable typ =
    do
      delete_type_variable type_variable
      replace_type_variable_in_equations type_variable typ
  tell_new_type_variable :: Integer -> Type_equation_maker ()
  tell_new_type_variable type_variable = tell (Type_equations (singleton type_variable) [])
  tell_type_equation :: Typing.Type -> Typing.Type -> Type_equation_maker ()
  tell_type_equation type_0 type_1 = tell (Type_equations empty [Type_equation type_0 type_1])
  type_def_or_instance_0 :: Def_or_instance -> (Entry Typing.Type, Def_or_instance')
  type_def_or_instance_0 def_or_instance =
    case def_or_instance of
      Def line_and_char name typ term ->
        let
          typ' = type_type typ
        in
          ((name, typ'), Def' line_and_char name typ' term)
  type_def_or_instance_1 :: Dictionary Typing.Type -> Def_or_instance' -> Err' (Entry Typing.Term)
  type_def_or_instance_1 term_types def_or_instance =
    case def_or_instance of
      Def' line_and_char name typ term ->
        ((,) name) <$> type_term' term_types line_and_char (Definition name) typ term
  type_defs_and_instances ::
    Dictionary Typing.Type -> [Def_or_instance] -> Err' (Dictionary Typing.Type, Dictionary Typing.Term)
  type_defs_and_instances term_types defs_and_instances =
    do
      let defs_and_instances' = type_def_or_instance_0 <$> defs_and_instances
      let term_types' = fromList (fst <$> defs_and_instances')
      term_defs <- traverse (type_def_or_instance_1 (Data.Map.union term_types term_types')) (snd <$> defs_and_instances')
      Right (term_types', fromList term_defs)
  type_file :: Dictionary Typing.Type -> File -> Err' (Dictionary Typing.Type, Dictionary Typing.Term)
  type_file term_types (File defs_and_instances) = type_defs_and_instances term_types defs_and_instances
  type_term :: Dictionary Typing.Type -> Naming.Term -> Err Typing.Term
  type_term term_types term =
    do
      (term', equations) <-
        add_file_name
          "input"
          (evalSWT
            (do
              typ <- new_type_variable
              make_type_equations_term term_types typ term)
            0)
      bimap Type_error_in_input (\() -> term') (evalStateT solve_type_equations equations)
  type_term' :: Dictionary Typing.Type -> Line_and_char -> Type_error_location -> Typing.Type -> Naming.Term -> Err' Typing.Term
  type_term' term_types line_and_char location typ term =
    do
      (term', equations) <- evalSWT (make_type_equations_term term_types typ term) 0
      bimap (Type_error location line_and_char) (\() -> term') (evalStateT solve_type_equations equations)
  type_test :: Dictionary Typing.Type -> Naming.Test -> Err' Typing.Test
  type_test term_types (Naming.Test line_and_char term_0 term_1) =
    do
      (test', equations) <-
        evalSWT
          (do
            typ <- new_type_variable
            make_type_equations_test term_types typ (Naming.Test line_and_char term_0 term_1))
          0
      bimap (Type_error Errors.Test line_and_char) (\() -> test') (evalStateT solve_type_equations equations)
  type_tests :: Dictionary Typing.Type -> [Naming.Test] -> Err' [Typing.Test]
  type_tests term_types = traverse (type_test term_types)
  type_type :: Parser.Type -> Typing.Type
  type_type typ =
    case typ of
      Parser.Arrow_type typ_0 typ_1 -> Typing.Arrow_type (type_type typ_0) (type_type typ_1)
      Parser.Int_type -> Typing.Int_type
  variable_occurs_in_type :: Integer -> Typing.Type -> Bool
  variable_occurs_in_type type_variable = fold_type (||) False ((==) type_variable)
--------------------------------------------------------------------------------------------------------------------------------