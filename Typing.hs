--------------------------------------------------------------------------------------------------------------------------------
{-# OPTIONS_GHC -Wall #-}
module Typing (
  (!->),
  (!=>),
  Typing.Arrow (..),
  Polymorphic_term (..),
  Polymorphic_type (..),
  Typing.Term (..),
  Typing.Term_pattern (..),
  Typing.Test (..),
  Typing.Type (..),
  substitute_type_names_in_term,
  type_file,
  type_term,
  type_tests) where
  import Control.Monad (replicateM)
  import Control.Monad.Except (MonadError (..))
  import Control.Monad.RWS.Strict (RWST, evalRWST)
  import Control.Monad.Reader (MonadReader (..))
  import Control.Monad.State.Strict (MonadState (..), StateT, evalStateT, execStateT, modify)
  import Control.Monad.Writer.Strict (MonadWriter (..))
  import Data.Bifunctor (Bifunctor (..))
  import Data.Map ((!), Map, empty, fromList, insert, lookup, singleton, union)
  import Data.Set (Set, delete, empty, singleton, union)
  import Dictionary (Dictionary, Entry, lookup)
  import Errors (
    Err,
    Err',
    Error (..),
    Error' (..),
    L (..),
    Line_and_char,
    Type_err,
    Type_error (..),
    Type_error_location (..))
  import Naming (Arrow (..), Def_or_instance (..), File (..), Term (..), Term_pattern (..), Test (..))
  import Parser (Kind (..), Type (..))
  import Transf (SWT, evalSWT)
  data Arrow = Arrow Typing.Term_pattern Typing.Term
      deriving Show
  data Arrow' = Arrow' Typing.Term_pattern Term'
      deriving Show
  data Def_or_instance' = Def' Line_and_char String [String] Typing.Type Naming.Term
      deriving Show
  data Kind_equation = Kind_equation Kind Kind
      deriving Show
  type Kind_equation_maker = RWST (Dictionary Kind) [Kind_equation] Integer Err'
  type Kind_equation_solver = StateT [Kind_equation] Maybe
  data Polymorphic_term = Def_term [String] Typing.Term
      deriving Show
  data Polymorphic_type = Def_type [String] Typing.Type | Local_type Typing.Type
      deriving Show
  data Term =
    Add_Int_term Typing.Term Typing.Term |
    Application_term Typing.Term Typing.Term |
    Arrow_term Typing.Arrow |
    -- Compare_Int_term Typing.Term Typing.Term |
    Crash_term |
    Def_name_term String [Typing.Type] |
    Div_term Typing.Term Typing.Term |
    Int_term Integer |
    Local_name_term String |
    Match_term Typing.Term [Typing.Arrow] |
    Mod_term Typing.Term Typing.Term |
    Times_Int_term Typing.Term Typing.Term
      deriving Show
  data Term' =
    Application_term' Term' Term' |
    Arrow_term' Arrow' |
    Def_name_term' String [Integer] |
    Int_term' Integer |
    Local_name_term' String |
    Match_term' Term' [Arrow']
      deriving Show
  data Term_pattern = Blank_term_pattern | Int_term_pattern Integer | Name_term_pattern String
      deriving Show
  data Test = Test Line_and_char Typing.Term Typing.Term
      deriving Show
  data Test' = Test' Line_and_char Term' Term'
      deriving Show
  data Type = Application_type Typing.Type Typing.Type | Name_type String | Variable_type Integer
      deriving Show
  data Type_equation = Type_equation Typing.Type Typing.Type
      deriving Show
  type Type_equation_maker = SWT Integer Type_equations Err'
  type Type_equation_solver = StateT Type_equations Type_err
  data Type_equations = Type_equations (Set Integer) [Type_equation] (Map Integer Typing.Type)
      deriving Show
-- todo: make operator for application term and type?
-- todo: define synonyms for frequently used terms and types?
  infixr 0 !->
  (!->) :: Typing.Term_pattern -> Typing.Term -> Typing.Term
  term_pattern !-> term = Typing.Arrow_term (Typing.Arrow term_pattern term)
  infixr 5 !=>
  (!=>) :: Typing.Type -> Typing.Type -> Typing.Type
  typ_0 !=> typ_1 = Typing.Application_type (Typing.Application_type (Typing.Name_type "Arrow") typ_0) typ_1
  instance Monoid Type_equations where
    mempty = Type_equations Data.Set.empty [] Data.Map.empty
  instance Semigroup Type_equations where
    Type_equations variables_0 equations_0 substitutions_0 <> Type_equations variables_1 equations_1 substitutions_1 =
      Type_equations
        (Data.Set.union variables_0 variables_1)
        (equations_0 ++ equations_1)
        (Data.Map.union substitutions_0 substitutions_1)
  all_type_variables_resolved :: Type_equation_solver ()
  all_type_variables_resolved =
    do
      type_variables <- get_type_variables
      case null type_variables of
        False -> throwError Unresolved_type_variables
        True -> return ()
  check_and_substitute_variable_in_type_equations :: Integer -> Typing.Type -> Type_equation_solver ()
  check_and_substitute_variable_in_type_equations variable typ =
    case variable_occurs_in_type variable typ of
      False -> substitute_variable_in_type_equations variable typ
      True -> throwError Type_mismatch
  fold_kind :: (t -> t -> t) -> t -> t -> (Integer -> t) -> Kind -> t
  fold_kind f_arrow f_nat f_type f_variable kind =
    let
      f = fold_kind f_arrow f_nat f_type f_variable
    in
      case kind of
        Arrow_kind kind_0 kind_1 -> f_arrow (f kind_0) (f kind_1)
        Nat_kind -> f_nat
        Type_kind -> f_type
        Variable_kind kind_variable -> f_variable kind_variable
  fold_term' ::
    (
      (t -> t -> t) ->
      (Arrow' -> t) ->
      (String -> [Integer] -> t) ->
      (Integer -> t) ->
      (String -> t) ->
      (t -> [Arrow'] -> t) ->
      Term' ->
      t)
  fold_term' f_application f_arrow f_def_name f_int f_local_name f_match term =
    let
      f = fold_term' f_application f_arrow f_def_name f_int f_local_name f_match
    in
      case term of
        Application_term' term_0 term_1 -> f_application (f term_0) (f term_1)
        Arrow_term' arrow -> f_arrow arrow
        Def_name_term' name variables -> f_def_name name variables
        Int_term' i -> f_int i
        Local_name_term' name -> f_local_name name
        Match_term' term' arrows -> f_match (f term') arrows
  fold_type :: (t -> t -> t) -> (String -> t) -> (Integer -> t) -> Typing.Type -> t
  fold_type f_application f_name f_variable typ =
    let
      f = fold_type f_application f_name f_variable
    in
      case typ of
        Typing.Application_type typ_0 typ_1 -> f_application (f typ_0) (f typ_1)
        Typing.Name_type name -> f_name name
        Variable_type type_variable -> f_variable type_variable
  get_kind_equation :: Kind_equation_solver (Maybe Kind_equation)
  get_kind_equation =
    do
      equations <- get
      case equations of
        [] -> return Nothing
        equation : equations' ->
          do
            put equations'
            return (Just equation)
  get_type_equation :: Type_equation_solver (Maybe Type_equation)
  get_type_equation =
    do
      equations <- get_type_equations
      case equations of
        [] -> return Nothing
        equation : equations' ->
          do
            put_type_equations equations'
            return (Just equation)
  get_type_equations :: Type_equation_solver [Type_equation]
  get_type_equations =
    do
      Type_equations _ equations _ <- get
      return equations
  get_type_variables :: Type_equation_solver (Set Integer)
  get_type_variables =
    do
      Type_equations variables _ _ <- get
      return variables
  make_and_solve_type_equations ::
    Type_equation_maker t' -> (Error' -> err) -> (Type_error -> err) -> (Map Integer Typing.Type -> t' -> t) -> Either err t
  make_and_solve_type_equations make_type_equations transf_error' transf_type_error substitute =
    do
      (x, equations) <- first transf_error' (evalSWT make_type_equations 0)
      case execStateT solve_type_equations equations of
        Left err -> Left (transf_type_error err)
        Right (Type_equations _ _ substitutions) -> Right (substitute substitutions x)
  make_kind_equation :: Kind -> Kind -> Kind_equation_maker ()
  make_kind_equation kind_0 kind_1 = tell [Kind_equation kind_0 kind_1]
  make_kind_equations :: Kind -> Parser.Type -> Kind_equation_maker Typing.Type
  make_kind_equations kind typ =
    case typ of
      Parser.Application_type typ_0 typ_1 ->
        do
          kind' <- new_kind_variable
          typ_0' <- make_kind_equations (Arrow_kind kind' kind) typ_0
          typ_1' <- make_kind_equations kind' typ_1
          return (Typing.Application_type typ_0' typ_1')
      Parser.Name_type name ->
        do
          type_kinds <- ask
-- todo: make dictionary lookup throw exceptions under any monad with error type Err'?
          case Dictionary.lookup "type" name type_kinds of
            Left err -> throwError err
            Right (name', kind') ->
              do
                make_kind_equation kind kind'
                return (Typing.Name_type name')
  make_type_equation :: Typing.Type -> Typing.Type -> Type_equation_maker ()
  make_type_equation typ_0 typ_1 = tell (Type_equations Data.Set.empty [Type_equation typ_0 typ_1] Data.Map.empty)
  make_type_equations_arrow ::
    Dictionary Polymorphic_type -> Typing.Type -> Typing.Type -> Naming.Arrow -> Type_equation_maker Arrow'
  make_type_equations_arrow term_types typ_0 typ_1 (Naming.Arrow term_pattern term) =
    do
      (term_types', term_pattern') <- make_type_equations_term_pattern term_types typ_0 term_pattern
      term' <- make_type_equations_term term_types' typ_1 term
      return (Arrow' term_pattern' term')
  make_type_equations_term :: Dictionary Polymorphic_type -> Typing.Type -> Naming.Term -> Type_equation_maker Term'
  make_type_equations_term term_types typ term =
    case term of
      Naming.Application_term term_0 term_1 ->
        do
          typ' <- new_type_variable
          term_0' <- make_type_equations_term term_types (typ' !=> typ) term_0
          term_1' <- make_type_equations_term term_types typ' term_1
          return (Application_term' term_0' term_1')
      Naming.Arrow_term arrow ->
        do
          typ_0 <- new_type_variable
          typ_1 <- new_type_variable
          make_type_equation typ (typ_0 !=> typ_1)
          arrow' <- make_type_equations_arrow term_types typ_0 typ_1 arrow
          return (Arrow_term' arrow')
      Naming.Int_term i ->
        do
          make_type_equation typ (Typing.Name_type "Int")
          return (Int_term' i)
      Naming.Match_term term' arrows ->
        do
          typ' <- new_type_variable
          term'' <- make_type_equations_term term_types typ' term'
          arrows' <- traverse (make_type_equations_arrow term_types typ' typ) arrows
          return (Match_term' term'' arrows')
      Naming.Name_term name ->
        case Dictionary.lookup "term" name term_types of
          Left err -> throwError err
          Right (name', polymorphic_type) ->
            case polymorphic_type of
              Def_type type_variables typ' ->
                do
                  types <- replicateM (length type_variables) new_type_variable
                  make_type_equation typ (substitute_names_in_type (fromList (zip type_variables types)) typ')
                  return (Def_name_term' name' ((\(Variable_type variable) -> variable) <$> types))
              Local_type typ' ->
                do
                  make_type_equation typ typ'
                  return (Local_name_term' name')
  make_type_equations_term_pattern ::
    (
      Dictionary Polymorphic_type ->
      Typing.Type ->
      Naming.Term_pattern ->
      Type_equation_maker (Dictionary Polymorphic_type, Typing.Term_pattern))
  make_type_equations_term_pattern term_types typ term_pattern =
    case term_pattern of
      Naming.Blank_term_pattern -> return (term_types, Typing.Blank_term_pattern)
      Naming.Int_term_pattern i ->
        do
          make_type_equation typ (Typing.Name_type "Int")
          return (term_types, Typing.Int_term_pattern i)
      Naming.Name_term_pattern name -> return (insert name (Local_type typ) term_types, Typing.Name_term_pattern name)
{-
      Naming.Struct_term_pattern name ->
        case search "constructor" name terms of
          Left err -> return_result (Left err)
          Right (name', typ') -> return (terms, [(typ, typ')], Typing.Struct_term_pattern name')
-}
  make_type_equations_test :: Dictionary Polymorphic_type -> Typing.Type -> Naming.Test -> Type_equation_maker Test'
  make_type_equations_test term_types typ (Naming.Test line_and_char term_0 term_1) =
    Test' line_and_char <$> make_type_equations_term term_types typ term_0 <*> make_type_equations_term term_types typ term_1
  new_kind_variable :: Kind_equation_maker Kind
  new_kind_variable =
    do
      variable_counter <- get
      put (1 + variable_counter)
      return (Variable_kind variable_counter)
  new_type_variable :: Type_equation_maker Typing.Type
  new_type_variable =
    do
      variable_counter <- get
      let typ = Variable_type variable_counter
      put (1 + variable_counter)
      tell (Type_equations (Data.Set.singleton variable_counter) [] (Data.Map.singleton variable_counter typ))
      return typ
  put_type_equations :: [Type_equation] -> Type_equation_solver ()
  put_type_equations equations =
    do
      Type_equations variables _ substitutions <- get
      put (Type_equations variables equations substitutions)
  solve_kind_equation :: Kind -> Kind -> Kind_equation_solver ()
  solve_kind_equation kind_0 kind_1 =
    do
      equations <- get
      put (Kind_equation kind_0 kind_1 : equations)
  solve_kind_equations :: Kind_equation_solver ()
  solve_kind_equations =
    do
      maybe_equation <- get_kind_equation
      case maybe_equation of
        Nothing -> return ()
        Just equation ->
          do
            case equation of
              Kind_equation (Arrow_kind kind_0 kind_1) (Arrow_kind kind_2 kind_3) ->
                do
                  solve_kind_equation kind_0 kind_2
                  solve_kind_equation kind_1 kind_3
              Kind_equation Nat_kind Nat_kind -> return ()
              Kind_equation Type_kind Type_kind -> return ()
              Kind_equation (Variable_kind variable) kind -> substitute_variable_in_kind_equations variable kind
              Kind_equation kind (Variable_kind variable) -> substitute_variable_in_kind_equations variable kind
              _ -> throwError ()
            solve_kind_equations
  solve_type_equation :: Typing.Type -> Typing.Type -> Type_equation_solver ()
  solve_type_equation typ_0 typ_1 =
    do
      Type_equations variables equations substitutions <- get
      put (Type_equations variables (Type_equation typ_0 typ_1 : equations) substitutions)
  solve_type_equations :: Type_equation_solver ()
  solve_type_equations =
    do
      maybe_equation <- get_type_equation
      case maybe_equation of
        Nothing -> all_type_variables_resolved
        Just equation ->
          do
            case equation of
              Type_equation (Typing.Application_type typ_0 typ_1) (Typing.Application_type typ_2 typ_3) ->
                do
                  solve_type_equation typ_0 typ_2
                  solve_type_equation typ_1 typ_3
              Type_equation (Typing.Name_type name_0) (Typing.Name_type name_1) ->
                case name_0 == name_1 of
                  False -> throwError Type_mismatch
                  True -> return ()
              Type_equation (Variable_type variable_0) (Variable_type variable_1) ->
                substitute_variable_in_type_equations variable_0 (Variable_type variable_1)
              Type_equation (Variable_type variable) typ -> check_and_substitute_variable_in_type_equations variable typ
              Type_equation typ (Variable_type variable) -> check_and_substitute_variable_in_type_equations variable typ
              _ -> throwError Type_mismatch
            solve_type_equations
  substitute_names_in_type :: Dictionary Typing.Type -> Typing.Type -> Typing.Type
  substitute_names_in_type substitutions =
    fold_type
      Typing.Application_type
      (\name ->
        case Data.Map.lookup name substitutions of
          Nothing -> Typing.Name_type name
          Just typ -> typ)
      undefined
  substitute_type_names_in_arrow :: Dictionary Typing.Type -> Typing.Arrow -> Typing.Arrow
  substitute_type_names_in_arrow substitutions (Typing.Arrow term_pattern term) =
    Typing.Arrow term_pattern (substitute_type_names_in_term substitutions term)
  substitute_type_names_in_term :: Dictionary Typing.Type -> Typing.Term -> Typing.Term
  substitute_type_names_in_term substitutions term =
    let
      f_arrow = substitute_type_names_in_arrow substitutions
      f_term = substitute_type_names_in_term substitutions
    in
      case term of
        Add_Int_term term_0 term_1 -> Add_Int_term (f_term term_0) (f_term term_1)
        Typing.Application_term term_0 term_1 -> Typing.Application_term (f_term term_0) (f_term term_1)
        Typing.Arrow_term arrow -> Typing.Arrow_term (f_arrow arrow)
        Crash_term -> Crash_term
        Def_name_term name types -> Def_name_term name (substitute_names_in_type substitutions <$> types)
        Div_term term_0 term_1 -> Div_term (f_term term_0) (f_term term_1)
        Typing.Int_term i -> Typing.Int_term i
        Local_name_term name -> Local_name_term name
        Typing.Match_term term' arrows -> Typing.Match_term (f_term term') (f_arrow <$> arrows)
        Mod_term term_0 term_1 -> Mod_term (f_term term_0) (f_term term_1)
        Times_Int_term term_0 term_1 -> Times_Int_term (f_term term_0) (f_term term_1)
  substitute_type_variables_in_arrow :: Map Integer Typing.Type -> Arrow' -> Typing.Arrow
  substitute_type_variables_in_arrow substitutions (Arrow' term_pattern term) =
    Typing.Arrow term_pattern (substitute_type_variables_in_term substitutions term)
  substitute_type_variables_in_term :: Map Integer Typing.Type -> Term' -> Typing.Term
  substitute_type_variables_in_term substitutions =
    let
      f = substitute_type_variables_in_arrow substitutions
    in
      fold_term'
        Typing.Application_term
        (\arrow -> Typing.Arrow_term (f arrow))
        (\name -> \variables -> Typing.Def_name_term name ((!) substitutions <$> variables))
        Typing.Int_term
        Typing.Local_name_term
        (\term -> \arrows -> Typing.Match_term term (f <$> arrows))
  substitute_type_variables_in_test :: Map Integer Typing.Type -> Test' -> Typing.Test
  substitute_type_variables_in_test substitutions (Test' line_and_char term_0 term_1) =
    let
      f = substitute_type_variables_in_term substitutions
    in
      Typing.Test line_and_char (f term_0) (f term_1)
  substitute_variable_in_kind :: Integer -> Kind -> Kind -> Kind
  substitute_variable_in_kind variable kind =
    fold_kind
      Arrow_kind
      Nat_kind
      Type_kind
      (\variable' ->
        case variable == variable' of
          False -> Variable_kind variable'
          True -> kind)
  substitute_variable_in_kind_equation :: Integer -> Kind -> Kind_equation -> Kind_equation
  substitute_variable_in_kind_equation variable kind (Kind_equation kind_0 kind_1) =
    Kind_equation (substitute_variable_in_kind variable kind kind_0) kind_1
  substitute_variable_in_kind_equations :: Integer -> Kind -> Kind_equation_solver ()
  substitute_variable_in_kind_equations variable kind = modify (fmap (substitute_variable_in_kind_equation variable kind))
  substitute_variable_in_type :: Integer -> Typing.Type -> Typing.Type -> Typing.Type
  substitute_variable_in_type variable typ =
    fold_type
    Typing.Application_type
    Typing.Name_type
    (\variable' ->
      case variable == variable' of
        False -> Variable_type variable'
        True -> typ)
  substitute_variable_in_type_equation :: Integer -> Typing.Type -> Type_equation -> Type_equation
  substitute_variable_in_type_equation variable typ (Type_equation typ_0 typ_1) =
    let
      f = substitute_variable_in_type variable typ
    in
      Type_equation (f typ_0) (f typ_1)
  substitute_variable_in_type_equations :: Integer -> Typing.Type -> Type_equation_solver ()
  substitute_variable_in_type_equations variable typ =
    do
      Type_equations variables equations substitutions <- get
      put
        (Type_equations
          (delete variable variables)
          (substitute_variable_in_type_equation variable typ <$> equations)
          (substitute_variable_in_type variable typ <$> substitutions))
  type_def_or_instance_0 :: Dictionary Kind -> Def_or_instance -> Err' (Entry Polymorphic_type, Def_or_instance')
  type_def_or_instance_0 type_kinds def_or_instance =
    case def_or_instance of
      Def line_and_char name type_variables typ term ->
        do
          let type_variables' = fst <$> type_variables
          typ' <- type_type (Data.Map.union type_kinds (fromList type_variables)) typ
          Right ((name, Def_type type_variables' typ'), Def' line_and_char name type_variables' typ' term)
  type_def_or_instance_1 :: Dictionary Polymorphic_type -> Def_or_instance' -> Err' (Entry Polymorphic_term)
  type_def_or_instance_1 term_types def_or_instance =
    case def_or_instance of
      Def' line_and_char name type_variables typ term ->
        do
          term' <- type_term' term_types (Definition name) line_and_char typ term
          Right (name, Def_term type_variables term')
  type_defs_and_instances ::
    (
      Dictionary Kind ->
      Dictionary Polymorphic_type ->
      [Def_or_instance] ->
      Err' (Dictionary Polymorphic_type, Dictionary Polymorphic_term))
  type_defs_and_instances type_kinds term_types defs_and_instances =
    do
      defs_and_instances' <- traverse (type_def_or_instance_0 type_kinds) defs_and_instances
      let term_types' = fromList (fst <$> defs_and_instances')
      term_defs <- traverse (type_def_or_instance_1 (Data.Map.union term_types term_types')) (snd <$> defs_and_instances')
      Right (term_types', fromList term_defs)
  type_file ::
    Dictionary Kind -> Dictionary Polymorphic_type -> File -> Err' (Dictionary Polymorphic_type, Dictionary Polymorphic_term)
  type_file type_kinds term_types (File defs_and_instances) = type_defs_and_instances type_kinds term_types defs_and_instances 
  type_term :: Dictionary Polymorphic_type -> Naming.Term -> Err Typing.Term
  type_term term_types term =
    make_and_solve_type_equations
      (do
        typ <- new_type_variable
        make_type_equations_term term_types typ term)
      (Error "input")
      Type_error_in_input
      substitute_type_variables_in_term
  type_term' ::
    Dictionary Polymorphic_type -> Type_error_location -> Line_and_char -> Typing.Type -> Naming.Term -> Err' Typing.Term
  type_term' term_types location line_and_char typ term =
    make_and_solve_type_equations
      (make_type_equations_term term_types typ term)
      id
      (Type_error location line_and_char)
      substitute_type_variables_in_term
  type_test :: Dictionary Polymorphic_type -> Naming.Test -> Err' Typing.Test
  type_test term_types (Naming.Test line_and_char term_0 term_1) =
    make_and_solve_type_equations
      (do
        typ <- new_type_variable
        make_type_equations_test term_types typ (Naming.Test line_and_char term_0 term_1))
      id
      (Type_error Errors.Test line_and_char)
      substitute_type_variables_in_test
  type_tests :: Dictionary Polymorphic_type -> [Naming.Test] -> Err' [Typing.Test]
  type_tests term_types = traverse (type_test term_types)
  type_type :: Dictionary Kind -> L Parser.Type -> Err' Typing.Type
  type_type type_kinds (L line_and_char typ) =
    do
      (typ', equations) <- evalRWST (make_kind_equations Type_kind typ) type_kinds 0
      case evalStateT solve_kind_equations equations of
        Nothing -> Left (Kind_mismatch line_and_char)
        Just () -> Right typ'
  variable_occurs_in_type :: Integer -> Typing.Type -> Bool
  variable_occurs_in_type type_variable = fold_type (||) (\_ -> False) ((==) type_variable)
--------------------------------------------------------------------------------------------------------------------------------