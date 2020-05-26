--------------------------------------------------------------------------------------------------------------------------------
{-# OPTIONS_GHC -Wall #-}
module Naming (
  Naming.Arrow (..),
  Naming.Def_or_instance (..),
  Naming.File (..),
  Naming.Term (..),
  Naming.Term_pattern (..),
  Naming.Test (..),
  Naming.Type_variable,
  naming_file,
  naming_term,
  naming_tests) where
  import Control.Monad.Except (MonadError (..))
  import Control.Monad.Reader (MonadReader (..))
  import Control.Monad.State.Strict (MonadState (..))
  import Control.Monad.Trans.Reader (ReaderT, runReaderT)
  import Dictionary (Dictionary, insert)
  import Errors (Err, Error (..), L (..), Language_or_location (..), Line_and_char)
  import Parser (
    Arrow (..),
    Code_file (..),
    Def_or_instance (..),
    Kind,
    Term (..),
    Term_pattern (..),
    Test (..),
    Type,
    Type_variable)
  import Transf (RST, runRST)
  data Arrow = Arrow Naming.Term_pattern Naming.Term
      deriving Show
  data Def_or_instance = Def Line_and_char String [Naming.Type_variable] (L Type) Naming.Term
      deriving Show
  data Def_or_instance' = Def' Line_and_char String [Parser.Type_variable] (L Type) Parser.Term
      deriving Show
  data File = File [Naming.Def_or_instance]
      deriving Show
  data File' = File' [Def_or_instance']
      deriving Show
  type Naming_0 = RST String (Dictionary Language_or_location) Err
  type Naming_1 = ReaderT String Err
  data Term =
    Application_term Naming.Term Naming.Term |
    Arrow_term Naming.Arrow |
    Int_term Integer |
    Match_term Naming.Term [Naming.Arrow] |
    Name_term (L String)
      deriving Show
  data Term_pattern = Blank_term_pattern | Int_term_pattern Integer | Name_term_pattern String
      deriving Show
  data Test = Test Line_and_char Naming.Term Naming.Term
      deriving Show
  type Type_variable = (String, Kind)
  naming_arrow :: Dictionary Language_or_location -> Parser.Arrow -> Naming_1 Naming.Arrow
  naming_arrow terms (Parser.Arrow term_pattern term) =
    do
      (terms', term_pattern') <- naming_term_pattern terms term_pattern
      Naming.Arrow term_pattern' <$> naming_term' terms' term
  naming_def_or_instance_0 :: Parser.Def_or_instance -> Naming_0 Def_or_instance'
  naming_def_or_instance_0 def_or_instance =
    case def_or_instance of
      Parser.Def line_and_char name type_variables typ term ->
        do
          name' <- naming_term_name (L line_and_char name)
          return (Def' line_and_char name' type_variables typ term)
  naming_def_or_instance_1 ::
    (Dictionary Language_or_location, Dictionary Language_or_location) -> Def_or_instance' -> Naming_1 Naming.Def_or_instance
  naming_def_or_instance_1 (types, terms) def_or_instance =
    case def_or_instance of
      Def' line_and_char name type_variables typ term ->
        do
          (_, type_variables') <- naming_type_variables types type_variables
          term' <- naming_term' terms term
          return (Naming.Def line_and_char name type_variables' typ term')
  naming_file ::
    (
      String ->
      (Dictionary Language_or_location, Dictionary Language_or_location) ->
      Parser.Code_file ->
      Err (Dictionary Language_or_location, Naming.File))
  naming_file file_name (types, terms) file =
    do
      (file', terms') <- runRST (naming_file_0 file) file_name terms
      file'' <- runReaderT (naming_file_1 (types, terms') file') file_name
      return (terms', file'')
  naming_file_0 :: Parser.Code_file -> Naming_0 Naming.File'
  naming_file_0 (Parser.Code_file defs_and_instances) = Naming.File' <$> traverse naming_def_or_instance_0 defs_and_instances
  naming_file_1 :: (Dictionary Language_or_location, Dictionary Language_or_location) -> Naming.File' -> Naming_1 Naming.File
  naming_file_1 (types, terms) (Naming.File' defs_and_instances) =
    Naming.File <$> traverse (naming_def_or_instance_1 (types, terms)) defs_and_instances
  naming_term :: Dictionary Language_or_location -> Parser.Term -> Err Naming.Term
  naming_term terms term = runReaderT (naming_term' terms term) "input"
  naming_term' :: Dictionary Language_or_location -> Parser.Term -> Naming_1 Naming.Term
  naming_term' terms term =
    case term of
      Parser.Application_term term_0 term_1 ->
        Naming.Application_term <$> naming_term' terms term_0 <*> naming_term' terms term_1
      Parser.Arrow_term arrow -> Naming.Arrow_term <$> naming_arrow terms arrow
      Parser.Int_term int -> return (Naming.Int_term int)
      Parser.Match_term term' arrows -> Naming.Match_term <$> naming_term' terms term' <*> traverse (naming_arrow terms) arrows
      Parser.Name_term name -> return (Naming.Name_term name)
  naming_term_name :: L String -> Naming_0 String
  naming_term_name (L line_and_char name) =
    do
      file_name <- ask
      terms <- get
-- TODO: kirjutada sellesse moodulisse monaadiline insert?
      case Dictionary.insert name (Location file_name line_and_char) terms of
        Left language_or_location ->
          throwError (Conflicting_definitions "term" name language_or_location file_name line_and_char)
        Right terms' ->
          do
            put terms'
            return name
  naming_term_pattern ::
    Dictionary Language_or_location -> L Parser.Term_pattern -> Naming_1 (Dictionary Language_or_location, Naming.Term_pattern)
  naming_term_pattern terms (L _ term_pattern) =
    case term_pattern of
      Parser.Blank_term_pattern -> return (terms, Naming.Blank_term_pattern)
      Parser.Int_term_pattern i -> return (terms, Naming.Int_term_pattern i)
      Parser.Name_term_pattern (L line_and_char name) ->
{-
        do
          (file_name, constructors) <- read_context
          case member name constructors of
            False ->
              case Data.Map.lookup name terms of
                Nothing -> return (Data.Map.insert name (Location file_name line_and_char) terms, Naming.Name_term_pattern name)
                Just language_or_location ->
                  return_result (Left (Conflicting_definitions_of "term" name language_or_location file_name line_and_char))
            True -> return (terms, Struct_term_pattern (Name line_and_char name))
-}
        do
          file_name <- ask
          case Dictionary.insert name (Location file_name line_and_char) terms of
            Left language_or_location ->
              throwError (Conflicting_definitions "term" name language_or_location file_name line_and_char)
            Right terms' -> return (terms', Naming.Name_term_pattern name)
  naming_test :: Dictionary Language_or_location -> Parser.Test -> Naming_1 Naming.Test
  naming_test terms (Parser.Test line_and_char term_0 term_1) =
    Naming.Test line_and_char <$> naming_term' terms term_0 <*> naming_term' terms term_1
  naming_tests :: String -> Dictionary Language_or_location -> [Parser.Test] -> Err [Naming.Test]
  naming_tests file_name terms tests = runReaderT (traverse (naming_test terms) tests) file_name
  naming_type_name_1 :: Dictionary Language_or_location -> L String -> Naming_1 (Dictionary Language_or_location, String)
  naming_type_name_1 types (L line_and_char name) =
    do
      file_name <- ask
      case Dictionary.insert name (Location file_name line_and_char) types of
        Left language_or_location ->
          throwError (Conflicting_definitions "type" name language_or_location file_name line_and_char)
        Right types' -> return (types', name)
  naming_type_variable ::
    Dictionary Language_or_location -> Parser.Type_variable -> Naming_1 (Dictionary Language_or_location, Naming.Type_variable)
  naming_type_variable types (name, kind) =
    do
      (types', name') <- naming_type_name_1 types name
      return (types', (name', kind))
  naming_type_variables ::
    (
      Dictionary Language_or_location ->
      [Parser.Type_variable] ->
      Naming_1 (Dictionary Language_or_location, [Naming.Type_variable]))
  naming_type_variables types type_variables =
    case type_variables of
      [] -> return (types, [])
      type_variable : type_variables' ->
        do
          (types', type_variable') <- naming_type_variable types type_variable
          (types'', type_variables'') <- naming_type_variables types' type_variables'
          return (types'', type_variable' : type_variables'')
--------------------------------------------------------------------------------------------------------------------------------