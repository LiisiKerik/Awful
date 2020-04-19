--------------------------------------------------------------------------------------------------------------------------------
{-# OPTIONS_GHC -Wall #-}
module Naming (
  Naming.Arrow (..),
  Naming.Def_or_instance (..),
  Naming.File (..),
  Naming.Term (..),
  Naming.Term_pattern (..),
  Naming.Test (..),
  naming_file,
  naming_term,
  naming_tests) where
  import Control.Monad.Except (MonadError (..))
  import Control.Monad.Reader (MonadReader (..))
  import Control.Monad.State.Strict (MonadState (..))
  import Control.Monad.Trans.Reader (ReaderT, runReaderT)
  import Dictionary (Dictionary, Name (..), insert)
  import Errors (Err, Error (..), Language_or_location (..), Line_and_char)
  import Parser (
    Arrow (..),
    Code_file (..),
    Def_or_instance (..),
    Term (..),
    Term_pattern (..),
    Term_pattern' (..),
    Test (..),
    Type)
  import Transf (RST, runRST)
  data Arrow = Arrow Naming.Term_pattern Naming.Term
      deriving Show
  data Def_or_instance = Def Line_and_char String Type Naming.Term
      deriving Show
  data Def_or_instance' = Def' Line_and_char String Type Parser.Term
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
    Name_term Name
      deriving Show
  data Term_pattern = Blank_term_pattern | Int_term_pattern Integer | Name_term_pattern String
      deriving Show
  data Test = Test Line_and_char Naming.Term Naming.Term
      deriving Show
  naming_arrow :: Dictionary Language_or_location -> Parser.Arrow -> Naming_1 Naming.Arrow
  naming_arrow terms (Parser.Arrow term_pattern term) =
    do
      (terms', term_pattern') <- naming_term_pattern terms term_pattern
      Naming.Arrow term_pattern' <$> naming_term' terms' term
  naming_def_or_instance_0 :: Parser.Def_or_instance -> Naming_0 Def_or_instance'
  naming_def_or_instance_0 def_or_instance =
    case def_or_instance of
      Parser.Def line_and_char name typ term ->
        do
          name' <- naming_term_name (Name line_and_char name)
          return (Def' line_and_char name' typ term)
  naming_def_or_instance_1 :: Dictionary Language_or_location -> Def_or_instance' -> Naming_1 Naming.Def_or_instance
  naming_def_or_instance_1 terms def_or_instance =
    case def_or_instance of
      Def' line_and_char name typ term -> Naming.Def line_and_char name typ <$> naming_term' terms term
  naming_file ::
    String -> Dictionary Language_or_location -> Parser.Code_file -> Err (Dictionary Language_or_location, Naming.File)
  naming_file file_name terms file =
    do
      (file', terms') <- runRST (naming_file_0 file) file_name terms
      file'' <- runReaderT (naming_file_1 terms' file') file_name
      return (terms', file'')
  naming_file_0 :: Parser.Code_file -> Naming_0 Naming.File'
  naming_file_0 (Parser.Code_file defs_and_instances) = Naming.File' <$> traverse naming_def_or_instance_0 defs_and_instances
  naming_file_1 :: Dictionary Language_or_location -> Naming.File' -> Naming_1 Naming.File
  naming_file_1 terms (Naming.File' defs_and_instances) =
    Naming.File <$> traverse (naming_def_or_instance_1 terms) defs_and_instances
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
  naming_term_name :: Name -> Naming_0 String
  naming_term_name (Name line_and_char name) =
    do
      file_name <- ask
      terms <- get
      case Dictionary.insert name (Location file_name line_and_char) terms of
        Left language_or_location ->
          throwError (Conflicting_definitions_of_term name language_or_location file_name line_and_char)
        Right terms' ->
          do
            put terms'
            return name
  naming_term_pattern ::
    Dictionary Language_or_location -> Term_pattern' -> Naming_1 (Dictionary Language_or_location, Naming.Term_pattern)
  naming_term_pattern terms (Term_pattern' _ term_pattern) =
    case term_pattern of
      Parser.Blank_term_pattern -> return (terms, Naming.Blank_term_pattern)
      Parser.Int_term_pattern i -> return (terms, Naming.Int_term_pattern i)
      Parser.Name_term_pattern (Name line_and_char name) ->
{-
        do
          (file_name, constructors) <- read_context
          case member name constructors of
            False ->
              case Data.Map.lookup name terms of
                Nothing -> return (Data.Map.insert name (Location file_name line_and_char) terms, Naming.Name_term_pattern name)
                Just language_or_location ->
                  return_result (Left (Conflicting_definitions_of_term name language_or_location file_name line_and_char))
            True -> return (terms, Struct_term_pattern (Name line_and_char name))
-}
        do
          file_name <- ask
          case Dictionary.insert name (Location file_name line_and_char) terms of
            Left language_or_location ->
              throwError (Conflicting_definitions_of_term name language_or_location file_name line_and_char)
            Right terms' -> return (terms', Naming.Name_term_pattern name)
  naming_test :: Dictionary Language_or_location -> Parser.Test -> Naming_1 Naming.Test
  naming_test terms (Parser.Test line_and_char term_0 term_1) =
    Naming.Test line_and_char <$> naming_term' terms term_0 <*> naming_term' terms term_1
  naming_tests :: String -> Dictionary Language_or_location -> [Parser.Test] -> Err [Naming.Test]
  naming_tests file_name terms tests = runReaderT (traverse (naming_test terms) tests) file_name
--------------------------------------------------------------------------------------------------------------------------------