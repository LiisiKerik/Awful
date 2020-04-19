--------------------------------------------------------------------------------------------------------------------------------
{-# OPTIONS_GHC -Wall #-}
module Main where
  import Control.Monad.Trans (lift)
  import Control.Monad.Except (ExceptT (..), MonadError (..), liftEither, runExceptT)
  import Control.Monad.State (MonadState (..), StateT, runStateT)
  import Data.Foldable (traverse_)
  import Data.Map (empty, fromList, insert, lookup, union, unions)
  import Dictionary (Dictionary, Name (..))
  import Errors (Err, Error (..), Error' (..), Language_or_location (..), add_file_name, write_error)
  import Eval (eval, run)
  import Naming (naming_file, naming_term, naming_tests)
  import Parser (
    Code_file,
    Code_file' (..),
    Input (..),
    Term,
    Test,
    Test_file (..),
    parse_code_file,
    parse_input,
    parse_test_file)
  import System.Directory (findFile)
  import System.Environment (getArgs)
  import Typing ((!->), (!=>), Term (..), Term_pattern (..), Type (..), type_file, type_term, type_tests)
  type Env = (Dictionary Language_or_location, Dictionary Type, Dictionary Typing.Term)
  type File_checker =
    StateT (Dictionary Language_or_location, Dictionary (Dictionary Type), Dictionary Typing.Term) (ExceptT Error IO)
  check :: [Name] -> ExceptT Error IO Env
  check file_names =
    do
      (term_types, (term_locations, _, term_defs)) <-
        runStateT (check_files [] "input" file_names) (init_term_locations, empty, init_term_defs)
      return (term_locations, term_types, term_defs)
  check_circular_dependencies :: [String] -> String -> File_checker ()
  check_circular_dependencies file_names file_name =
    case elem file_name file_names of
      False -> return ()
      True -> throwError (Circular_dependency_between_files file_names)
  check_file :: [String] -> String -> Name -> File_checker (Dictionary Type)
  check_file requesting_file_names requesting_file_name (Name line_and_char requested_file_name) =
    do
      files <- get_files
      case Data.Map.lookup requested_file_name files of
        Nothing ->
          do
            check_circular_dependencies requesting_file_names requested_file_name
            file <-
              lift
                (ExceptT
                  (
                    (\maybe_file ->
                      case maybe_file of
                        Nothing -> Left (Error requesting_file_name (Failed_to_find_file requested_file_name line_and_char))
                        Just file -> Right file) <$>
                    read_file requested_file_name))
            Code_file' imports file' <- liftEither (add_file_name requested_file_name (parse_code_file file))
            term_types <- check_files (requested_file_name : requesting_file_names) requested_file_name imports
            term_locations <- get_term_locations
            term_defs <- get_term_defs
            (term_locations', term_types', term_defs') <-
              liftEither (naming_and_type_file requested_file_name term_locations term_types file')
            put (term_locations', insert requested_file_name term_types' files, union term_defs term_defs')
            return term_types'
        Just term_types -> return term_types
  check_files :: [String] -> String -> [Name] -> File_checker (Dictionary Type)
  check_files requested_file_names requesting_file_name imports =
    do
      term_types <- traverse (check_file requested_file_names requesting_file_name) imports
      return (unions (init_term_types : term_types))
  eval :: [Name] -> Parser.Term -> ExceptT Error IO String
  eval file_names term =
    do
      env <- check file_names
      liftEither (naming_type_and_eval_term env term)
  get_files :: File_checker (Dictionary (Dictionary Type))
  get_files =
    do
      (_, files, _) <- get
      return files
  get_term_defs :: File_checker (Dictionary Typing.Term)
  get_term_defs =
    do
      (_, _, term_defs) <- get
      return term_defs
  get_term_locations :: File_checker (Dictionary Language_or_location)
  get_term_locations =
    do
      (term_locations, _, _) <- get
      return term_locations
{-
  init_constructors :: Set String
  init_constructors = Data.Set.fromList ["EQ", "GT", "LT"]
-}
  init_term_defs :: Dictionary Typing.Term
  init_term_defs =
    Data.Map.fromList
      [
        ("Add", Name_term_pattern "x" !-> Name_term_pattern "y" !-> Add_Int_term (Name_term "x") (Name_term "y")),
        -- ("Compare", Name_term_pattern "x" !-> Name_term_pattern "y" !-> Compare_Int_term (Name_term "x") (Name_term "y")),
        ("Convert", Name_term_pattern "x" !-> Name_term "x"),
        ("Div", Name_term_pattern "x" !-> Name_term_pattern "y" !-> Div_term (Name_term "x") (Name_term "y")),
        ("Mod", Name_term_pattern "x" !-> Name_term_pattern "y" !-> Mod_term (Name_term "x") (Name_term "y")),
        ("Times", Name_term_pattern "x" !-> Name_term_pattern "y" !-> Times_Int_term (Name_term "x") (Name_term "y"))]
  init_term_locations :: Dictionary Language_or_location
  init_term_locations = Data.Map.fromList ((\name -> (name, Language)) <$> ["Add", "Convert", "Div", "Mod", "Times"])
  init_term_types :: Dictionary Type
  init_term_types =
    Data.Map.fromList
      [
        ("Add", Int_type !=> Int_type !=> Int_type),
        -- ("Compare", Int_type !=> Int_type !=> Ordering_type),
        ("Convert", Int_type !=> Int_type),
        ("Div", Int_type !=> Int_type !=> Int_type),
{-
        ("EQ", Ordering_type),
        ("GT", Ordering_type),
        ("LT", Ordering_type),
-}
        ("Mod", Int_type !=> Int_type !=> Int_type),
        ("Times", Int_type !=> Int_type !=> Int_type)]
  main :: IO ()
  main =
    do
      args <- getArgs
      case args of
        [arg] ->
          case parse_input arg of
            Left err -> putStrLn (write_error err)
            Right input ->
              case input of
                Check file_names ->
                  do
                    result <- runExceptT (check file_names)
                    case result of
                      Left err -> putStrLn (write_error err)
                      Right _ -> return ()
                Eval file_names term ->
                  do
                    maybe_term' <- runExceptT (Main.eval file_names term)
                    putStrLn
                      (case maybe_term' of
                        Left err -> write_error err
                        Right term' -> term')
                Run file_names ->
                  do
                    result <- runExceptT (traverse_ Main.run file_names)
                    case result of
                      Left err -> putStrLn (write_error err)
                      Right () -> return ()
        _ -> putStrLn (write_error Input_error)
  naming_and_type_file :: String -> Dictionary Language_or_location -> Dictionary Type -> Code_file -> Err Env
  naming_and_type_file file_name term_locations term_types file =
    do
      (term_locations', file') <- naming_file file_name term_locations file
      (term_types', term_defs) <- add_file_name file_name (type_file term_types file')
      return (term_locations', term_types', term_defs)
  naming_type_and_eval_term :: Env -> Parser.Term -> Err String
  naming_type_and_eval_term (term_locations, term_types, term_defs) term =
    do
      term' <- naming_term term_locations term
      term'' <- type_term term_types term'
      Eval.eval term_defs term''
  naming_type_and_run_tests :: String -> Env -> [Parser.Test] -> Err ()
  naming_type_and_run_tests file_name (term_locations, term_types, term_defs) tests =
    do
      tests' <- naming_tests file_name term_locations tests
      tests'' <- add_file_name file_name (type_tests term_types tests')
      add_file_name file_name (Eval.run term_defs tests'')
  read_file :: String -> IO (Maybe String)
  read_file file_name =
    do
      maybe_file_path <- findFile [""] file_name
      case maybe_file_path of
        Nothing -> return Nothing
        Just file_path ->
          do
            file <- readFile file_path
            return (Just file)
  run :: Name -> ExceptT Error IO ()
  run (Name line_and_char file_name) =
    do
      file <-
        ExceptT
          (
            (\maybe_file ->
              case maybe_file of
                Nothing -> Left (Error "input" (Failed_to_find_file file_name line_and_char))
                Just file -> Right file) <$>
            read_file file_name)
      Test_file imports tests <- liftEither (add_file_name file_name (parse_test_file file))
      env <- check imports
      liftEither (naming_type_and_run_tests file_name env tests)
--------------------------------------------------------------------------------------------------------------------------------