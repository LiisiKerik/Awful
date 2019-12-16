--------------------------------------------------------------------------------------------------------------------------------
{-# OPTIONS_GHC -Wall #-}
module Files (eval) where
  import Data.Map
  import Data.Set
  import Dictionary
  import Errors
  import Eval
  import Naming
  import Parser
  import Standard
  import Transf
  import Typing
  eval :: String -> Err String
  eval term =
    do
      term' <- parse_term term
      term'' <- run (naming_term init_term_locations (standard_term term')) ("input", init_constructors) ()
      term''' <- type_term init_term_types term''
      eval' init_term_defs term'''
  init_constructors :: Set String
  init_constructors = Data.Set.fromList ["EQ", "GT", "LT"]
  init_term_defs :: Dictionary Term_3
  init_term_defs =
    Data.Map.fromList
      [
        ("Add", Name_term_pattern_3 "x" !-> Name_term_pattern_3 "y" !-> Add_Int_term_3 (Name_term_3 "x") (Name_term_3 "y")),
        (
          "Compare",
          Name_term_pattern_3 "x" !-> Name_term_pattern_3 "y" !-> Compare_Int_term_3 (Name_term_3 "x") (Name_term_3 "y")),
        ("Convert", Name_term_pattern_3 "x" !-> Name_term_3 "x"),
        ("Div", Name_term_pattern_3 "x" !-> Name_term_pattern_3 "y" !-> Div_term_3 (Name_term_3 "x") (Name_term_3 "y")),
        ("EQ", EQ_term_3),
        ("GT", GT_term_3),
        ("LT", LT_term_3),
        ("Mod", Name_term_pattern_3 "x" !-> Name_term_pattern_3 "y" !-> Mod_term_3 (Name_term_3 "x") (Name_term_3 "y")),
        ("Times", Name_term_pattern_3 "x" !-> Name_term_pattern_3 "y" !-> Times_Int_term_3 (Name_term_3 "x") (Name_term_3 "y"))]
  init_term_locations :: Dictionary Language_or_location
  init_term_locations = Data.Map.fromList ((\name -> (name, Language)) <$> ["Add", "Convert", "Div", "Mod", "Times"])
  init_term_types :: Dictionary Type
  init_term_types =
    Data.Map.fromList
      [
        ("Add", Int_type !=> Int_type !=> Int_type),
        ("Compare", Int_type !=> Int_type !=> Ordering_type),
        ("Convert", Int_type !=> Int_type),
        ("Div", Int_type !=> Int_type !=> Int_type),
        ("EQ", Ordering_type),
        ("GT", Ordering_type),
        ("LT", Ordering_type),
        ("Mod", Int_type !=> Int_type !=> Int_type),
        ("Times", Int_type !=> Int_type !=> Int_type)]
{-
  write :: Err String -> String
  write maybe_result =
    case maybe_result of
      Left err -> write_error "input" err
      Right result -> result
-}
--------------------------------------------------------------------------------------------------------------------------------