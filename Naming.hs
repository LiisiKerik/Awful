--------------------------------------------------------------------------------------------------------------------------------
{-# OPTIONS_GHC -Wall #-}
module Naming (Arrow_2 (..), Term_2 (..), Term_pattern_2 (..), naming_term) where
  import Data.Map
  import Data.Set
  import Dictionary
  import Errors
  import Parser
  import Standard
  import Transf
  data Arrow_2 = Arrow_2 Term_pattern_2 Term_2 deriving Show
  type Naming = Transf (String, Set String) () Err
  data Term_2 =
    Application_term_2 Term_2 Term_2 |
    Arrow_term_2 Arrow_2 |
    Int_term_2 Integer |
    Match_term_2 Term_2 [Arrow_2] |
    Name_term_2 Name
      deriving Show
  data Term_pattern_2 = Int_term_pattern_2 Integer | Name_term_pattern_2 String | Struct_term_pattern_2 Name deriving Show
  naming_arrow :: Dictionary Language_or_location -> Arrow_1 -> Naming Arrow_2
  naming_arrow terms (Arrow_1 term_pattern term) =
    do
      (terms', term_pattern') <- naming_term_pattern term_pattern terms
      Arrow_2 term_pattern' <$> naming_term terms' term
  naming_term :: Dictionary Language_or_location -> Standard.Term -> Naming Term_2
  naming_term terms term =
    case term of
      Standard.Application_term term_0 term_1 -> Application_term_2 <$> naming_term terms term_0 <*> naming_term terms term_1
      Standard.Arrow_term arrow -> Arrow_term_2 <$> naming_arrow terms arrow
      Standard.Int_term int -> return (Int_term_2 int)
      Standard.Match_term term' arrows -> Match_term_2 <$> naming_term terms term' <*> traverse (naming_arrow terms) arrows
      Standard.Name_term name -> return (Name_term_2 name)
  naming_term_pattern ::
    Term_pattern_1 -> Dictionary Language_or_location -> Naming (Dictionary Language_or_location, Term_pattern_2)
  naming_term_pattern (Term_pattern_1 _ term_pattern) terms =
    case term_pattern of
      Int_term_pattern_0 int -> return (terms, Int_term_pattern_2 int)
      Name_term_pattern_0 (Name line_and_char name) ->
        do
          (file_name, constructors) <- read_context
          case Data.Set.member name constructors of
            False ->
              case Data.Map.lookup name terms of
                Nothing -> return (Data.Map.insert name (Location file_name line_and_char) terms, Name_term_pattern_2 name)
                Just language_or_location ->
                  return_result (Left (Conflicting_definitions_of_term name language_or_location line_and_char))
            True -> return (terms, Struct_term_pattern_2 (Name line_and_char name))
--------------------------------------------------------------------------------------------------------------------------------