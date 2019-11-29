--------------------------------------------------------------------------------------------------------------------------------
{-# OPTIONS_GHC -Wall #-}
module Naming (
  Arrow_2 (..),
  Binding_2 (..),
  Class_3 (..),
  Constructor_2 (..),
  Data_3 (..),
  Data_branch_3 (..),
  Def_or_instance_2 (..),
  Field_2 (..),
  File_4 (..),
  Method_3 (..),
  Term_2 (..),
  Term_pattern_5 (..),
  Type_pattern_3 (..),
  Type_pattern_4 (..),
  Type_variable_1 (..),
  naming_file,
  naming_term) where
  import Data.Bifunctor
  import Dictionary
  import Errors
  import Modular
  import Standard
  import Transf
  import Tree
  data Arrow_2 = Arrow_2 Term_pattern_5 Term_2 deriving Show
  data Binding_2 = Binding_2 Name Term_2 deriving Show
  data Class_2 = Class_2 String Type_variable_0 [Constraint_0] [Method_2] deriving Show
  data Class_3 = Class_3 String Type_variable_1 [Constraint_0] [Method_3] deriving Show
  data Constructor_2 = Constructor_2 Line_and_char Status String [Type_3] deriving Show
  data Data_2 = Data_2 Status String [Type_variable_0] Data_branch_2 deriving Show
  data Data_3 = Data_3 Status String [Type_variable_1] Data_branch_3 deriving Show
  data Data_branch_2 =
    Algebraic_data_2 [Constructor_2] |
    Branch_data_2 Name (Data_branch_2, (Name, Data_branch_2)) |
    Struct_data_2 Line_and_char String [Field_2]
      deriving Show
  data Data_branch_3 =
    Algebraic_data_3 [Constructor_2] |
    Branch_data_3 Name (Data_branch_3, (String, Data_branch_3)) |
    Struct_data_3 Line_and_char String [Field_2]
      deriving Show
  data Def_or_instance_2 =
    Def_2 Line_and_char Status String [Type_variable_1] [Constraint_0] Type_2 Term_2 |
    Instance_2 Line_and_char Name Type_pattern_4 [Constraint_0] [Binding_2]
      deriving Show
  data Field_2 = Field_2 String Type_3 deriving Show
  data Method_2 = Method_2 String [Type_variable_0] Type_3 deriving Show
  data Method_3 = Method_3 String [Type_variable_1] Type_3 deriving Show
  data Term_2 =
    Application_term_2 Term_2 Term_2 |
    Arrow_term_2 Arrow_2 |
    Branch_term_2 Name (Term_2, (Type_pattern_3, Term_2)) |
    Int_term_2 Integer |
    Match_term_2 Term_2 [Arrow_2] |
    Modular_term_2 Modular_0 |
    Name_term_2 Name
      deriving Show
  data Term_pattern_5 =
    Blank_term_pattern_5 |
    Int_term_pattern_5 Integer |
    Modular_term_pattern_5 Modular_0 |
    Name_term_pattern_5 String |
    Struct_term_pattern_5 Name [Term_pattern_5]
      deriving Show
  data File_3 = File_3 [Data_2] [Class_2] [Def_or_instance_1] deriving Show
  data File_4 = File_4 [Data_3] [Class_3] [Def_or_instance_2] deriving Show
  data Type_pattern_3 = Blank_type_pattern_3 | Name_type_pattern_3 String deriving (Eq, Ord, Show)
  data Type_pattern_4 = Type_pattern_4 Name [Type_pattern_3] deriving Show
  data Type_variable_1 = Type_variable_1 String Kind_0 deriving Show
  naming_arrow :: String -> Dictionary Language_or_location -> Dictionary Language_or_location -> Arrow_1 -> Err Arrow_2
  naming_arrow file_name types terms_0 (Arrow_1 term_pattern term) =
    (
      naming_term_pattern term_pattern file_name terms_0 >>=
      \(terms_1, term_pattern') -> Arrow_2 term_pattern' <$> naming_term file_name types terms_1 term)
  naming_binding :: String -> Dictionary Language_or_location -> Dictionary Language_or_location -> Binding_1 -> Err Binding_2
  naming_binding file_name types terms (Binding_1 name term) = Binding_2 name <$> naming_term file_name types terms term
  naming_class_0 ::
    (
      Class_1 ->
      String  ->
      (Dictionary Language_or_location, Dictionary Language_or_location) ->
      Err ((Dictionary Language_or_location, Dictionary Language_or_location), Class_2))
  naming_class_0 (Class_1 line_and_char name_0 type_variable constraints methods) file_name (terms_0, classes_0) =
    (
      (\(classes_1, name_1) -> \(terms_1, methods') ->
        ((terms_1, classes_1), Class_2 name_1 type_variable constraints methods')) <$>
      naming_name "class" (Name line_and_char name_0) file_name classes_0 <*>
      transf_list naming_method_0 methods file_name terms_0)
  naming_class_1 :: String -> Dictionary Language_or_location -> Class_2 -> Err Class_3
  naming_class_1 file_name types_0 (Class_2 name type_variable constraints methods) =
    (
      naming_type_variable type_variable file_name types_0 >>=
      \(types_1, type_variable') ->
        Class_3 name type_variable' constraints <$> traverse (naming_method_1 file_name types_1) methods)
  naming_constructor ::
    Constructor_1 -> String -> Dictionary Language_or_location -> Err (Dictionary Language_or_location, Constructor_2)
  naming_constructor (Constructor_1 line_and_char status name_0 types) file_name terms =
    (
      second (\name_1 -> Constructor_2 line_and_char status name_1 types) <$>
      naming_name "term" (Name line_and_char name_0) file_name terms)
  naming_data_0 ::
    (
      Data_1 ->
      String ->
      (Dictionary Language_or_location, Dictionary Language_or_location) ->
      Err ((Dictionary Language_or_location, Dictionary Language_or_location), Data_2))
  naming_data_0 (Data_1 line_and_char status name_0 type_variables data_branch) file_name (types_0, terms_0) =
    (
      (\(types_1, name_1) -> \(terms_1, data_branch') ->
        ((types_1, terms_1), Data_2 status name_1 type_variables data_branch')) <$>
      naming_name "type" (Name line_and_char name_0) file_name types_0 <*>
      naming_data_branch_0 data_branch file_name terms_0)
  naming_data_1 :: String -> Dictionary Language_or_location -> Data_2 -> Err Data_3
  naming_data_1 file_name types_0 (Data_2 status name type_variables data_branch) =
    (
      naming_type_variables type_variables file_name types_0 >>=
      \(types_1, type_variables') -> Data_3 status name type_variables' <$> naming_data_branch_1 file_name types_1 data_branch)
  naming_data_branch_0 ::
    Data_branch_1 -> String -> Dictionary Language_or_location -> Err (Dictionary Language_or_location, Data_branch_2)
  naming_data_branch_0 data_branch_0 file_name terms_0 =
    case data_branch_0 of
      Algebraic_data_1 constructors -> second Algebraic_data_2 <$> transf_list naming_constructor constructors file_name terms_0
      Branch_data_1 type_variable_0 (data_branch_1, (type_variable_1, data_branch_2)) ->
        (
          naming_data_branch_0 data_branch_1 file_name terms_0 >>=
          \(terms_1, data_branch'_0) ->
            (
              second (\data_branch'_1 -> Branch_data_2 type_variable_0 (data_branch'_0, (type_variable_1, data_branch'_1))) <$>
              naming_data_branch_0 data_branch_2 file_name terms_1))
      Struct_data_1 line_and_char name_0 fields ->
        (
          naming_name "term" (Name line_and_char name_0) file_name terms_0 >>=
          \(terms_1, name_1) ->
            second (Struct_data_2 line_and_char name_1) <$> transf_list naming_field fields file_name terms_1)
  naming_data_branch_1 :: String -> Dictionary Language_or_location -> Data_branch_2 -> Err Data_branch_3
  naming_data_branch_1 file_name types_0 data_branch_0 =
    case data_branch_0 of
      Algebraic_data_2 constructors -> Right (Algebraic_data_3 constructors)
      Branch_data_2 type_variable_0 (data_branch_1, (type_variable_1, data_branch_2)) ->
        (
          Branch_data_3 type_variable_0 <$>
          (
            (,) <$>
            naming_data_branch_1 file_name types_0 data_branch_1 <*>
            (
              naming_name "type" type_variable_1 file_name types_0 >>=
              \(types_1, type_variable') -> (,) type_variable' <$> naming_data_branch_1 file_name types_1 data_branch_2)))
      Struct_data_2 line_and_char name fields -> Right (Struct_data_3 line_and_char name fields)
  naming_def_or_instance_0 ::
    (
      Def_or_instance_1 ->
      String ->
      (Dictionary Language_or_location, Dictionary' Language_or_location) ->
      Err ((Dictionary Language_or_location, Dictionary' Language_or_location), ()))
  naming_def_or_instance_0 def_or_instance file_name (terms_0, instances_0) =
    case def_or_instance of
      Def_1 line_and_char _ name _ _ _ _ ->
        (\(terms_1, _) -> ((terms_1, instances_0), ())) <$> naming_name "term" (Name line_and_char name) file_name terms_0
      Instance_1 line_and_char (Name _ cls) (Type_pattern_2 (Name _ typ) _) _ _ ->
        bimap
          (\language_or_location -> Conflicting_instances cls typ language_or_location file_name line_and_char)
          (\instances_1 -> ((terms_0, instances_1), ()))
          (add (cls, typ) (Location file_name line_and_char) instances_0)
  naming_def_or_instance_1 ::
    String -> Dictionary Language_or_location -> Dictionary Language_or_location -> Def_or_instance_1 -> Err Def_or_instance_2
  naming_def_or_instance_1 file_name types_0 terms def_or_instance =
    case def_or_instance of
      Def_1 line_and_char status name type_variables constraints typ term ->
        (
          naming_type_variables type_variables file_name types_0 >>=
          \(types_1, type_variables') ->
            (
              Def_2 line_and_char status name type_variables' constraints typ <$>
              naming_term file_name types_1 terms term))
      Instance_1 line_and_char cls type_pattern constraints bindings ->
        (
          naming_type_pattern_1 type_pattern file_name types_0 >>=
          \(types_1, type_pattern') ->
            (
              Instance_2 line_and_char cls type_pattern' constraints <$>
              traverse (naming_binding file_name types_1 terms) bindings))
  naming_field :: Field_1 -> String -> Dictionary Language_or_location -> Err (Dictionary Language_or_location, Field_2)
  naming_field (Field_1 name typ) file_name terms =
    second (\name' -> Field_2 name' typ) <$> naming_name "term" name file_name terms
  naming_file ::
    (
      File_2 ->
      String ->
      (
        Dictionary Language_or_location,
        Dictionary Language_or_location,
        Dictionary Language_or_location,
        Dictionary Language_or_location,
        Dictionary Language_or_location,
        Dictionary' Language_or_location) ->
      Err
        (
          (
            Dictionary Language_or_location,
            Dictionary Language_or_location,
            Dictionary Language_or_location,
            Dictionary Language_or_location,
            Dictionary Language_or_location,
            Dictionary' Language_or_location),
          File_4))
  naming_file file file_name names =
    (
      naming_file_0 file file_name names >>=
      \((type_operators, term_operators, types, terms, classes, instances), file') ->
        (,) (type_operators, term_operators, types, terms, classes, instances) <$> naming_file_1 file_name types terms file')
  naming_file_0 ::
    (
      File_2 ->
      String ->
      (
        Dictionary Language_or_location,
        Dictionary Language_or_location,
        Dictionary Language_or_location,
        Dictionary Language_or_location,
        Dictionary Language_or_location,
        Dictionary' Language_or_location) ->
      Err
        (
          (
            Dictionary Language_or_location,
            Dictionary Language_or_location,
            Dictionary Language_or_location,
            Dictionary Language_or_location,
            Dictionary Language_or_location,
            Dictionary' Language_or_location),
          File_3))
  naming_file_0
    (File_2 type_operators term_operators datas classes defs_and_instances)
    file_name
    (type_operators_0, term_operators_0, types_0, terms_0, classes_0, instances_0) =
      (
        naming_operators "type" type_operators file_name type_operators_0 >>=
        \type_operators_1 ->
          (
            naming_operators "term" term_operators file_name term_operators_0 >>=
            \term_operators_1 ->
              (
                transf_list naming_data_0 datas file_name (types_0, terms_0) >>=
                \((types_1, terms_1), datas') ->
                  (
                    transf_list naming_class_0 classes file_name (terms_1, classes_0) >>=
                    \((terms_2, classes_1), classes') ->
                      (
                        transf_list naming_def_or_instance_0 defs_and_instances file_name (terms_2, instances_0) >>=
                        \((terms_3, instances_1), _) ->
                          Right
                            (
                              (type_operators_1, term_operators_1, types_1, terms_3, classes_1, instances_1),
                              File_3 datas' classes' defs_and_instances))))))
  naming_file_1 :: String -> Dictionary Language_or_location -> Dictionary Language_or_location -> File_3 -> Err File_4
  naming_file_1 file_name types terms (File_3 datas classes defs_and_instances) =
    (
      File_4 <$>
      traverse (naming_data_1 file_name types) datas <*>
      traverse (naming_class_1 file_name types) classes <*>
      traverse (naming_def_or_instance_1 file_name types terms) defs_and_instances)
  naming_method_0 :: Method_1 -> String -> Dictionary Language_or_location -> Err (Dictionary Language_or_location, Method_2)
  naming_method_0 (Method_1 name type_variables typ) file_name terms =
    second (\name' -> Method_2 name' type_variables typ) <$> naming_name "term" name file_name terms
  naming_method_1 :: String -> Dictionary Language_or_location -> Method_2 -> Err Method_3
  naming_method_1 file_name types (Method_2 name type_variables typ) =
    (\(_, type_variables') -> Method_3 name type_variables' typ) <$> naming_type_variables type_variables file_name types
  naming_name :: String -> Name -> String -> Dictionary Language_or_location -> Err (Dictionary Language_or_location, String)
  naming_name kind (Name line_and_char name) file_name names_0 =
    (
      bimap
        (\language_or_location -> Conflicting_definitions kind name language_or_location file_name line_and_char)
        (\names_1 -> (names_1, name))
        (add name (Location file_name line_and_char) names_0))
  naming_operators :: String -> [Name] -> String -> Dictionary Language_or_location -> Err (Dictionary Language_or_location)
  naming_operators kind operators file_name operators_0 =
    fst <$> transf_list (naming_name (kind ++ " operator")) operators file_name operators_0
  naming_term :: String -> Dictionary Language_or_location -> Dictionary Language_or_location -> Term_1 -> Err Term_2
  naming_term file_name types_0 terms_0 term_0 =
    case term_0 of
      Application_term_1 term_1 term_2 ->
        Application_term_2 <$> naming_term file_name types_0 terms_0 term_1 <*> naming_term file_name types_0 terms_0 term_2
      Arrow_term_1 arrow -> Arrow_term_2 <$> naming_arrow file_name types_0 terms_0 arrow
      Branch_term_1 type_variable (term_1, (type_pattern, term_2)) ->
        (
          Branch_term_2 type_variable <$>
          (
            (,) <$>
            naming_term file_name types_0 terms_0 term_1 <*>
            (
              naming_type_pattern_0 type_pattern file_name types_0 >>=
              \(types_1, type_pattern') -> (,) type_pattern' <$> naming_term file_name types_1 terms_0 term_2)))
      Int_term_1 x -> Right (Int_term_2 x)
      Let_term_1 term_pattern term_1 term_2 ->
        (
          naming_term_pattern term_pattern file_name terms_0 >>=
          \(terms_1, term_pattern') ->
            (
              (\term'_0 -> \term'_1 -> Application_term_2 (Arrow_term_2 (Arrow_2 term_pattern' term'_1)) term'_0) <$>
              naming_term file_name types_0 terms_1 term_1 <*>
              naming_term file_name types_0 terms_1 term_2))
      Match_term_1 term_1 arrows ->
        (
          Match_term_2 <$>
          naming_term file_name types_0 terms_0 term_1 <*>
          traverse (naming_arrow file_name types_0 terms_0) arrows)
      Modular_term_1 x -> Right (Modular_term_2 x)
      Name_term_1 name -> Right (Name_term_2 name)
  naming_term_pattern ::
    Term_pattern_3 -> String -> Dictionary Language_or_location -> Err (Dictionary Language_or_location, Term_pattern_5)
  naming_term_pattern term_pattern file_name terms =
    case term_pattern of
      Struct_term_pattern_3 name term_patterns ->
        second (Struct_term_pattern_5 name) <$> transf_list naming_term_pattern term_patterns file_name terms
      Blank_term_pattern_3 -> Right (terms, Blank_term_pattern_5)
      Int_term_pattern_3 x -> Right (terms, Int_term_pattern_5 x)
      Modular_term_pattern_3 x -> Right (terms, Modular_term_pattern_5 x)
      Name_term_pattern_3 name -> second Name_term_pattern_5 <$> naming_name "term" name file_name terms
  naming_type_pattern_0 ::
    Type_pattern_0 -> String -> Dictionary Language_or_location -> Err (Dictionary Language_or_location, Type_pattern_3)
  naming_type_pattern_0 type_pattern file_name types =
    case type_pattern of
      Blank_type_pattern_0 -> Right (types, Blank_type_pattern_3)
      Name_type_pattern_0 name -> second Name_type_pattern_3 <$> naming_name "type" name file_name types
  naming_type_pattern_1 ::
    Type_pattern_2 -> String -> Dictionary Language_or_location -> Err (Dictionary Language_or_location, Type_pattern_4)
  naming_type_pattern_1 (Type_pattern_2 name type_patterns) file_name types =
    second (Type_pattern_4 name) <$> transf_list naming_type_pattern_0 type_patterns file_name types
  naming_type_variable ::
    Type_variable_0 -> String -> Dictionary Language_or_location -> Err (Dictionary Language_or_location, Type_variable_1)
  naming_type_variable (Type_variable_0 name kind) file_name types =
    second (\name' -> Type_variable_1 name' kind) <$> naming_name "type" name file_name types
  naming_type_variables ::
    [Type_variable_0] -> String -> Dictionary Language_or_location -> Err (Dictionary Language_or_location, [Type_variable_1])
  naming_type_variables = transf_list naming_type_variable
--------------------------------------------------------------------------------------------------------------------------------