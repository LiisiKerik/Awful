--------------------------------------------------------------------------------------------------------------------------------
{-# OPTIONS_GHC -Wall #-}
module Files (checks, eval'') where
  import Data.Map
  import Data.Set
  import Dictionary
  import Errors
  import Eval
  import System.Directory
  import Tree
  import Typing
  check ::
    (
      [String] ->
      (
        Dictionary
          (
            (
              Dictionary Kind_0,
              Dictionary Constructor_3,
              Dictionary Polymorphic_type,
              Dictionary Class_6,
              Dictionary Class_5,
              Dictionary (Dictionary [[String]]),
              Dictionary Kind_0),
            (Dictionary Operator_0, Dictionary Operator_0)),
        (
          Set String,
          (
            Dictionary Language_or_location,
            Dictionary Language_or_location,
            Dictionary Language_or_location,
            Dictionary Language_or_location,
            Dictionary Language_or_location,
            Dictionary' Language_or_location)),
        Dictionary Polymorphic_term) ->
      String ->
      Line_and_char ->
      String ->
      IO
        (Err
          (
            (
              Dictionary
                (
                  (
                    Dictionary Kind_0,
                    Dictionary Constructor_3,
                    Dictionary Polymorphic_type,
                    Dictionary Class_6,
                    Dictionary Class_5,
                    Dictionary (Dictionary [[String]]),
                    Dictionary Kind_0),
                  (Dictionary Operator_0, Dictionary Operator_0)),
              (
                Set String,
                (
                  Dictionary Language_or_location,
                  Dictionary Language_or_location,
                  Dictionary Language_or_location,
                  Dictionary Language_or_location,
                  Dictionary Language_or_location,
                  Dictionary' Language_or_location)),
              Dictionary Polymorphic_term),
            (
              (
                Dictionary Kind_0,
                Dictionary Constructor_3,
                Dictionary Polymorphic_type,
                Dictionary Class_6,
                Dictionary Class_5,
                Dictionary (Dictionary [[String]]),
                Dictionary Kind_0),
              (Dictionary Operator_0, Dictionary Operator_0)))))
  check b m' @ (f, _, _) j7 j name_qc =
    case Data.Map.lookup name_qc f of
      Just a -> return (Right (m', a))
      Nothing ->
        case check' name_qc b of
          Just a -> return (Left (Circular_dependency_between_files a))
          Nothing ->
            do
              find_file <- findFile [""] name_qc
              case find_file of
                Just file -> do
                  a <- readFile file
                  case parse_file name_qc a of
                    Left c -> return (Left c)
                    Right (File_1 c d) -> do
                      g <- check_imports name_qc (name_qc : b) (m', init_type_context) c Data.Map.empty
                      return
                        (
                          g >>=
                          \((h, i, l), m) ->
                            (
                              (\(k, n, o) -> ((Data.Map.insert name_qc n h, k, o), n)) <$>
                              standard_naming_typing name_qc d (i, m, l)))
                Nothing -> return (Left (Failed_to_find_the_file name_qc j7 j))
  check' :: String -> [String] -> Maybe [String]
  check' a b =
    case b of
      [] -> Nothing
      c : d -> if c == a then Just [a] else (:) c <$> check' a d
  check_imports ::
    (
      String ->
      [String] ->
      (
        (
          Dictionary
            (
              (
                Dictionary Kind_0,
                Dictionary Constructor_3,
                Dictionary Polymorphic_type,
                Dictionary Class_6,
                Dictionary Class_5,
                Dictionary (Dictionary [[String]]),
                Dictionary Kind_0),
              (Dictionary Operator_0, Dictionary Operator_0)),
          (
            Set String,
            (
              Dictionary Language_or_location,
              Dictionary Language_or_location,
              Dictionary Language_or_location,
              Dictionary Language_or_location,
              Dictionary Language_or_location,
              Dictionary' Language_or_location)),
          Dictionary Polymorphic_term),
        (
          (
            Dictionary Kind_0,
            Dictionary Constructor_3,
            Dictionary Polymorphic_type,
            Dictionary Class_6,
            Dictionary Class_5,
            Dictionary (Dictionary [[String]]),
            Dictionary Kind_0),
          (Dictionary Operator_0, Dictionary Operator_0))) ->
      [Name] ->
      Dictionary Line_and_char ->
      IO
        (Err
          (
            (
              Dictionary
                (
                  (
                    Dictionary Kind_0,
                    Dictionary Constructor_3,
                    Dictionary Polymorphic_type,
                    Dictionary Class_6,
                    Dictionary Class_5,
                    Dictionary (Dictionary [[String]]),
                    Dictionary Kind_0),
                  (Dictionary Operator_0, Dictionary Operator_0)),
              (
                Set String,
                (
                  Dictionary Language_or_location,
                  Dictionary Language_or_location,
                  Dictionary Language_or_location,
                  Dictionary Language_or_location,
                  Dictionary Language_or_location,
                  Dictionary' Language_or_location)),
              Dictionary Polymorphic_term),
            (
              (
                Dictionary Kind_0,
                Dictionary Constructor_3,
                Dictionary Polymorphic_type,
                Dictionary Class_6,
                Dictionary Class_5,
                Dictionary (Dictionary [[String]]),
                Dictionary Kind_0),
              (Dictionary Operator_0, Dictionary Operator_0)))))
  check_imports j a b @ (f, k) c h =
    case c of
      [] -> return (Right b)
      Name (Line_and_char d m) g : e ->
        case Data.Map.lookup g h of
          Nothing ->
            do
              x <- check a f j (Line_and_char d m) g
              case x of
                Left i -> return (Left i)
                Right (i, n) -> check_imports j a (i, context_union k n) e (Data.Map.insert g (Line_and_char d m) h)
          Just i -> return (Left (Duplicate_import_of_file g j i (Line_and_char d m)))
  checks :: [Name] -> IO String
  checks a =
    case a of
      [] -> return "Library check successful!"
      Name b c : d ->
        do
          e <- check [] init' "input" b c
          case e of
            Left f -> return (write_error f)
            Right _ -> checks d
  eval'' :: [Name] -> Term_0 -> IO (Err String)
  eval'' a b = do
    c <- check_imports "input" [] (init', init_type_context) a Data.Map.empty
    return
      (
        c >>=
        \((_, (e, (_, _, z, x, _, _)), f), ((j, h, i, _, _, m, _), (_, u))) ->
          tokenise_parse_naming_typing_eval (e, z, x) j (h, i) f b m u)
  init' ::
    (
      Dictionary
        (
          (
            Dictionary Kind_0,
            Dictionary Constructor_3,
            Dictionary Polymorphic_type,
            Dictionary Class_6,
            Dictionary Class_5,
            Dictionary (Dictionary [[String]]),
            Dictionary Kind_0),
          (Dictionary Operator_0, Dictionary Operator_0)),
      (
        Set String,
        (
          Dictionary Language_or_location,
          Dictionary Language_or_location,
          Dictionary Language_or_location,
          Dictionary Language_or_location,
          Dictionary Language_or_location,
          Dictionary' Language_or_location)),
      Dictionary Polymorphic_term)
  init' =
    (
      Data.Map.empty,
      (
        Data.Set.fromList ["EQ", "GT", "Just", "LT", "Nothing"],
        (
          Data.Map.empty,
          Data.Map.empty,
          locations_0,
          locations_2,
          locations_1,
          Data.Map.fromList
            [
              (("Field", "Modular"), Language),
              (("Nonzero", "Next"), Language),
              (("Ord", "Int"), Language),
              (("Ord", "Modular"), Language),
              (("Ring", "Int"), Language),
              (("Ring", "Modular"), Language)])),
      defs)
--------------------------------------------------------------------------------------------------------------------------------