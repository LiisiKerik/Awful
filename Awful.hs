-----------------------------------------------------------------------------------------------------------------------------
{-# OPTIONS_GHC -Wall #-}
import Data.Bifunctor
import Data.List
import Data.Map
import Data.Set
import Eval
import Naming
import Standard
import System.Directory
import System.Environment
import System.FilePath
import Tokenise
import Tree
import Typing
type Files = Map' (File, Map' Op)
check ::
  (
    [String] ->
    (
      Files,
      (Set String, Locations, Locations),
      Map' Expression_2,
      Map' Polykind,
      Map' (Map' Location'),
      Map' ([String], Map' [(String, Nat)])) ->
    Location' ->
    String ->
    IO
      (Err
        (
          (
            Files,
            (Set String, Locations, Locations),
            Map' Expression_2,
            Map' Polykind,
            Map' (Map' Location'),
            Map' ([String], Map' [(String, Nat)])),
          (File, Map' Op))))
check b m' @ (f, _, _, _, _, _) j name_qc =
  case Data.Map.lookup name_qc f of
    Just a -> return (Right (m', a))
    Nothing ->
      case check' name_qc b of
        Just a -> return (Left ("Circular dependency between files [" ++ intercalate ", " a ++ "]."))
        Nothing -> do
          find_file <- findFile [""] name_qc
          case find_file of
            Just file -> do
              a <- readFile file
              case parse_tree (Location_1 name_qc) a of
                Left c -> return (Left c)
                Right (Tree_1 c d) -> do
                  g <-
                    check_imports
                      (name_qc : b)
                      (m', init_type_context)
                      ((\(Name h i) -> (Library (Location_1 name_qc h), i)) <$> c)
                  return
                    (
                      g >>=
                      \((h, i, l, p, p', r'), m) ->
                        (
                          (\(k, n, o, q, s, t') -> ((Data.Map.insert name_qc n h, k, o, q, s, t'), n)) <$>
                          standard_naming_typing name_qc d (i, m, l, p, p', r')))
            Nothing ->
              err
                (
                  "Failed to find file " ++
                  name_qc ++
                  " requested" ++
                  case j of
                    Language -> " in the command."
                    Library k -> location' k)
check' :: String -> [String] -> Maybe [String]
check' a b =
  case b of
    [] -> Nothing
    c : d -> if c == a then Just [a] else (:) c <$> check' a d
check_extension :: String -> Err ()
check_extension a =
  case takeExtension a of
    ".awf" -> Right ()
    _ -> Left ("File " ++ a ++ " has an invalid extension. You can only use a .awf file.")
check_extensions :: String -> [String] -> Err ([String], String)
check_extensions a c =
  case c of
    [] -> Right ([], a)
    d : e -> check_extension a >> first ((:) a) <$> check_extensions d e
check_imports ::
  (
    [String] ->
    (
      (
        Files,
        (Set String, Locations, Locations),
        Map' Expression_2,
        Map' Polykind,
        Map' (Map' Location'),
        Map' ([String], Map' [(String, Nat)])),
      (File, Map' Op)) ->
    [(Location', String)] ->
    IO
      (Err
        (
          (
            Files,
            (Set String, Locations, Locations),
            Map' Expression_2,
            Map' Polykind,
            Map' (Map' Location'),
            Map' ([String], Map' [(String, Nat)])),
          (File, Map' Op))))
check_imports a b @ (f, k) c =
  case c of
    [] -> return (Right b)
    (d, g) : e -> do
      x <- check a f d g
      case x of
        Left i -> err i
        Right (i, n) -> check_imports a (i, context_union k n) e
err :: String -> IO (Err t)
err = return <$> Left
eval'' :: [String] -> String -> IO (Err String)
eval'' a b = do
  c <- check_imports [] (init', init_type_context) ((,) Language <$> a)
  return
    (
      c >>=
      \((_, (e, t, _), f, j, _, y), (File _ g h i w _ _ _ m _ _ z, u)) ->
        tokenise_parse_naming_typing_eval (e, t) j (g, h, i) f b m y w z u)
init' ::
  (
    Files,
    (Set String, Locations, Locations),
    Map' Expression_2,
    Map' Polykind,
    Map' (Map' Location'),
    Map' ([String], Map' [(String, Nat)]))
init' =
  (
    Data.Map.empty,
    (Data.Set.singleton "Pair", locations, Data.Map.fromList ((\x -> (x, Language)) <$> ["#", "->", "="])),
    defs,
    kinds,
    Data.Map.fromList
      [
        ("Field", Data.Map.fromList [("Modular", Language)]),
        ("Nonzero", Data.Map.fromList []),
        ("Ord", Data.Map.fromList [("Char", Language), ("Int", Language), ("Modular", Language)]),
        ("Ring", Data.Map.fromList [("Int", Language), ("Modular", Language)]),
        ("Writeable", Data.Map.fromList [("Int", Language), ("Modular", Language)])],
    Data.Map.fromList
      [
        ("Field", (["Inverse"], Data.Map.fromList [("Modular", [("Nonzero", Zr)])])),
        (
          "Nonzero",
          (
            [
              "Add_Modular",
              "Convert_Modular",
              "Div'",
              "Inverse_Modular",
              "Multiply_Modular",
              "Negate_Modular",
              "Write_Brackets_Modular"],
            Data.Map.fromList [])),
        ("Ord", (["Compare"], Data.Map.fromList [("Char", []), ("Int", []), ("Modular", [])])),
        (
          "Ring",
          (["Add", "Convert", "Multiply", "Negate"], Data.Map.fromList [("Int", []), ("Modular", [("Nonzero", Zr)])])),
        ("Writeable", (["Write_Brackets"], Data.Map.fromList [("Int", []), ("Modular", [("Nonzero", Zr)])]))])
main :: IO ()
main = do
  args <- getArgs
  case args of
    [] -> putStrLn "Missing command."
    command : arguments ->
      case command of
        "check" ->
          case arguments of
            [f] ->
              case check_extension f of
                Left a -> putStrLn a
                _ -> do
                  res <- check [] init' Language f
                  putStrLn
                    (case res of
                      Left e -> e
                      _ -> "Library check successful!")
            _ -> putStrLn "Command check expects 1 argument."
        "eval" ->
          case arguments of
            a : b ->
              case check_extensions a b of
                Left e -> putStrLn e
                Right (c, d) -> do
                  e <- eval'' c d
                  putStrLn
                    (case e of
                      Left f -> f
                      Right f -> f)
            _ -> putStrLn "Command eval expects at least 1 argument."
        _ -> putStrLn ("Invalid command " ++ command ++ ".")
-----------------------------------------------------------------------------------------------------------------------------