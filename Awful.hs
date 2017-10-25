-----------------------------------------------------------------------------------------------------------------------------
{-# OPTIONS_GHC -Wall #-}
import Data.Bifunctor
import Data.List
import Data.Map
import Eval
import Naming
import Standard
import System.Directory
import System.Environment
import System.FilePath
import Tokenise
import Tree
import Typing
type Files = Map' File
check ::
  [String] ->
  (Files, Locations, Defs, Map' Type_1) ->
  Location' ->
  String ->
  IO (Err (Files, Locations, Defs, Map' Type_1, File))
check b (f, x, y, e) j name_qc = case Data.Map.lookup name_qc f of
  Just a -> return (Right (f, x, y, e, a))
  Nothing -> case check' name_qc b of
    Just a -> return (Left ("Circular dependency between files " ++ intercalate ", " a ++ "."))
    Nothing -> do
      find_file <- findFile [""] name_qc
      case find_file of
        Just file -> do
          a <- readFile file
          case standard (Location_1 name_qc) a of
            Left c -> return (Left c)
            Right (Tree_3 c d) -> do
              g <-
                check_imports
                  (name_qc : b)
                  (f, x, y, e, init_type_context)
                  ((\(Name h i) -> (Library (Location_1 name_qc h), i)) <$> c)
              return
                (
                  g >>=
                  \(h, i, l, p, m) ->
                    (\(k, n, o, q) -> (Data.Map.insert name_qc n h, k, o, q, n)) <$> naming_typing name_qc d (i, m, l, p))
        Nothing -> err ("Failed to find file " ++ name_qc ++ " requested" ++ case j of
          Language -> " in the command."
          Library k -> location' k)
check' :: String -> [String] -> Maybe [String]
check' a b = case b of
  [] -> Nothing
  c : d -> if c == a then Just [a] else (:) c <$> check' a d
check_extension :: String -> Err ()
check_extension a = case takeExtension a of
  ".awf" -> Right ()
  _ -> Left ("File " ++ a ++ " has an invalid extension. You can only load a .awf file.")
check_extensions :: String -> [String] -> Err ([String], String)
check_extensions a c = case c of
  [] -> Right ([], a)
  d : e -> check_extension a >> first ((:) a) <$> check_extensions d e
check_imports ::
  [String] ->
  (Files, Locations, Defs, Map' Type_1, File) ->
  [(Location', String)] ->
  IO (Err (Files, Locations, Defs, Map' Type_1, File))
check_imports a b @ (f, h, l, p, k) c = case c of
  [] -> return (Right b)
  (d, g) : e -> do
    x <- check a (f, h, l, p) d g
    case x of
      Left i -> err i
      Right (i, j, m, o, n) -> check_imports a (i, j, m, o, context_union k n) e
err :: String -> IO (Err t)
err = return <$> Left
eval'' :: [String] -> String -> IO (Err String)
eval'' a b = do
  c <- check_imports [] (empty, locations, defs, kinds, init_type_context) ((,) Language <$> a)
  return (c >>= \(_, e, f, j, (File _ g h i _)) -> tokenise_parse_naming_typing_eval e j (g, h, i) f b)
main :: IO ()
main = do
  args <- getArgs
  case args of
    [] -> putStrLn "Missing command."
    command : arguments -> case command of
      "check" -> case arguments of
        [f] -> case check_extension f of
          Left a -> putStrLn a
          _ -> do
            res <- check [] (empty, locations, defs, kinds) Language f
            putStrLn (case res of
              Left e -> e
              _ -> "Library check successful!")
        _ -> putStrLn "Command check expects 1 argument."
      "eval" -> case arguments of
        a : b -> case check_extensions a b of
          Left e -> putStrLn e
          Right (c, d) -> do
            e <- eval'' c d
            putStrLn (case e of
              Left f -> f
              Right f -> f)
        _ -> putStrLn "Command compile expects at least 1 argument."
      _ -> putStrLn ("Invalid command " ++ command ++ ".")
-----------------------------------------------------------------------------------------------------------------------------