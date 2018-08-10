--------------------------------------------------------------------------------------------------------------------------------
{-# OPTIONS_GHC -Wall #-}
import Data.List
import Data.Map
import Data.Set
import Eval
import Naming
import Standard
import System.Directory
import System.Environment
import Tokenise
import Tree
import Typing
type Files = Map' (File, Map' Op)
check ::
  (
    [String] ->
    (Files, ((Set String, Set String), Locations, Locations), Map' Expr_2, Map' (Map' Location')) ->
    Location_1 ->
    String ->
    IO (Err ((Files, ((Set String, Set String), Locations, Locations), Map' Expr_2, Map' (Map' Location')), (File, Map' Op))))
check b m' @ (f, _, _, _) j name_qc =
  case Data.Map.lookup name_qc f of
    Just a -> return (Right (m', a))
    Nothing ->
      case check' name_qc b of
        Just a -> return (Left ("Circular dependency between files (" ++ intercalate ", " a ++ ")."))
        Nothing ->
          do
            find_file <- findFile [""] name_qc
            case find_file of
              Just file -> do
                a <- readFile file
                case parse_tree (Location_1 name_qc) a of
                  Left c -> return (Left c)
                  Right (Tree_1 c d) -> do
                    g <- check_imports name_qc (name_qc : b) (m', init_type_context) c Data.Map.empty
                    return
                      (
                        g >>=
                        \((h, i, l, p'), m) ->
                          (
                            (\(k, n, o, s) -> ((Data.Map.insert name_qc n h, k, o, s), n)) <$>
                            standard_naming_typing name_qc d (i, m, l, p')))
              Nothing -> err ("Failed to find file " ++ name_qc ++ " requested" ++ location' j)
check' :: String -> [String] -> Maybe [String]
check' a b =
  case b of
    [] -> Nothing
    c : d -> if c == a then Just [a] else (:) c <$> check' a d
check_imports ::
  (
    String ->
    [String] ->
    ((Files, ((Set String, Set String), Locations, Locations), Map' Expr_2, Map' (Map' Location')), (File, Map' Op)) ->
    [Name] ->
    Map' Location_0 ->
    IO (Err ((Files, ((Set String, Set String), Locations, Locations), Map' Expr_2, Map' (Map' Location')), (File, Map' Op))))
check_imports j a b @ (f, k) c h =
  case c of
    [] -> return (Right b)
    Name (Location_0 d m) g : e ->
      case Data.Map.lookup g h of
        Nothing ->
          do
            x <- check a f (Location_1 j (Location_0 d m)) g
            case x of
              Left i -> err i
              Right (i, n) -> check_imports j a (i, context_union k n) e (Data.Map.insert g (Location_0 d m) h)
        Just (Location_0 i l) ->
          err
            (
              "Conflicting imports of " ++
              g ++
              " in " ++
              j ++
              " at " ++
              show i ++
              ":" ++
              show l ++
              " and " ++
              show d ++
              ":" ++
              show m ++
              ".")
checks :: [Name] -> IO String
checks a =
  case a of
    [] -> return "Library check successful!"
    Name b c : d ->
      do
        e <- check [] init' (Location_1 "input" b) c
        case e of
          Left f -> return f
          Right _ -> checks d
err :: String -> IO (Err t)
err = return <$> Left
eval'' :: [Name] -> Expression_0 -> IO (Err String)
eval'' a b = do
  c <- check_imports "input" [] (init', init_type_context) a Data.Map.empty
  return
    (c >>= \((_, (e, t, _), f, _), (File j g h i _ _ m _, u)) -> tokenise_parse_naming_typing_eval (e, t) j (g, h, i) f b m u)
init' :: (Files, ((Set String, Set String), Locations, Locations), Map' Expr_2, Map' (Map' Location'))
init' =
  (
    Data.Map.empty,
    (
      (
        Data.Set.singleton "Pair",
        Data.Set.fromList
          ["Construct_List", "EQ", "Empty_List", "False", "GT", "Left", "LT", "Nothing", "Pair", "Right", "True", "Wrap"]),
      locations,
      Data.Map.fromList ((\x -> (x, Language)) <$> ["#", "->", "="])),
    defs,
    Data.Map.fromList
      [
        ("Field", Data.Map.fromList [("Modular", Language)]),
        ("Nonzero", Data.Map.fromList [("Next", Language)]),
        ("Ord", Data.Map.fromList [("Char", Language), ("Int", Language), ("Modular", Language)]),
        ("Ring", Data.Map.fromList [("Int", Language), ("Modular", Language)]),
        ("Writeable", Data.Map.fromList [("Int", Language), ("Modular", Language)])])
main :: IO ()
main =
  do
    args <- getArgs
    f <-
      case args of
        [arg] ->
          case parse_input arg of
            Left e -> return e
            Right a ->
              case a of
                Check b -> checks b
                Eval b c ->
                  do
                    d <- eval'' b c
                    return
                      (case d of
                        Left e -> e
                        Right e -> e)
        _ -> return "Awful requires one command line argument."
    putStrLn f
--------------------------------------------------------------------------------------------------------------------------------