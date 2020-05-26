--------------------------------------------------------------------------------------------------------------------------------
{-# OPTIONS_GHC -Wall #-}
module Eval (eval, run) where
  import Control.Monad.Except (MonadError (..))
  import Control.Monad.Reader (MonadReader (..), ReaderT, runReaderT)
  import Data.Bifunctor (first)
  import Data.Foldable (traverse_)
  import Data.Map ((!), empty, fromList, lookup, singleton, withoutKeys)
  import Data.Set (Set, empty, singleton)
  import Dictionary (Dictionary)
  import Errors (Err, Err', Error (..), Error' (..), Eval_err, Evaluation_error (..))
  import Typing (Arrow (..), Polymorphic_term (..), Term (..), Term_pattern (..), Test (..), substitute_type_names_in_term)
  type Eval = ReaderT (Dictionary Polymorphic_term) Maybe
  collect_names :: Term_pattern -> Set String
  collect_names term_pattern =
    case term_pattern of
      Blank_term_pattern -> Data.Set.empty
      Int_term_pattern _ -> Data.Set.empty
      Name_term_pattern name -> Data.Set.singleton name
  eval :: Dictionary Polymorphic_term -> Term -> Err String
  eval terms term = first Evaluation_error (eval' terms term)
  eval' :: Dictionary Polymorphic_term -> Term -> Eval_err String
  eval' term_defs term =
    case runReaderT (eval_term term) term_defs of
      Nothing -> Left Crashed
      Just term' -> write_term term'
  eval_arrow :: Arrow -> Term -> Eval (Maybe Term)
  eval_arrow (Arrow term_pattern term) term' =
    case match term_pattern term' of
      Nothing -> return Nothing
      Just substitutions -> Just <$> eval_term (substitute_names_in_term substitutions term)
  eval_arrows :: Term -> [Arrow] -> Eval Term
  eval_arrows term arrows =
    case arrows of
      [] -> throwError ()
      arrow : arrows' ->
        do
          maybe_term <- eval_arrow arrow term
          case maybe_term of
            Nothing -> eval_arrows term arrows'
            Just term' -> return term'
  eval_term :: Term -> Eval Term
  eval_term term =
    case term of
      Add_Int_term (Int_term i) (Int_term j) -> return (Int_term (i + j))
      Application_term term_0 term_1 ->
        do
          Arrow_term arrow <- eval_term term_0
          term' <- eval_term term_1
          maybe_term <- eval_arrow arrow term'
          case maybe_term of
            Nothing -> throwError ()
            Just term'' -> return term''
{-
      Compare_Int_term (Int_term int_0) (Int_term int_1) ->
        Just
          (case compare int_0 int_1 of
            LT -> LT_term
            EQ -> EQ_term
            GT -> GT_term)
-}
      Crash_term -> throwError ()
      Def_name_term name types ->
        do
          term_defs <- ask
          let Def_term type_variables term' = term_defs ! name
          eval_term (substitute_type_names_in_term (fromList (zip type_variables types)) term')
      Div_term (Int_term i) (Int_term j) ->
        do
          positive j
          return (Int_term (div i j))
      Match_term term' arrows ->
        do
          term'' <- eval_term term'
          term''' <- eval_arrows term'' arrows
          eval_term term'''
      Mod_term (Int_term i) (Int_term j) ->
        do
          positive j
          return (Int_term (mod i j))
      Times_Int_term (Int_term i) (Int_term j) -> return (Int_term (i * j))
      _ -> return term
  match :: Term_pattern -> Term -> Maybe (Dictionary Term)
  match term_pattern term =
    case (term_pattern, term) of
      (Blank_term_pattern, _) -> Just Data.Map.empty
      (Int_term_pattern i, Int_term j) ->
        case i == j of
          False -> Nothing
          True -> Just Data.Map.empty
      (Name_term_pattern name, _) -> Just (Data.Map.singleton name term)
      _ -> Nothing
  positive :: Integer -> Eval ()
  positive i =
    case 0 < i of
      False -> throwError ()
      True -> return ()
  run :: Dictionary Polymorphic_term -> [Test] -> Err' ()
  run term_defs = traverse_ (run' term_defs)
  run' :: Dictionary Polymorphic_term -> Test -> Err' ()
  run' terms (Test line_and_char term_0 term_1) =
    do
      (term_0', term_1') <- first (Run_error line_and_char) ((,) <$> eval' terms term_0 <*> eval' terms term_1)
      case term_0' == term_1' of
        False -> Left (Test_failed line_and_char term_0' term_1')
        True -> return ()
  substitute_names_in_arrow :: Dictionary Term -> Arrow -> Arrow
  substitute_names_in_arrow substitutions (Arrow term_pattern term) =
    Arrow term_pattern (substitute_names_in_term (withoutKeys substitutions (collect_names term_pattern)) term)
  substitute_names_in_term :: Dictionary Term -> Term -> Term
  substitute_names_in_term substitutions term =
    let
      f_arrow = substitute_names_in_arrow substitutions
      f_term = substitute_names_in_term substitutions
    in
      case term of
        Add_Int_term term_0 term_1 -> Add_Int_term (f_term term_0) (f_term term_1)
        Typing.Application_term term_0 term_1 -> Typing.Application_term (f_term term_0) (f_term term_1)
        Typing.Arrow_term arrow -> Typing.Arrow_term (f_arrow arrow)
        Crash_term -> Crash_term
        Def_name_term name types -> Def_name_term name types
        Div_term term_0 term_1 -> Div_term (f_term term_0) (f_term term_1)
        Typing.Int_term i -> Typing.Int_term i
        Local_name_term name ->
          case Data.Map.lookup name substitutions of
            Nothing -> Local_name_term name
            Just term' -> term'
        Typing.Match_term term' arrows -> Typing.Match_term (f_term term') (f_arrow <$> arrows)
        Mod_term term_0 term_1 -> Mod_term (f_term term_0) (f_term term_1)
        Times_Int_term term_0 term_1 -> Times_Int_term (f_term term_0) (f_term term_1)
  write_term :: Term -> Eval_err String
  write_term term =
    case term of
      Int_term i -> Right (show i)
      _ -> Left Not_writeable
--------------------------------------------------------------------------------------------------------------------------------