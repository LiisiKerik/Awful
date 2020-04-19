--------------------------------------------------------------------------------------------------------------------------------
{-# OPTIONS_GHC -Wall #-}
module Eval (eval, run) where
  import Control.Monad.Except (MonadError (..))
  import Control.Monad.Reader (MonadReader (..), ReaderT, runReaderT)
  import Data.Bifunctor (first)
  import Data.Foldable (traverse_)
  import Data.Map ((!), empty, lookup, singleton, withoutKeys)
  import Data.Set (Set, empty, singleton)
  import Dictionary (Dictionary) -- delete
  import Errors (Err, Err', Error (..), Error' (..), Eval_err, Evaluation_error (..))
  import Typing (Arrow (..), Term (..), Term_pattern (..), Test (..))
  type Eval = ReaderT (Dictionary Term) Maybe
  collect_variables :: Term_pattern -> Set String
  collect_variables term_pattern =
    case term_pattern of
      Blank_term_pattern -> Data.Set.empty
      Int_term_pattern _ -> Data.Set.empty
      Name_term_pattern name -> Data.Set.singleton name
  eval :: Dictionary Term -> Term -> Err String
  eval terms term = first Evaluation_error (eval' terms term)
  eval' :: Dictionary Term -> Term -> Eval_err String
  eval' terms term =
    case runReaderT (eval_term term) terms of
      Nothing -> Left Crashed
      Just term' -> write_term term'
  eval_arrow :: Arrow -> Term -> Eval Term
  eval_arrow (Arrow term_pattern term_0) term_1 =
    do
      substitutions <- eval_term_pattern term_pattern term_1
      return (replace_variables_in_term substitutions term_0)
  eval_arrows :: Term -> [Arrow] -> Eval Term
  eval_arrows term arrows =
    case arrows of
      [] -> throwError ()
      arrow : arrows' -> catchError (eval_arrow arrow term) (\() -> eval_arrows term arrows')
  eval_term :: Term -> Eval Term
  eval_term term =
    case term of
      Add_Int_term (Int_term i) (Int_term j) -> return (Int_term (i + j))
      Application_term term_0 term_1 ->
        do
          Arrow_term arrow <- eval_term term_0
          term' <- eval_term term_1
          term'' <- eval_arrow arrow term'
          eval_term term''
{-
      Compare_Int_term (Int_term int_0) (Int_term int_1) ->
        Just
          (case compare int_0 int_1 of
            LT -> LT_term
            EQ -> EQ_term
            GT -> GT_term)
-}
      Crash_term -> throwError ()
      Div_term (Int_term i) (Int_term j) ->
        case j > 0 of
          False -> throwError ()
          True -> return (Int_term (div i j))
      Match_term term' arrows ->
        do
          term'' <- eval_term term'
          term''' <- eval_arrows term'' arrows
          eval_term term'''
      Mod_term (Int_term i) (Int_term j) ->
        case j > 0 of
          False -> throwError ()
          True -> return (Int_term (mod i j))
      Name_term name ->
        do
          terms <- ask
          eval_term (terms ! name)
      Times_Int_term (Int_term i) (Int_term j) -> return (Int_term (i * j))
      _ -> return term
  eval_term_pattern :: Term_pattern -> Term -> Eval (Dictionary Term)
  eval_term_pattern term_pattern term =
    case (term_pattern, term) of
      (Blank_term_pattern, _) -> return Data.Map.empty
      (Int_term_pattern i, Int_term j) ->
        case i == j of
          False -> throwError ()
          True -> return Data.Map.empty
      (Name_term_pattern name, _) -> return (Data.Map.singleton name term)
      _ -> throwError ()
  replace_variables_in_arrow :: Dictionary Term -> Arrow -> Arrow
  replace_variables_in_arrow substitutions (Arrow term_pattern term) =
    Arrow term_pattern (replace_variables_in_term (withoutKeys substitutions (collect_variables term_pattern)) term)
  replace_variables_in_term :: Dictionary Term -> Term -> Term
  replace_variables_in_term substitutions term =
    case term of
      Add_Int_term term_0 term_1 ->
        Add_Int_term (replace_variables_in_term substitutions term_0) (replace_variables_in_term substitutions term_1)
      Application_term term_0 term_1 ->
        Application_term (replace_variables_in_term substitutions term_0) (replace_variables_in_term substitutions term_1)
      Arrow_term arrow -> Arrow_term (replace_variables_in_arrow substitutions arrow)
      -- Compare_Int_term term_0 term_1 -> Compare_Int_term (replace_variables_in_term terms term_0) (replace_variables_in_term terms term_1)
      Crash_term -> Crash_term
      Div_term term_0 term_1 ->
        Div_term (replace_variables_in_term substitutions term_0) (replace_variables_in_term substitutions term_1)
      Int_term i -> Int_term i
      Match_term term' arrows ->
        Match_term (replace_variables_in_term substitutions term') (replace_variables_in_arrow substitutions <$> arrows)
      Mod_term term_0 term_1 ->
        Mod_term (replace_variables_in_term substitutions term_0) (replace_variables_in_term substitutions term_1)
      Name_term name ->
        case Data.Map.lookup name substitutions of
          Nothing -> term
          Just term' -> term'
      Times_Int_term term_0 term_1 ->
        Times_Int_term (replace_variables_in_term substitutions term_0) (replace_variables_in_term substitutions term_1)
  run :: Dictionary Term -> [Test] -> Err' ()
  run terms = traverse_ (run' terms)
  run' :: Dictionary Term -> Test -> Err' ()
  run' terms (Test line_and_char term_0 term_1) =
    do
      (term_0', term_1') <- first (Run_error line_and_char) ((,) <$> eval' terms term_0 <*> eval' terms term_1)
      case term_0' == term_1' of
        False -> Left (Test_failed line_and_char term_0' term_1')
        True -> return ()
  write_term :: Term -> Eval_err String
  write_term term =
    case term of
      Int_term i -> Right (show i)
      _ -> Left Not_writeable
--------------------------------------------------------------------------------------------------------------------------------