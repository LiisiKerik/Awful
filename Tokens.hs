--------------------------------------------------------------------------------------------------------------------------------
{-# OPTIONS_GHC -Wall #-}
{-# LANGUAGE StandaloneDeriving #-}
module Tokens (Token (..), Tokens, current_line_and_char, nom_token, Tokens.null, tokenise) where
  import Control.Monad.Except (MonadError (..))
  import Control.Monad.State.Strict (MonadState (..), StateT, runStateT)
  import Data.Bifunctor (first)
  import Data.Char (isDigit, isLetter)
  import Errors (Err', Error' (..), Line_and_char, init_line_and_char, next_char, next_line)
  data Char' =
    Delimiter_char Token |
    Invalid_char |
    Minus_char |
    Nat_char Char |
    Newline_char |
    Operator_char Char |
    Slash_char |
    Space_char |
    Tick_char |
    Tilde_char |
    Word_char Char
      deriving Show
  data Token =
    Algebraic_token |
    Blank_token |
    Branch_token |
    Check_token |
    Class_token |
    Comma_token |
    Data_token |
    Def_token |
    Eq_token |
    Eval_token |
    Instance_token |
    Import_token |
    Left_curly_token |
    Left_round_token |
    Left_square_token |
    Match_token |
    Name_token String |
    Nat_token Integer |
    Negate_token |
    Operator_token String |
    Right_curly_token |
    Right_round_token |
    Right_square_token |
    Run_token |
    Struct_token |
    Term_token |
    Test_token |
    Type_token
      deriving Show
  data Token' = Token' Line_and_char Token
      deriving Show
  type Tokeniser = StateT (Line_and_char, [Char']) Err'
  data Tokens = Tokens [Token'] Line_and_char
      deriving Show
  deriving instance Eq Token
  add_token :: Tokeniser (Token, [Token']) -> Tokeniser [Token']
  add_token tokenise'' =
    do
      line_and_char <- get_line_and_char
      (token, tokens) <- tokenise''
      return (Token' line_and_char token : tokens)
  classify_char :: Char -> Char'
  classify_char c =
    case c of
      '\n' -> Newline_char
      ' ' -> Space_char
      '(' -> Delimiter_char Left_round_token
      ')' -> Delimiter_char Right_round_token
      ',' -> Delimiter_char Comma_token
      '-' -> Minus_char
      '/' -> Slash_char
      '[' -> Delimiter_char Left_square_token
      ']' -> Delimiter_char Right_square_token
      '`' -> Tick_char
      '{' -> Delimiter_char Left_curly_token
      '}' -> Delimiter_char Right_curly_token
      '~' -> Tilde_char
      _ ->
        case (is_operator c, is_letter c, isDigit c) of
          (True, False, False) -> Operator_char c
          (False, True, False) -> Word_char c
          (False, False, True) -> Nat_char c
          _ -> Invalid_char
  current_line_and_char :: Tokens -> Line_and_char
  current_line_and_char (Tokens tokens end_line_and_char) =
    case tokens of
      [] -> end_line_and_char
      Token' line_and_char _ : _ -> line_and_char
  end_tokens :: Tokeniser [Token']
  end_tokens = return []
  gather_token :: (Char' -> Maybe Char) -> (String -> Token) -> Tokeniser [Token']
  gather_token token_char string_to_token = add_token (first string_to_token <$> gather_token' token_char)
  gather_token' :: (Char' -> Maybe Char) -> Tokeniser (String, [Token'])
  gather_token' token_char =
    do
      maybe_char <- get_char 0
      case maybe_char >>= token_char of
        Nothing -> (,) "" <$> tokenise'
        Just c ->
          do
            Tokens.next_char
            (token, tokens) <- gather_token' token_char
            return (c : token, tokens)
  get_char :: Integer -> Tokeniser (Maybe Char')
  get_char i = index i <$> get_text
  get_line_and_char :: Tokeniser Line_and_char
  get_line_and_char =
    do
      (line_and_char, _) <- get
      return line_and_char
  get_text :: Tokeniser [Char']
  get_text =
    do
      (_, text) <- get
      return text
  index :: Integer -> [t] -> Maybe t
  index i x =
    case x of
      [] -> Nothing
      y : x' ->
        case i of
          0 -> Just y
          _ -> index (i - 1) x'
  is_letter :: Char -> Bool
  is_letter c = elem c ['\'', '_'] || isLetter c
  is_operator :: Char -> Bool
  is_operator c = elem c ['!', '#', '$', '%', '&', '*', '+', '.', ':', ';', '<', '=', '>', '?', '@', '\\', '^', '|']
  nat_char :: Char' -> Maybe Char
  nat_char char =
    case char of
      Nat_char c -> Just c
      _ -> Nothing
  nat_token :: String -> Token
  nat_token i = Nat_token (read i)
  next_char :: Tokeniser ()
  next_char =
    do
      (line_and_char, text) <- get
      case text of
        [] -> undefined
        char : text' ->
          put
            (
              case char of
                Newline_char -> next_line line_and_char
                _ -> Errors.next_char line_and_char,
              text')
  nom_token :: (Token -> Maybe t) -> Tokens -> Maybe (t, Tokens)
  nom_token f (Tokens tokens end_line_and_char) =
    case tokens of
      [] -> Nothing
      Token' _ token : tokens' ->
        do
          x <- f token
          Just (x, Tokens tokens' end_line_and_char)
  null :: Tokens -> Bool
  null (Tokens tokens _) = Prelude.null tokens
  operator_char :: Char' -> Maybe Char
  operator_char char' =
    case char' of
      Operator_char char -> Just char
      Minus_char -> Just '-'
      Slash_char -> Just '/'
      Tilde_char -> Just '~'
      _ -> Nothing
  operator_token :: String -> Token
  operator_token operator =
    case operator of
      "=" -> Eq_token
      _ -> Operator_token operator
  throw_error :: (Line_and_char -> Error') -> Tokeniser t
  throw_error err =
    do
      line_and_char <- get_line_and_char
      throwError (err line_and_char)
  tokenise :: String -> Err' Tokens
  tokenise text =
    do
      (tokens, (line_and_char, _)) <- runStateT tokenise' (init_line_and_char, classify_char <$> text)
      Right (Tokens tokens line_and_char)
  tokenise' :: Tokeniser [Token']
  tokenise' =
    do
      maybe_char <- get_char 0
      case maybe_char of
        Nothing -> end_tokens
        Just char ->
          case char of
            Delimiter_char token ->
              add_token
                (do
                  Tokens.next_char
                  tokens <- tokenise'
                  return (token, tokens))
            Invalid_char -> throw_error Invalid_character
            Minus_char ->
              do
                maybe_char' <- get_char 1
                case maybe_char' >>= nat_char of
                  Nothing -> tokenise_operator
                  Just c ->
                    case c of
                      '0' -> throw_error Negation_of_int_starting_with_zero
                      _ ->
                        add_token
                          (do
                            Tokens.next_char
                            tokens <- tokenise_nat
                            return (Negate_token, tokens))
            Nat_char c ->
              do
                maybe_char' <- get_char 1
                case (c, maybe_char' >>= nat_char) of
                  ('0', Just _) -> throw_error Int_starting_with_zero
                  _ -> tokenise_nat
            Newline_char ->
              do
                Tokens.next_char
                tokenise'
            Operator_char _ -> tokenise_operator
            Slash_char -> tokenise_operator
            Space_char ->
              do
                Tokens.next_char
                tokenise'
            Tick_char ->
              do
                Tokens.next_char
                tokenise_single_line_comment
            Tilde_char ->
              do
                maybe_char' <- get_char 1
                maybe_char'' <- get_char 2
                case (maybe_char', maybe_char'' >>= operator_char) of
                  (Just Slash_char, Nothing) ->
                    do
                      Tokens.next_char
                      Tokens.next_char
                      tokenise_multiline_comment
                  _ -> tokenise_operator
            Word_char _ -> tokenise_word
  tokenise_multiline_comment :: Tokeniser [Token']
  tokenise_multiline_comment =
    do
      maybe_char <- get_char 0
      case maybe_char of
        Nothing -> throwError Missing_end_comment
        Just char ->
          do
            Tokens.next_char
            case char of
              Slash_char -> tokenise_multiline_comment_slash
              _ ->
                case operator_char char of
                  Nothing -> tokenise_multiline_comment
                  Just _ -> tokenise_multiline_comment_operator
  tokenise_multiline_comment_operator :: Tokeniser [Token']
  tokenise_multiline_comment_operator =
    do
      maybe_char <- get_char 0
      case maybe_char >>= operator_char of
        Nothing -> tokenise_multiline_comment
        Just _ ->
          do
            Tokens.next_char
            tokenise_multiline_comment_operator
  tokenise_multiline_comment_slash :: Tokeniser [Token']
  tokenise_multiline_comment_slash =
    do
      maybe_char <- get_char 0
      case maybe_char >>= operator_char of
        Nothing -> tokenise_multiline_comment
        Just c ->
          do
            Tokens.next_char
            case c of
              '~' -> tokenise_multiline_comment_slash_tilde
              _ -> tokenise_multiline_comment_operator
  tokenise_multiline_comment_slash_tilde :: Tokeniser [Token']
  tokenise_multiline_comment_slash_tilde =
    do
      maybe_char <- get_char 0
      case maybe_char >>= operator_char of
        Nothing -> tokenise'
        Just _ -> tokenise_multiline_comment_operator
  tokenise_nat :: Tokeniser [Token']
  tokenise_nat = gather_token nat_char nat_token
  tokenise_operator :: Tokeniser [Token']
  tokenise_operator = gather_token operator_char operator_token
  tokenise_single_line_comment :: Tokeniser [Token']
  tokenise_single_line_comment =
    do
      maybe_char <- get_char 0
      case maybe_char of
        Nothing -> end_tokens
        Just char ->
          do
            Tokens.next_char
            case char of
              Newline_char -> tokenise'
              _ -> tokenise_single_line_comment
  tokenise_word :: Tokeniser [Token']
  tokenise_word = gather_token word_char word_token
  word_char :: Char' -> Maybe Char
  word_char char' =
    case char' of
      Nat_char char -> Just char
      Word_char char -> Just char
      _ -> Nothing
  word_token :: String -> Token
  word_token word =
    case word of
      "_" -> Blank_token
      "Algebraic" -> Algebraic_token
      "Branch" -> Branch_token
      "Check" -> Check_token
      "Class" -> Class_token
      "Data" -> Data_token
      "Def" -> Def_token
      "Eval" -> Eval_token
      "Import" -> Import_token
      "Instance" -> Instance_token
      "Match" -> Match_token
      "Run" -> Run_token
      "Struct" -> Struct_token
      "Term" -> Term_token
      "Test" -> Test_token
      "Type" -> Type_token
      _ -> Name_token word
--------------------------------------------------------------------------------------------------------------------------------