--------------------------------------------------------------------------------------------------------------------------------
{-# OPTIONS_GHC -Wall #-}
module Standard (Arrow_1 (..), Standard.Term (..), standard_term) where
  import Parser
  data Arrow_1 = Arrow_1 Parser.Term_pattern_1 Standard.Term deriving Show
  data Term =
    Application_term Standard.Term Standard.Term |
    Arrow_term Arrow_1 |
    Int_term Integer |
    Match_term Standard.Term [Arrow_1] |
    Name_term Name
      deriving Show
  standard_arrow :: Arrow -> Arrow_1
  standard_arrow (Arrow term_pattern term) = Arrow_1 term_pattern (standard_term term)
  standard_term :: Parser.Term -> Standard.Term
  standard_term term =
    case term of
      Parser.Application_term term' terms -> foldl Standard.Application_term (standard_term term') (standard_term <$> terms)
      Parser.Arrow_term arrow -> Standard.Arrow_term (standard_arrow arrow)
      Parser.Int_term i -> Standard.Int_term i
      Parser.Match_term term' arrows -> Standard.Match_term (standard_term term') (standard_arrow <$> arrows)
      Parser.Name_term name -> Standard.Name_term name
--------------------------------------------------------------------------------------------------------------------------------