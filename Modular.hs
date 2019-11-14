--------------------------------------------------------------------------------------------------------------------------------
{-# OPTIONS_GHC -Wall #-}
module Modular (Modular_0 (..), valid_modular, write_modular) where
  data Modular_0 = Modular_0 Integer Integer deriving Show
  valid_modular :: Modular_0 -> Bool
  valid_modular (Modular_0 x y) = x > y
  write_modular :: Modular_0 -> String
  write_modular (Modular_0 x y) = show y ++ " # " ++ show x
--------------------------------------------------------------------------------------------------------------------------------