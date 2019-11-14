--------------------------------------------------------------------------------------------------------------------------------
{-# OPTIONS_GHC -Wall #-}
module Transf (Transf (..), get_state, transf_list) where
  import Data.Bifunctor
  newtype Transf t u f v = Transf {transf :: t -> u -> f (u, v)}
  instance Monad f => Applicative (Transf t u f) where
    Transf f <*> Transf g = Transf (\x -> \y -> f x y >>= \(z, h) -> second h <$> g x z)
    pure x = Transf (return (\y -> return (y, x)))
  instance Functor f => Functor (Transf t u f) where
    fmap f (Transf g) = Transf (\x -> \y -> second f <$> g x y)
  instance Monad f => Monad (Transf t u f) where
    Transf f >>= g = Transf (\x -> \y -> f x y >>= \(z, a) -> transf (g a) x z)
  get_state :: Applicative f => Transf t u f u
  get_state = Transf (return (\x -> pure (x, x)))
  transf_list :: Monad f => (t -> u -> v -> f (v, w)) -> [t] -> u -> v -> f (v, [w])
  transf_list f x = transf (foldr (\y -> \transf_list_w -> (:) <$> Transf (f y) <*> transf_list_w) (return []) x)
--------------------------------------------------------------------------------------------------------------------------------