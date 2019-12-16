--------------------------------------------------------------------------------------------------------------------------------
{-# OPTIONS_GHC -Wall #-}
module Transf (Transf, f_transf, get_state, read_context, return_result, run, set_state) where
  import Data.Bifunctor
  newtype Transf t u f v = Transf {transf :: t -> u -> f (u, v)}
  instance Monad f => Applicative (Transf t u f) where
    Transf f <*> Transf g = Transf (\x -> \y -> f x y >>= \(z, h) -> second h <$> g x z)
    pure x = Transf (\_ -> \y -> return (y, x))
  instance Functor f => Functor (Transf t u f) where
    fmap f (Transf g) = Transf (\x -> \y -> second f <$> g x y)
  instance Monad f => Monad (Transf t u f) where
    Transf f >>= g = Transf (\x -> \y -> f x y >>= \(z, a) -> transf (g a) x z)
  f_transf :: (f (u, v) -> f' (u, v') -> f'' (u, v'')) -> Transf t u f v -> Transf t u f' v' -> Transf t u f'' v''
  f_transf f (Transf g) (Transf h) = Transf (\x -> \y -> f (g x y) (h x y))
  get_state :: Applicative f => Transf t u f u
  get_state = Transf (\_ -> \x -> pure (x, x))
  read_context :: Applicative f => Transf t u f t
  read_context = Transf (\x -> \y -> pure (y, x))
  return_result :: f (t, u) -> Transf v t f u
  return_result x = Transf (\_ -> \_ -> x)
  run :: Functor f => Transf t u f v -> t -> u -> f v
  run (Transf f) x y = snd <$> f x y
  set_state :: Applicative f => t -> Transf u t f ()
  set_state x = Transf (\_ -> \_ -> pure (x, ()))
--------------------------------------------------------------------------------------------------------------------------------