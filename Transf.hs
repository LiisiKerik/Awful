--------------------------------------------------------------------------------------------------------------------------------
{-# OPTIONS_GHC -Wall #-}
{-# LANGUAGE FlexibleInstances, MultiParamTypeClasses #-}
module Transf (PairT (..), RST, SPT, SWT, evalSPT, evalSWT, runRST, runSPT) where
  import Control.Applicative (Alternative (..))
  import Control.Monad (MonadPlus (..), join)
  import Control.Monad.Trans.Reader (ReaderT, runReaderT)
  import Control.Monad.Trans.State.Strict (StateT, runStateT)
  import Control.Monad.Trans.Writer.Strict (WriterT, runWriterT)
  import Control.Monad.Writer.Strict (MonadWriter (..))
  import Data.Bifunctor (second)
  newtype PairT output f t = PairT {runPairT :: (output, f t)}
      deriving Show
  type RST env state f = ReaderT env (StateT state f)
  type SPT state output f = StateT state (PairT output f)
  type SWT state output f = StateT state (WriterT output f)
  instance (Alternative f, Monoid output) => Alternative (PairT output f) where
    PairT (output_0, x) <|> PairT (output_1, y) = PairT (output_0 <> output_1, x <|> y)
    empty = PairT (mempty, empty)
  instance (Applicative f, Monoid output) => Applicative (PairT output f) where
    PairT (output_0, x) <*> PairT (output_1, y) = PairT (output_0 <> output_1, x <*> y)
    pure x = PairT (mempty, pure x)
  instance Functor f => Functor (PairT output f) where
    fmap f (PairT (output, x)) = PairT (output, f <$> x)
  instance (Monad f, Monoid output, Traversable f) => Monad (PairT output f) where
    PairT (output, x) >>= f =
      let
        PairT (output', y) = traverse f x
      in
        PairT (output <> output', join y)
  instance (MonadPlus f, Monoid output, Traversable f) => MonadPlus (PairT output f)
  instance (Monad f, Monoid output, Traversable f) => MonadWriter output (PairT output f) where
    listen (PairT (output, x)) = PairT (output, (\y -> (y, output)) <$> x)
    pass (PairT (output, x)) = PairT (foldr ($) output (snd <$> x), fst <$> x)
    tell output = PairT (output, return ())
  evalSPT :: Functor f => SPT state output f t -> state -> (output, f t)
  evalSPT f state = second (fmap fst) (runSPT f state)
  evalSWT :: Functor f => SWT state output f t -> state -> f (t, output)
  evalSWT f state = (\((x, _), output) -> (x, output)) <$> runWriterT (runStateT f state)
  runRST :: RST env state f t -> env -> state -> f (t, state)
  runRST f env = runStateT (runReaderT f env)
  runSPT :: SPT state output f t -> state -> (output, f (t, state))
  runSPT f state = runPairT (runStateT f state)
--------------------------------------------------------------------------------------------------------------------------------