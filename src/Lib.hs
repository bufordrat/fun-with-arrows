module Lib where

class Arrow arr where
  arr :: (a -> b) -> arr a b
  (>>>) :: arr a b -> arr b c -> arr a c

instance Arrow (->) where
  arr = id
  (>>>) = flip (.)

newtype Kleisli m a b = Kleisli {runKleisli :: a -> m b}

instance Monad m => Arrow (Kleisli m) where
  arr f = Kleisli (return . f)
  (Kleisli f) >>> (Kleisli g) = Kleisli $ \x -> do
    applied <- f x
    g applied
