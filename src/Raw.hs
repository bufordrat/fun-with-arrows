module Raw where

class Arrow arr where
  arr :: (a -> b) -> arr a b
  (>>>) :: arr a b -> arr b c -> arr a c
  first :: arr a b -> arr (a,c) (b,c)

instance Arrow (->) where
  arr = id
  (>>>) = flip (.)
  first f (a, c) = (f a, c)

newtype Kleisli m a b = Kleisli {runKleisli :: a -> m b}

instance Monad m => Arrow (Kleisli m) where
  arr f = Kleisli $ return . f
  (Kleisli f) >>> (Kleisli g) = Kleisli $ \x -> do
    applied <- f x
    g applied
  first (Kleisli f) = Kleisli $ \(a,c) ->
                                  do b <- f a
                                     pure (b,c)

second :: Arrow arr => arr a b -> arr (c,a) (c,b)
second f =
  let swap (x, y) = (y, x)
  in arr swap >>> first f >>> arr swap

count :: String -> String -> IO ()
count w filepath =
  runKleisli (Kleisli readFile >>>
               arr words >>> arr (filter (==w)) >>> arr length >>>
               Kleisli print) filepath

(***) :: Arrow arr => arr b c -> arr b' c' -> arr (b, b') (c, c')
f *** g = first f >>> second g

(&&&) :: Arrow arr => arr b c -> arr b c' -> arr b (c, c')
f &&& g = arr (\b -> (b, b)) >>> (f *** g)

addA :: Arrow arr => arr b Int -> arr b Int -> arr b Int
addA f g = f &&& g >>> arr (\(x, y) -> x + y)

f :: Int -> Int
f = succ

g :: Int -> Int
g = (* 100)
