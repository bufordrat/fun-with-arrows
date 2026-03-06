{-# LANGUAGE Arrows #-}

module Syntax where

import Control.Arrow

f :: Int -> Int
f = succ

g :: Int -> Int
g = (* 100)

fK :: Kleisli Maybe Int Int
fK = let fK' n = if even n
                 then Just (succ n)
                 else Nothing
     in Kleisli fK'

gK :: Kleisli Maybe Int Int
gK = let gK' n = if n > 0
                 then Just (n * 100)
                 else Nothing
     in Kleisli gK'

exampleArr1 :: Int -> String
exampleArr1 =
  proc i -> do
    x <- f -< i
    y <- g -< i
    returnA -< show (x + y)

exampleArr2 :: Kleisli Maybe Int String
exampleArr2 =
  proc i -> do
    x <- fK -< i
    y <- gK -< i
    returnA -< show (x + y)
