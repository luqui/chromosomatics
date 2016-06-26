{-# LANGUAGE FlexibleInstances #-}

module Distribution where

import qualified Control.Monad.Random as Rand

class (Monad d) => Distribution d where
    uniform :: [a] -> d a
    biased :: Double -> d Bool
    intRange :: Int -> d Int

pick :: (Distribution d) => a -> a -> d a
pick x y = uniform [x,y]

modifyP :: (Distribution d) => Double -> (a -> d a) -> a -> d a
modifyP prob f x = do
    m <- biased prob
    if m then f x else return x

instance (Rand.RandomGen g) => Distribution (Rand.Rand g) where
    uniform = Rand.uniform
    biased p = (< p) <$> Rand.getRandomR (0,1)
    intRange n = Rand.getRandomR (0, n-1)
