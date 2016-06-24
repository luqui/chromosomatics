{-# LANGUAGE DeriveFunctor, LambdaCase #-}

import DNA
import qualified Control.Monad.Random as Rand
import qualified Data.Map as Map
import Control.Monad (join)
import Control.Arrow
import Control.Applicative
import Data.Ord (comparing)
import Data.List (sortBy, maximumBy)

data NumberF a
    = Zero
    | One
    | Plus a a
    deriving (Functor, Show)

type Symbol = Char
symbolSpace = 26

symbolDist :: Distribution d => d Symbol
symbolDist = uniform ['A'..'Z']

maxDepth = 10

integerDepthAlg :: ZAlgebra NumberF (Int -> Integer)
integerDepthAlg = maybe (const 0) (\case
    Zero -> const 0
    One -> const 1
    Plus xf yf -> \d -> if d > maxDepth then 0 else xf (d+1) + yf (d+1))

integerAlg :: ZAlgebra NumberF Integer
integerAlg = maybe 0 (\case
    Zero -> 0
    One -> 1
    Plus x y -> x + y)

numberFDist :: (Distribution d) => FDist d NumberF
numberFDist aDist = join (uniform [
        pure Zero,
        pure One,
        Plus <$> aDist <*> aDist
    ])

type Pool = [DNA Symbol NumberF]

initialPool :: (Distribution d) => Int -> d Pool
initialPool size = replicateA size (newDNA symbolSpace symbolDist numberFDist)

hookup :: (Distribution d) => Pool -> d (DNA Symbol NumberF)
hookup dnas = join (combineDNA <$> uniform dnas <*> uniform dnas)

prunePool :: Int -> Pool -> Pool
prunePool killSize =
    map (flip (birth integerDepthAlg) 0 &&& id) >>>
    sortBy (comparing fst) >>> 
    map snd >>>
    drop killSize

iter :: (Distribution d) => Int -> Int -> Int -> Pool -> d Pool
iter poolSize bufferSize newLifeSize pool = do
    pool' <- initialPool newLifeSize
    pool'' <- replicateA (poolSize+bufferSize) (hookup (pool' ++ pool))
    return (prunePool bufferSize pool'')

showDNALines :: DNA Symbol NumberF -> [String]
showDNALines (DNA table s0) = concat [
        ["START: " ++ show s0],
        map ("\t" ++) (showTableLines table)
    ]

showTableLines :: (Show a, Show b) => Map.Map a b -> [String]
showTableLines m = [ show k ++ ": \t" ++ show v | (k,v) <- Map.assocs m ]

report :: Pool -> IO ()
report pool = do
    let inst = map (flip (birth integerDepthAlg) 0 &&& id) pool
    putStrLn "------------"
    putStrLn $ "pool size: " ++ show (length inst)
    putStrLn $ "average: " ++ show (fromIntegral (sum (map fst inst)) / fromIntegral (length inst))
    let best = maximumBy (comparing fst) inst
    putStrLn $ "best: " ++ show (fst best)
    putStrLn . unlines $ showDNALines (snd best)


mainIter :: Pool -> IO a
mainIter pool = do
    report pool
    mainIter =<< Rand.evalRandIO (iter 1000 200 0 pool)

main = mainIter =<< Rand.evalRandIO (initialPool 1000)
