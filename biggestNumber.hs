{-# LANGUAGE DeriveFunctor, LambdaCase #-}

import DNA
import qualified Control.Monad.Random as Rand
import qualified Data.Map as Map
import qualified Data.Sequence as Seq
import Control.Monad (join)
import Control.Arrow
import Control.Applicative
import Data.Ord (comparing)
import Data.List (sortBy, maximumBy)
import Data.Monoid ((<>))

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

type Pool = Seq.Seq (DNA Symbol NumberF)

initialPool :: (Distribution d) => Int -> d Pool
initialPool size = Seq.replicateA size (newDNA symbolSpace symbolDist numberFDist)

prunePool :: Int -> Pool -> Pool
prunePool killSize =
    fmap (flip (birth integerDepthAlg) 0 &&& id) >>>
    Seq.unstableSortBy (comparing fst) >>> 
    fmap snd >>>
    Seq.drop killSize

iter :: (Distribution d) => Int -> Int -> Int -> Pool -> d Pool
iter poolSize bufferSize newLifeSize pool = do
    pool' <- initialPool newLifeSize
    pool'' <- Seq.replicateA (poolSize+bufferSize) (hookup (pool' <> pool))
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
    let inst = fmap (flip (birth integerDepthAlg) 0 &&& id) pool
    putStrLn "------------"
    putStrLn $ "pool size: " ++ show (Seq.length inst)
    putStrLn $ "average: " ++ 
        show (fromIntegral (sum (fmap fst inst)) / fromIntegral (Seq.length inst))
    let best = maximumBy (comparing fst) inst
    putStrLn $ "best: " ++ show (fst best)
    putStrLn . unlines $ showDNALines (snd best)


mainIter :: Pool -> IO a
mainIter pool = do
    report pool
    mainIter =<< Rand.evalRandIO (iter 1000 200 0 pool)

main = mainIter =<< Rand.evalRandIO (initialPool 1000)
