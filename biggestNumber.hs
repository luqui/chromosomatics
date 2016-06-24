{-# LANGUAGE DeriveFunctor, LambdaCase, DeriveGeneric #-}

import qualified Control.Monad.Random as Rand
import qualified Data.Map as Map
import qualified Data.Sequence as Seq
import Control.Monad (join)
import Control.Arrow
import Data.Ord (comparing)
import Data.List (maximumBy)

import DNA
import Pool

data NumberF a
    = Zero
    | One
    | Plus a a
    deriving (Functor, Show)

type Symbol = Char

simConfig :: SimConfig Symbol NumberF
simConfig = SimConfig {
    codeSize = 26,
    symbolDist = uniform ['A'..'Z'],
    fDist = numberFDist
}

iterConfig :: IterConfig Symbol NumberF
iterConfig = IterConfig {
    poolSize = 1000,
    oldGuardSize = 50,
    childrenSize = 1500,
    cestpoolSize = 50,
    metric = Metric (\dna -> birth integerDepthAlg dna 0),
    mutationOdds = 200
}

integerDepthAlg :: ZAlgebra NumberF (Int -> Integer)
integerDepthAlg = depthAlg 10 (maybe 0 (\case
    Zero -> 0
    One -> 1
    Plus x y -> x + y))

numberFDist :: (Distribution d) => FDist d NumberF
numberFDist aDist = join (uniform [
        pure Zero,
        pure One,
        Plus <$> aDist <*> aDist
    ])




showDNALines :: DNA Symbol NumberF -> [String]
showDNALines (DNA table s0) = concat [
        ["START: " ++ show s0],
        map ("\t" ++) (showTableLines table)
    ]

showTableLines :: (Show a, Show b) => Map.Map a b -> [String]
showTableLines m = [ show k ++ ": \t" ++ show v | (k,v) <- Map.assocs m ]

report :: Pool Symbol NumberF -> IO ()
report pool = do
    let inst = fmap (flip (birth integerDepthAlg) 0 &&& id) (getPool pool)
    putStrLn "------------"
    putStrLn $ "pool size: " ++ show (Seq.length inst)
    putStrLn $ "average: " ++ 
        show (fromIntegral (sum (fmap fst inst)) / fromIntegral (Seq.length inst))
    let best = maximumBy (comparing fst) inst
    putStrLn $ "best: " ++ show (fst best)
    putStrLn . unlines $ showDNALines (snd best)

mainIter :: Pool Symbol NumberF -> IO a
mainIter pool = do
    report pool
    mainIter =<< Rand.evalRandIO (iterPool simConfig iterConfig pool)

main = mainIter =<< Rand.evalRandIO (initialPool simConfig 1000)
