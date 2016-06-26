{-# LANGUAGE DeriveFunctor, LambdaCase, DeriveGeneric #-}

import qualified Control.Monad.Random as Rand
import qualified Data.Map as Map
import qualified Data.Sequence as Seq
import qualified Control.Monad.Free as Free
import Control.Monad (join)
import Control.Arrow
import Data.Ord (comparing, Down(..))
import Data.List (maximumBy, sortBy)
import Data.Foldable (toList)

import Distribution
import qualified DNA
import Pool

data NumberGF a
    = Zero
    | One
    | Plus a a
    deriving (Eq, Ord, Functor, Show)

type NumberF = Free.Free NumberGF

simConfig :: SimConfig NumberF
simConfig = SimConfig {
    codeSize = 26,
    fDist = numberFDist
}

unfreeAlg :: (Functor f) => DNA.Algebra f a -> DNA.Algebra (Free.Free f) a
unfreeAlg alg = Free.iter alg

iterConfig :: IterConfig NumberF
iterConfig = IterConfig {
    poolSize = 1000,
    oldGuardSize = 50,
    childrenSize = 1500,
    cestpoolSize = 50,
    metric = Metric (\dna -> DNA.birth integerDepthAlg dna 0),
    mutationOdds = 200
}

integerDepthAlg :: DNA.Algebra NumberF (Int -> Integer)
integerDepthAlg = unfreeAlg $ DNA.depthAlg 10 0 (\case
    Zero -> 0
    One -> 1
    Plus x y -> x + y)

numberFDist :: (Distribution d) => DNA.FDist d NumberF
numberFDist aDist = Free.liftF <$> join (uniform [
        pure Zero,
        pure One,
        Plus <$> aDist <*> aDist
    ])



report :: Pool NumberF -> IO ()
report pool = do
    let inst = fmap (flip (DNA.birth integerDepthAlg) 0 &&& id) (getPool pool)
    putStrLn "------------"
    putStrLn $ "pool size: " ++ show (Seq.length inst)
    putStrLn $ "average: " ++ 
        show (fromIntegral (sum (fmap fst inst)) / fromIntegral (Seq.length inst))
    let best = maximumBy (comparing fst) inst
    putStrLn $ "best: " ++ show (fst best)
    putStrLn . unlines $ DNA.showDNALines (snd best)

mainIter :: Pool NumberF -> IO a
mainIter pool = do
    report pool
    mainIter =<< Rand.evalRandIO (iterPool simConfig iterConfig pool)

main = mainIter =<< Rand.evalRandIO (initialPool simConfig 1000)
