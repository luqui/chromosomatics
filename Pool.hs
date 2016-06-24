{-# LANGUAGE RankNTypes, GeneralizedNewtypeDeriving, ExistentialQuantification, StandaloneDeriving #-}

module Pool where

import qualified Data.Sequence as Seq
import Control.Arrow
import Data.Ord (comparing, Down(..))
import Data.Monoid ((<>))

import DNA

newtype Pool s f = Pool { getPool :: Seq.Seq (DNA s f) }
    deriving (Monoid)

data SimConfig s f = SimConfig {
    codeSize :: Int,
    symbolDist :: forall d. (Distribution d) => d s,
    fDist :: forall d. (Distribution d) => FDist d f
}

initialPool :: (Ord s, Distribution d) => SimConfig s f -> Int -> d (Pool s f)
initialPool conf size = 
    Pool <$> Seq.replicateA size (newDNA (codeSize conf) (symbolDist conf) (fDist conf))

interbreedPool :: (Ord s, Distribution d) => Int -> Pool s f -> d (Pool s f)
interbreedPool poolSize pool = Pool <$> Seq.replicateA poolSize (hookup (getPool pool))


data Metric a = forall b. Ord b => Metric (a -> b)

data IterConfig s f = IterConfig {
    poolSize :: Int,
    oldGuardSize :: Int,
    childrenSize :: Int,
    cestpoolSize :: Int,
    metric :: Metric (DNA s f),
    mutationOdds :: Int  -- 1 / mutationRate
}

rank :: (Ord s) => Metric (DNA s f) -> Seq.Seq (DNA s f) -> Seq.Seq (DNA s f)
rank (Metric metric) =
    fmap (metric &&& id) >>>
    Seq.unstableSortBy (comparing (Down . fst)) >>>
    fmap snd

-- odds = 1 / mutationRate
mutatePool :: (Distribution d, Functor f) => SimConfig s f -> Int -> Pool s f -> d (Pool s f)
mutatePool conf odds pool = Pool <$> traverse mutateDNA (getPool pool)
    where
    mutateDNA (DNA table s0) = DNA <$> traverse mutateFDist table <*> mutateSymbol s0
    mutateFDist = hit (const (fDist conf (symbolDist conf)))
    mutateSymbol = hit (const (symbolDist conf))
            
    hit :: (Distribution d) => (a -> d a) -> a -> d a
    hit | odds == 0 = \f x -> return x
        | otherwise = \f x -> do
            h <- intRange odds
            if h == 0
                then f x
                else return x

iterPool :: (Ord s, Functor f, Distribution d) 
         => SimConfig s f -> IterConfig s f -> Pool s f -> d (Pool s f)
iterPool conf iterConf pool = do
    let rankedPool = rank (metric iterConf) (getPool pool)
    let oldGuard = Pool (Seq.take (oldGuardSize iterConf) rankedPool)
    children <- interbreedPool (childrenSize iterConf) pool
    cestpool <- initialPool conf (cestpoolSize iterConf)

    let newPool = Pool $ 
            Seq.take (poolSize iterConf) 
                     (rank (metric iterConf) (getPool (oldGuard <> children <> cestpool)))
    mutatePool conf (mutationOdds iterConf) newPool
    
    
