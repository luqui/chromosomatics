{-# LANGUAGE RankNTypes, GeneralizedNewtypeDeriving, ExistentialQuantification, StandaloneDeriving #-}

module Pool where

import qualified Data.Sequence as Seq
import Control.Arrow
import Data.Ord (comparing, Down(..))
import Data.Monoid ((<>))

import Distribution
import qualified DNA

newtype Pool f = Pool { getPool :: Seq.Seq (DNA.DNA f) }
    deriving (Monoid)

data SimConfig f = SimConfig {
    codeSize :: Int,
    fDist :: forall d. (Distribution d) => DNA.FDist d f
}

initialPool :: (Distribution d) => SimConfig f -> Int -> d (Pool f)
initialPool conf size = 
    Pool <$> Seq.replicateA size (DNA.new (codeSize conf) (fDist conf))

interbreedPool :: (Distribution d) => Int -> Pool f -> d (Pool f)
interbreedPool poolSize pool = Pool <$> Seq.replicateA poolSize (hookup (getPool pool))


data Metric a = forall b. Ord b => Metric (a -> b)

data IterConfig f = IterConfig {
    poolSize :: Int,
    oldGuardSize :: Int,
    childrenSize :: Int,
    cestpoolSize :: Int,
    metric :: Metric (DNA.DNA f),
    mutationOdds :: Int  -- 1 / mutationRate
}

rank :: Metric (DNA.DNA f) -> Seq.Seq (DNA.DNA f) -> Seq.Seq (DNA.DNA f)
rank (Metric metric) =
    fmap (metric &&& id) >>>
    Seq.unstableSortBy (comparing (Down . fst)) >>>
    fmap snd

-- odds = 1 / mutationRate
mutatePool :: (Distribution d, Monad f) => SimConfig f -> Int -> Pool f -> d (Pool f)
mutatePool conf odds pool =
    Pool <$> traverse (\dna -> 
                DNA.mutate mutP mutP mutP (modifyP mutP (const (fDist conf (DNA.newSymbol dna)))) dna)
                (getPool pool)
    where
    mutP = 1 / fromIntegral odds

hookup :: (Distribution d) => Seq.Seq (DNA.DNA f) -> d (DNA.DNA f)
hookup dnas 
    | Seq.null dnas = error "hookup: empty sequence"
    | otherwise = do
        i <- intRange (Seq.length dnas)
        j <- intRange (Seq.length dnas)
        DNA.combine (Seq.index dnas i) (Seq.index dnas j)

iterPool :: (Monad f, Distribution d) 
         => SimConfig f -> IterConfig f -> Pool f -> d (Pool f)
iterPool conf iterConf pool = do
    let rankedPool = rank (metric iterConf) (getPool pool)
    let oldGuard = Pool (Seq.take (oldGuardSize iterConf) rankedPool)
    children <- interbreedPool (childrenSize iterConf) pool
    cestpool <- initialPool conf (cestpoolSize iterConf)

    let newPool = Pool $ 
            Seq.take (poolSize iterConf) 
                     (rank (metric iterConf) (getPool (oldGuard <> children <> cestpool)))
    mutatePool conf (mutationOdds iterConf) newPool
    
    
