{-# LANGUAGE RankNTypes, LambdaCase, TypeSynonymInstances, FlexibleInstances #-}

module DNA where

import qualified Data.Map as Map
import qualified Control.Monad.Random as Rand
import Control.Applicative

-- A `ZAlgebra` is an algebra with a 'zero' element (`alg Nothing`).
type ZAlgebra f a = Maybe (f a) -> a

-- `DNA s f` is a symbol table over the algebra `f`, with a distinguished start symbol.
type Table s f = Map.Map s (f s)
data DNA s f = DNA (Table s f) s

fromTable :: (Functor f, Ord s) => ZAlgebra f a -> Table s f -> s -> a
fromTable alg table s = alg ((fmap.fmap) (fromTable alg table) (Map.lookup s table))

birth :: (Functor f, Ord s) => ZAlgebra f a -> DNA s f -> a
birth alg (DNA table s0) = fromTable alg table s0


class (Applicative d) => Distribution d where
    pick :: a -> a -> d a

instance (Rand.RandomGen g) => Distribution (Rand.Rand g) where
    pick x y = fmap (\case False -> x; True -> y) Rand.getRandom

-- `Dist a` is a probability distribution of `a`s.
type Dist = Rand.Rand Rand.StdGen
type FDist d f = forall a. d a -> d (f a)

replicateA :: (Applicative f) => Int -> f a -> f [a]
replicateA z = sequenceA . replicate z

newDNA :: (Ord s, Distribution d) => Int -> d s -> FDist d f -> d (DNA s f)
newDNA size symDist fDist = 
    DNA <$> (Map.fromList <$> replicateA size ((,) <$> symDist <*> fDist symDist)) <*> symDist

combineDNA :: (Ord s, Distribution d) => DNA s f -> DNA s f -> d (DNA s f)
combineDNA (DNA table1 s1) (DNA table2 s2) = do
    DNA <$> (Map.unions <$> sequenceA [
                sequenceA (Map.intersectionWith pick table1 table2),
                pure table1,
                pure table2])
        <*> pick s1 s2
