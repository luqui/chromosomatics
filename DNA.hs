{-# LANGUAGE RankNTypes, LambdaCase, TypeSynonymInstances, FlexibleInstances, ScopedTypeVariables #-}

module DNA (
    FDist,

    Algebra,
    depthAlg,

    Symbol, DNA,
    birth, newSymbol, new, combine, mutate,
    showDNALines
) where

import qualified Data.Vector as V
import qualified Data.Map.Strict as Map
import qualified Control.Monad.Random as Rand
import qualified Data.Sequence as Seq
import Control.Monad.Trans.Writer
import Control.Monad ((<=<))
import Data.Monoid

import Distribution


type Algebra f a = f a -> a

depthAlg :: (Functor f) => Int -> a -> Algebra f a -> Algebra f (Int -> a)
depthAlg maxDepth zero alg f depth
    | depth >= maxDepth = zero
    | otherwise = alg (fmap ($ depth + 1) f)


-- `DNA f` is a symbol table over the algebra `f`, with a distinguished start symbol.
newtype Symbol = Symbol { getSymbol :: Int }

instance Show Symbol where
    show (Symbol n) = '@' : show n

data DNA f = DNA !(V.Vector (f Symbol)) !Symbol

birth :: (Functor f) => Algebra f a -> DNA f -> a
birth alg (DNA table s0) = fromTable alg table s0
    where
    fromTable alg table s = alg (fmap (fromTable alg table) (table V.! getSymbol s))

-- `Dist a` is a probability distribution of `a`s.
type Dist = Rand.Rand Rand.StdGen
type FDist d f = forall a. d a -> d (f a)

-- Probability distribution for a symbol in the given DNA
newSymbol :: (Distribution d) => DNA f -> d Symbol
newSymbol (DNA table _) = Symbol <$> intRange (V.length table)

new :: (Distribution d) => Int -> FDist d f -> d (DNA f)
new size fDist = 
    DNA <$> V.replicateM size (fDist symDist) <*> symDist
    where
    symDist = Symbol <$> intRange size

combine :: (Distribution d) => DNA f -> DNA f -> d (DNA f)
combine (DNA table1 s1) (DNA table2 s2) = DNA <$> table' <*> pick s1 s2
    where
    table' = V.generateM (max (V.length table1) (V.length table2)) $ \i -> 
                pickJust (table1 V.!? i) (table2 V.!? i)

    pickJust (Just x) (Just y) = pick x y
    pickJust (Just x) Nothing = pure x
    pickJust Nothing (Just y) = pure y
    pickJust Nothing Nothing = error "pickJust: nothing to pick"

mutate :: (Distribution d, Functor f)
       => Double -> Double -> (f Symbol -> d (f Symbol)) -> DNA f -> d (DNA f)
mutate startP symP mutateCode dna@(DNA table s0) = 
    DNA <$> V.mapM mutateElem table
        <*> modifyP startP (const (newSymbol dna)) s0
    where
    mutateElem e = mutateCode =<< modifyP symP (\x -> (<$ x) <$> newSymbol dna) e

showDNALines :: (Show (f Symbol)) => DNA f -> [String]
showDNALines (DNA table s0) = 
    ["START " ++ show s0] ++ [ show n ++ ":\t" ++ show c | (n,c) <- zip [0..] (V.toList table) ]
