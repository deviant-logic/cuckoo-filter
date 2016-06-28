{-# LANGUAGE ParallelListComp #-}


module Main where

import           Control.Monad
import           Control.DeepSeq
import           Criterion.Main
import qualified Data.BloomFilter.Easy as BF
import qualified Data.HashSet as HS
import qualified Data.Vector as V
import qualified System.Random.MWC as MWC

import qualified Data.CuckooFilter.Mutable as CM

main :: IO ()
main = do
  cfs <- forM sizes $ \n -> do
          mc <- CM.new n
          mapM_ (`CM.insert` mc) [1..n]
          return mc
  let bfs = force $ BF.easyList 0.03 . enumFromTo 1 <$> sizes
  defaultMain [
        bgroup  "inserts"  (foldMap inserts [5000, 10000, 100000, 500000])
       , bgroup "elems" $ concat [[cfes mc n [1..n], bfes bf n [1..n]]
                                           | mc <- cfs
                                           | bf <- bfs
                                           | n  <- sizes]
       ]

sizes :: [Int]
sizes = [5000, 10000, 100000, 500000]

inserts :: Int -> [Benchmark]
inserts n = [cfis n, hsis n, bfis n]

cfes mc n ns = bench ("cuckoo-" ++ show n) . nfIO $ do
                 mapM_ (`CM.elem` mc) ns

bfes bf n ns = bench ("bloomfilter-" ++ show n) . nf (fmap (`BF.elem` bf)) $ ns

cfis :: Int -> Benchmark
cfis n = bench ("cuckoo-" ++ show n) . nfIO $ do
           mc  <- CM.new n
           mapM_ (`CM.insert` mc) [1..n]

hsis :: Int -> Benchmark
hsis n = bench ("hashset-" ++ show n) . nf HS.fromList $ [1..n]

bfis :: Int -> Benchmark
bfis n = bench ("bloomfilter-" ++ show n) . nf (BF.easyList 0.03) $ [1..n]

