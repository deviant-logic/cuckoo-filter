{-# LANGUAGE TupleSections #-}
{-# LANGUAGE BangPatterns    #-}
{-# LANGUAGE RecordWildCards #-}

module Data.CuckooFilter.Mutable where

import           Control.Monad
import           Control.Monad.Primitive
import           Data.Bits
import           Data.Primitive.MutVar
import           Data.Vector.Unboxed.Mutable (MVector)
import qualified Data.Vector.Unboxed.Mutable as V
import           Data.Word

import           Data.Hashabler (Hashable)
import qualified System.Random.MWC as MWC

import           Prelude hiding (elem, delete)

import           Data.CuckooFilter.Base

import Debug.Trace

data MCuckoo s a = MCuckoo {
      mcvector  :: !(MVector s Bucket)
    , mcsize    :: !(MutVar s Word)
    , mcvictim  :: !(MutVar s (Maybe (Fingerprint, Index)))
    }

instance CuckooHash (MCuckoo s) where
    buckets = V.length . mcvector

elem :: (PrimMonad m, Hashable a) => a -> MCuckoo (PrimState m) a -> m Bool
elem a mc@MCuckoo {..} = do
  b1 <- inBucket f <$> V.unsafeRead mcvector h1
  if b1
  then return True
  else do
    b2 <- inBucket f <$> V.unsafeRead mcvector h2
    if b2
    then return True
    else inVictim
      where !(f, h1, h2) = indexing mc a
            inVictim     = readMutVar mcvictim >>= \fidx ->
                             return ((fidx == Just (f,h1)) || (fidx == Just (f,h2)))

insert :: (PrimMonad m, Hashable a) => a -> MCuckoo (PrimState m) a -> m Bool
insert a mc@MCuckoo {..} = do
  victim <- readMutVar mcvictim
  if victim == Nothing
  then go 500 (fingerprint mc h) (priIndex mc h)
  else return False
    where !h            = hash a
          go  0 !f !idx = writeMutVar mcvictim (Just (f,idx)) *> return False
          go !n !f !idx = do
            b <- V.unsafeRead mcvector idx
            let !e = fromIntegral $ h .&. 0x03
            case insertFingerprint f b e of
              Right b'       -> modifyMutVar mcsize (1+) *> V.unsafeWrite mcvector idx b' *> return True
              Left  (b', f') -> V.unsafeWrite mcvector idx b' *> go (n - 1) f' (altIndex mc f' idx)

delete :: (PrimMonad m, Hashable a) => a -> MCuckoo (PrimState m) a -> m Bool
delete a mc@MCuckoo {..} = go h1 >>= \d ->
                           if d  then return d  else go h2 >>= \d' ->
                           if d' then return d' else goVictim
    where !(f, h1, h2) = indexing mc a
          go idx       = do
            b <- V.unsafeRead mcvector idx
            let b' = deleteFromBucket f b
            if b == b'
            then return False
            else modifyMutVar mcsize (subtract 1) *> V.unsafeWrite mcvector idx b' *> return True
          goVictim     = do
            v <- readMutVar mcvictim
            case v of
              Nothing -> return False
              Just (f', idx) -> if f == f' && ((h1 == idx) || (h2 == idx))
                               then writeMutVar mcvictim Nothing *> return True
                               else return False


new :: (PrimMonad m, Hashable a) => Int -> m (MCuckoo (PrimState m) a)
new size = MCuckoo <$> V.replicate (hp2 $ size `div` 4) 0
                   <*> newMutVar 0
                   <*> newMutVar Nothing

fromList :: (PrimMonad m, Hashable a) => Int -> [a] -> m (MCuckoo (PrimState m) a, [a])
fromList n as = do
  mc <- new n
  (_,as') <- fill mc as
  return (mc, as)

fill ::(PrimMonad m, Hashable a) => MCuckoo (PrimState m) a -> [a] -> m (Word, [a])
fill mc []     = (,[]) <$> readMutVar (mcsize mc)
fill mc (x:xs) = do
  i <- insert x mc
  if i
  then fill mc xs
  else (,xs) <$> readMutVar (mcsize mc)

thingy :: Int -> IO ()
thingy n = do
  mc  <- new n
  i   <- fromIntegral . fst <$> fill mc [1..n]
  print i
  tps <- mapM (`elem` mc) [1..n]
  let tpc = length . filter not $ tps
  fps <- mapM (`elem` mc) [i+1..2*i]
  let fpc = length . filter id $ fps
  putStrLn ("True Positives:  " ++ show tpc)
  putStrLn ("False Positives: " ++ show fpc)
  putStrLn ("Rate: " ++ show (fromIntegral fpc / fromIntegral (length fps)))
  occupancy mc >>= print

blah mc x = if i1 == altIndex mc f i2
            then return ()
            else print (x, i1, altIndex mc f i2)
    where i1 = priIndex mc x
          f  = fingerprint mc x
          i2 = altIndex mc f i1

occupancy :: PrimMonad m => MCuckoo (PrimState m) a -> m Double
occupancy MCuckoo {..} = do
  cnt <- readMutVar mcsize
  return $ fromIntegral cnt / (4.0 * (fromIntegral . V.length $ mcvector))
