{-# LANGUAGE BangPatterns        #-}
{-# LANGUAGE RecordWildCards     #-}

module Data.CuckooFilter where

import           Control.Monad.Primitive
import           Control.Monad.ST
import           Data.Bits
import           Data.Bytes.Serial
import           Data.Hashabler (Hashable)
import qualified Data.Primitive.MutVar as PV
import           Data.Vector.Unboxed (Vector)
import qualified Data.Vector.Unboxed as V
import qualified Data.Vector.Unboxed.Mutable as MV
import           Data.Word

import           Prelude hiding (elem, notElem)

import           Data.CuckooFilter.Base
import qualified Data.CuckooFilter.Mutable as CM

data Cuckoo a = Cuckoo {
      vector  :: !(Vector Bucket)
    , size    :: !Word
    , victim  :: !(Maybe (Fingerprint, Index))
    }

instance CuckooHash Cuckoo where
    buckets = V.length . vector

freeze :: PrimMonad m => CM.MCuckoo (PrimState m) a -> m (Cuckoo a)
freeze CM.MCuckoo {..} = Cuckoo <$> V.freeze mcvector
                                <*> PV.readMutVar mcsize
                                <*> PV.readMutVar mcvictim

thaw :: PrimMonad m => Cuckoo a -> m (CM.MCuckoo (PrimState m) a)
thaw Cuckoo {..} = CM.MCuckoo <$> V.thaw vector
                              <*> PV.newMutVar size
                              <*> PV.newMutVar victim

unsafeFreeze :: PrimMonad m => CM.MCuckoo (PrimState m) a -> m (Cuckoo a)
unsafeFreeze CM.MCuckoo {..} = Cuckoo <$> V.unsafeFreeze mcvector
                                      <*> PV.readMutVar mcsize
                                      <*> PV.readMutVar mcvictim

fromList :: Hashable a => Int -> [a] -> (Cuckoo a, [a])
fromList n as = runST $ do
                  (mc,as') <- CM.fromList n as
                  cf       <- unsafeFreeze mc
                  return (cf, as')

fromList' :: Hashable a => Int -> [a] -> Cuckoo a
fromList' n = fst . fromList n

elem :: Hashable a => a -> Cuckoo a -> Bool
elem a cf@Cuckoo {..} = (f `inBucket` V.unsafeIndex vector h1)
                        || (f `inBucket` V.unsafeIndex vector h2)
                        || (victim == Just (f, h1))
                        || (victim == Just (f, h2))
    where !(f, h1, h2) = indexing cf a

notElem :: Hashable a => a -> Cuckoo a -> Bool
notElem a cf = not $ elem a cf
