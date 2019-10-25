{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE FlexibleContexts #-}

module KeyedVector
  ( KeyedVector
  , IsKeyedVector
    -- * Construction
  , fromList
  , fromSortedList
    -- * Queries
  , takeUntilIncl, takeUntilExcl
  , dropUntilIncl, dropUntilExcl
  ) where

import Data.Bifunctor (bimap)
import Control.Monad.ST
import qualified Data.Vector.Unboxed as VU
import qualified Data.Vector.Generic as VG
import qualified Data.Vector.Generic.Mutable as VGM
import qualified Data.Vector.Algorithms.Search as Search
import qualified Data.Vector.Algorithms.Intro as Sort


type IsKeyedVector v i a = (Ord i, VG.Vector v (i, a))

newtype KeyedVector v i a
  = KeyedVector (v (i, a))

deriving instance Show (v (i, a)) => Show (KeyedVector v i a)

fromList :: (IsKeyedVector v i a)
         => [(i, a)]
         -> KeyedVector v i a
fromList = KeyedVector . VG.modify (Sort.sortBy compareKey) . VG.fromList

fromSortedList :: VG.Vector v (i, a)
               => [(i, a)]
               -> KeyedVector v i a
fromSortedList = KeyedVector . VG.fromList

data Inclusive = InclusiveLeft | InclusiveRight

split :: (IsKeyedVector v i a)
      => Inclusive  -- ^ Should the element at index @i@ be placed in the left
                    -- or right partition?
      -> i
      -> KeyedVector v i a
      -> (KeyedVector v i a, KeyedVector v i a)
split inclusive i0 (KeyedVector v) = runST $ do
  v' <- VG.unsafeThaw v
  i <- Search.binarySearchLBy compareKey v' (i0, undefined)
  return $ bimap KeyedVector KeyedVector $
    case inclusive of
      InclusiveLeft ->
        let l = VG.length $ VG.takeWhile (\(i,_) -> i == i0) $ VG.drop i v
        in VG.splitAt (i+l) v
      InclusiveRight ->
        VG.splitAt i v

compareKey :: Ord i => Search.Comparison (i, a)
compareKey a b = compare (fst a) (fst b)

-- | Take the prefix up to (but excluding) the given index.
takeUntilIncl
  :: (IsKeyedVector v i a)
  => i
  -> KeyedVector v i a
  -> KeyedVector v i a
takeUntilIncl i = fst . split InclusiveLeft i
--
-- | Take the prefix up to (but excluding) the given index.
takeUntilExcl
  :: (IsKeyedVector v i a)
  => i
  -> KeyedVector v i a
  -> KeyedVector v i a
takeUntilExcl i = fst . split InclusiveRight i

-- | Drop the prefix up to (and including) the given index.
dropUntilIncl
  :: (IsKeyedVector v i a)
  => i
  -> KeyedVector v i a
  -> KeyedVector v i a
dropUntilIncl i = snd . split InclusiveLeft i

-- | Drop the prefix up to (but excluding) the given index.
dropUntilExcl
  :: (IsKeyedVector v i a)
  => i
  -> KeyedVector v i a
  -> KeyedVector v i a
dropUntilExcl i = snd . split InclusiveRight i

testA :: KeyedVector VU.Vector Int Int
testA = fromList [(0,0), (1,1), (1,2), (4,4), (6,6)]

