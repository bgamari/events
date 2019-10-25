module Mean
  ( Mean
  , nOf
  , singleton
  , getMean
  ) where

import Data.Monoid

data Mean a
  = Mean !Int !a
  deriving (Eq, Show)

instance (Num a) => Monoid (Mean a) where
  mempty = Mean 0 0

instance (Num a) => Semigroup (Mean a) where
  Mean n acc <> Mean n' acc' = Mean (n+n') (acc+acc')

nOf :: Int -> a -> Mean a
nOf = Mean

singleton :: a -> Mean a
singleton = nOf 1

getMean :: (Real a, RealFrac b) => Mean a -> b
getMean (Mean n acc) = realToFrac acc / realToFrac n
