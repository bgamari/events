module Bin
  ( Bin(..), binStart, binEnd
  , bin
  ) where

import Time
import Data.Foldable

foldMap' :: Monoid m => (a -> m) -> [a] -> m
foldMap' f = foldl' (\acc x -> acc `mappend` f x) mempty

data Bin a
  = Bin { binInterval :: !Interval
        , binValue :: a
        }

binStart, binEnd :: Bin a -> Time
binStart = intervalStart . binInterval
binEnd   = intervalEnd   . binInterval

bin :: Monoid a => TimeDiff -> [(Time, a)] -> [Bin a]
bin _binSize [] = []
bin binSize xs0 = go (fst (head xs0) `addTime` binSize) xs0
  where
    go !binEnd [] = []
    go binEnd xs
      = let (ys, zs) = span (\(t,_) -> t < binEnd) xs
            bin = Bin { binInterval = Interval (binEnd `addTime` negate binSize) binEnd
                      , binValue = foldMap' snd ys
                      }
         in bin : go (binEnd `addTime` binSize) zs
