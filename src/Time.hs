{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE TypeFamilies #-}

module Time where

import Data.Int
import Data.Word
import Linear.Affine
import Linear.Vector

newtype Nanoseconds
  = Nanoseconds { getNanoseconds :: Int64 }
  deriving (Eq, Ord, Show, Enum, Num, Real)

type Time = Time' Nanoseconds

newtype Time' a
  = Time a
  deriving (Eq, Ord, Show, Enum, Functor)

instance Applicative Time' where
  pure = Time
  Time f <*> Time x = Time (f x)

instance Affine Time' where
  type Diff Time' = TimeDiff'
  Time x .-. Time y = TimeDiff (x-y)
  Time x .+^ TimeDiff y= Time (x+y)

zeroTime :: Num a => Time' a
zeroTime = Time 0

fromNanoseconds :: Int64 -> Time
fromNanoseconds = Time . Nanoseconds

type TimeDiff = TimeDiff' Nanoseconds

newtype TimeDiff' a
  = TimeDiff a
  deriving (Eq, Ord, Show, Enum, Functor)

instance Applicative TimeDiff' where
  pure = TimeDiff
  TimeDiff f <*> TimeDiff x = TimeDiff (f x)

instance Additive TimeDiff' where
  zero = TimeDiff 0

divTimeDiff :: Fractional a => TimeDiff' a -> TimeDiff' a -> a
divTimeDiff (TimeDiff x) (TimeDiff y) = x / y

diffFromNanoseconds :: Int64 -> TimeDiff
diffFromNanoseconds = TimeDiff . Nanoseconds

diffFromMicroseconds, diffFromMilliseconds, diffFromSeconds :: Integral a => a -> TimeDiff
diffFromMicroseconds t = TimeDiff (fromIntegral t * 1000)
diffFromMilliseconds t = TimeDiff (fromIntegral t * 1000000)
diffFromSeconds      t = TimeDiff (fromIntegral t * 1000000000)

-- | A closed interval of 'Time'.
data Interval
  = Interval { intervalStart, intervalEnd :: !Time }
  deriving (Eq, Show)

data Range
  = Instant !Time
  | Interval' !Interval
  deriving (Eq, Show)

