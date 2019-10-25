{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Time where

import Data.Int
import Data.Word

newtype Time
  = Time Word64
  deriving (Eq, Ord, Show, Enum)

zeroTime :: Time
zeroTime = Time 0

newtype TimeDiff
  = TimeDiff Int64
  deriving (Eq, Ord, Show, Enum, Num)

fromNanoseconds :: Int64 -> TimeDiff
fromNanoseconds t = TimeDiff t

fromMicroseconds, fromMilliseconds :: Integral a => a -> TimeDiff
fromMicroseconds t = TimeDiff (fromIntegral t * 1000)
fromMilliseconds t = TimeDiff (fromIntegral t * 1000000)

addTime :: Time -> TimeDiff -> Time
addTime (Time x) (TimeDiff y)
  | let z = fromIntegral x + y
  , z >= 0 = Time (fromIntegral z)
  | otherwise = error "addTime: negative time"

-- | A closed interval of 'Time'.
data Interval
  = Interval { intervalStart, intervalEnd :: !Time }
  deriving (Eq, Show)

data Range
  = Instant !Time
  | Interval' !Interval
  deriving (Eq, Show)

