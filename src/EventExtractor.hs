{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE RecordWildCards #-}

module EventExtractor
  ( EventExtractor, EventExtractorT
  , runExtractor
    -- * High-level patterns
  , sampleTimes
  , sampleTimes'
  , startEndIntervals
  , startEndIntervals'
    -- * Low-level primitives
  , untilJust
  , yieldJust
    -- * Utilities
  , eventTime
  ) where

import Data.Foldable
import Data.Functor.Identity
import qualified Data.Map.Strict as M
import Control.Monad
import Data.Machine
import GHC.RTS.Events hiding (ThreadId)
import qualified GHC.RTS.Events as Events

import Time

eventTime :: Event -> Time
eventTime = Time.fromNanoseconds . fromIntegral . evTime

type EventExtractor a = EventExtractorT Identity a
type EventExtractorT m a = ProcessT m Event a

runExtractor :: Foldable f => EventExtractor a -> f Event -> [a]
runExtractor extractor events = run $ supply events extractor

untilJust :: (a -> Maybe b) -> Plan (Is a) o b
untilJust f = loop
  where
    loop = do
      x <- await
      case f x of
        Just y  -> return y
        Nothing -> loop

yieldJust :: (a -> Maybe b) -> Plan (Is a) b ()
yieldJust f = do
    x <- untilJust f
    yield $! x

sampleTimes'
  :: (EventInfo -> Maybe a)
  -> EventExtractor (Time, a)
sampleTimes' isInteresting = repeatedly $ do
    yieldJust $ \ev -> fmap (\x -> (eventTime ev, x)) (isInteresting $ evSpec ev)

sampleTimes
  :: (EventInfo -> Bool)
  -> EventExtractor Time
sampleTimes isInteresting = repeatedly $ do
    yieldJust $ \ev -> case isInteresting (evSpec ev) of
                         True -> Just $ eventTime ev
                         False -> Nothing

startEndIntervals
  :: (EventInfo -> Bool)  -- ^ start predicate
  -> (EventInfo -> Bool)  -- ^ end predicate
  -> EventExtractor Interval
startEndIntervals isStart isEnd =
    startEndIntervals' (toMaybe  . isStart) (toMaybe . isEnd) (\interval _ _ -> interval)
  where
    toMaybe True  = Just ()
    toMaybe False = Nothing

startEndIntervals'
  :: (EventInfo -> Maybe a)  -- ^ start predicate
  -> (EventInfo -> Maybe b)  -- ^ end predicate
  -> (Interval -> a -> b -> c)
  -> EventExtractor c
startEndIntervals' isStart isEnd toEvent = repeatedly $ do
    (t0, a) <- untilJust $ withTime isStart
    (t1, b) <- untilJust $ withTime isEnd
    yield $! toEvent (Interval t0 t1) a b
  where
    withTime f ev = fmap (\x -> (eventTime ev, x)) (f $ evSpec ev)
