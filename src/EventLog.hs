{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE RecordWildCards #-}

module EventLog where

import Foreign.Storable
import Data.Foldable
import Data.Word
import Data.Int
import Data.Vector.Unboxed.Deriving
import qualified Data.Vector.Unboxed as VU
import qualified Data.Vector.Unboxed.Mutable as VUM
import qualified Data.Vector.Generic as VG
import qualified Data.Vector.Generic.Mutable as VGM

import Data.Functor.Identity
import qualified Data.Map.Strict as M
import Control.Monad
import Control.Monad.Trans.Class
import Control.Monad.Trans.State.Strict
import Data.Machine
import GHC.RTS.Events hiding (ThreadId)
import qualified GHC.RTS.Events as Events

import Time
import Mean
import KeyedVector
import EventExtractor

data GenEvent t a
  = PointEvent !t !a
  | IntervalEvent !t !a

eventTime :: Event -> Time
eventTime = Time.fromNanoseconds . fromIntegral . evTime

newtype Bytes
  = Bytes { getBytes :: Word64 }
  deriving stock (Eq, Ord, Show)
  deriving newtype (Num, Integral, Enum, Real)

newtype PointEventList v a
  = PointEventList (KeyedVector v Time a)

newtype IntervalEventList v a
  = IntervalEventList (KeyedVector v Time a)

data GC
  = GC { gcInterval :: !Interval
       , gcGeneration :: !Int
       }
  deriving (Show)

garbageCollections :: EventExtractor GC
garbageCollections =
  startEndIntervals'
    (\case StartGC -> Just (); _ -> Nothing)
    (\case GCStatsGHC{..} -> Just gen; _ -> Nothing)
    (\gcInterval _ gcGeneration -> GC{..})

data NonMovingGC
  = NonMovingGC { nmgcMarkIntervals :: [Interval]
                , nmgcSyncInterval  :: Interval
                , nmgcSweepInterval :: Interval
                }

nonmovingMarkIntervals :: EventExtractor Interval
nonmovingMarkIntervals =
    startEndIntervals
      (\case ConcMarkBegin -> True; _ -> False)
      (\case ConcMarkEnd _ -> True; _ -> False)

nonmovingSweepIntervals :: EventExtractor Interval
nonmovingSweepIntervals =
    startEndIntervals
      (\case ConcSweepBegin -> True; _ -> False)
      (\case ConcSweepEnd -> True; _ -> False)

nonmovingSyncIntervals :: EventExtractor Interval
nonmovingSyncIntervals =
    startEndIntervals
      (\case ConcSyncBegin -> True; _ -> False)
      (\case ConcSyncEnd -> True; _ -> False)

nonmovingUpdRemSetFlushes :: EventExtractor Time
nonmovingUpdRemSetFlushes =
  sampleTimes (\case ConcUpdRemSetFlush{} -> True; _ -> False)

heapSizeSamples :: EventExtractor (Time, Bytes)
heapSizeSamples =
  sampleTimes' (\case HeapSize{sizeBytes=n} -> Just (Bytes n); _ -> Nothing)

heapLiveSamples :: EventExtractor (Time, Bytes)
heapLiveSamples =
  sampleTimes' (\case HeapLive{liveBytes=n} -> Just (Bytes n); _ -> Nothing)

newtype Capability
  = Capability { getCapability :: Int }
  deriving (Eq, Ord, Show, Storable)

derivingUnbox "Capability" [t| Capability -> Int |] [| getCapability |] [| Capability |]

newtype ThreadId
  = ThreadId { getThreadId :: Events.ThreadId }
  deriving (Eq, Ord, Show)

derivingUnbox "ThreadId" [t| ThreadId -> Events.ThreadId |] [| getThreadId |] [| ThreadId |]

capThreads :: Capability -> EventExtractor (Interval, ThreadId)
capThreads cap = repeatedly $ do
  (t0, tid) <- untilJust $ \case
    ev@Event{evSpec=RunThread tid, evCap=Just cap'}
      | Capability cap' == cap -> Just (eventTime ev, ThreadId tid)
    _ -> Nothing
  t1 <- untilJust $ \case
    ev@Event{evSpec=StopThread _ _, evCap=Just cap'}
      | Capability cap' == cap -> Just (eventTime ev)
    _ -> Nothing
  yield (Interval t0 t1, tid)

runItM :: (Monad m, Monoid s) => StateT s m a -> m a
runItM m = evalStateT m mempty

runIt :: Monoid s => State s a -> a
runIt = runIdentity . runItM

capThreads' :: EventExtractorT (State (M.Map Capability (Time, ThreadId))) (Interval, Capability, ThreadId)
capThreads' = repeatedly $ do
  (cap, t0, tid) <- untilJust $ \case
    ev@Event{evSpec=RunThread tid, evCap=Just cap}
      -> Just (Capability cap, eventTime ev, ThreadId tid)
    _ -> Nothing
  s <- lift get
  case M.lookup cap s of
    Just (t1, tid) -> yield (Interval t0 t1, cap, tid)
    Nothing -> return ()

  lift $ modify' $ M.insert cap (t0, tid)
