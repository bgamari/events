{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Main where

import Data.Monoid
import GHC.RTS.Events as Event
import GHC.RTS.Events.Incremental as Event
import qualified Data.ByteString.Lazy as BSL
import Diagrams.Backend.SVG
import Diagrams
import Data.Machine
import Data.Colour
import Data.Colour.Names as Colours
import Linear.Affine

import EventExtractor
import EventLog
import Bin
import Time
import Mean

toSeconds :: Time -> Double
toSeconds (Time t) = realToFrac t / 1e9

diffToSeconds :: TimeDiff -> Double
diffToSeconds (TimeDiff t) = realToFrac t / 1e9

durationPlot :: [Interval] -> Path V2 Double
durationPlot = foldMap toDuration
  where
    toDuration :: Interval -> Path V2 Double
    toDuration x =
        translateX x0 $ rect width 1
      where
        x0 = toSeconds $ intervalStart x
        width = diffToSeconds $ intervalStart x .-. intervalEnd x

barPlot :: [(Time, Double)] -> Path V2 Double
barPlot = foldMap toBar
  where
    toBar (t, y) =
      pathFromTrailAt
        (trailFromOffsets [V2 0 y])
        (p2 (toSeconds t, 0))

linePlot :: [(Time, Double)] -> Path V2 Double
linePlot [] = mempty
linePlot xs =
  pathFromTrailAt
    (trailFromVertices $ map toPt xs)
    (toPt (head xs))
  where
    toPt (t, y) = p2 (toSeconds t, y)

heatMap :: forall b.
           (Renderable (Path V2 Double) b)
        => (Int -> Int -> AlphaColour Double)
        -> TimeDiff
        -> [Time]
        -> QDiagram b V2 Double Any
heatMap fillColor binSize xs =
  let bins :: [Bin (Sum Int)]
      bins = bin binSize (map (\t -> (t, Sum 1)) xs)

      Sum maxBin = maximum $ map binValue bins

      toBinRect :: Bin (Sum Int) -> QDiagram b V2 Double Any
      toBinRect bin =
          rect width 1 # fcA fill # lw 0
        where
          fill = fillColor maxBin $ getSum $ binValue bin
          width = diffToSeconds binSize
   in hcat $ map toBinRect bins

heatToColor :: Real a => a -> a -> AlphaColour Double
heatToColor m n = Colours.green `withOpacity` (realToFrac n / realToFrac m)

gcPausesPlot :: [GC] -> Path V2 Double
gcPausesPlot = durationPlot . map gcInterval

readIt :: IO [Event]
readIt = do
  Right (elog, _) <- Event.readEventLog <$> BSL.readFile "hi.eventlog"
  return $ Event.events $ Event.dat elog

main :: IO ()
main = do
  events <- readIt
  --print $ length $ run $ supply events garbageCollections
  --print $ take 10 heapSizeEvents
  --print $ length $ run $ supply events (capThreads $ Capability 0)
  --mapM_ print $ runIt $ runT $ supply events capThreads'
  plotIt events

plotIt :: [Event] -> IO ()
plotIt events = renderSVG "hi.svg" size dia
  where
    heapSizeEvents = runExtractor heapSizeSamples events
    heapLiveEvents = runExtractor heapLiveSamples events

    smoothBytesSamples :: [(Time, Bytes)] -> [(Time, Double)]
    smoothBytesSamples samples =
      map (\bin -> (binStart bin, Mean.getMean $ binValue bin))
      $ bin (diffFromMilliseconds 100)
        [ (t, Mean.singleton $ realToFrac y / 1e8)
        | (t, Bytes y) <- samples
        ]

    meanHeapSize = smoothBytesSamples heapSizeEvents
    meanHeapLive = smoothBytesSamples heapLiveEvents

    size = dims $ V2 600 400 :: SizeSpec V2 Double

    dia :: QDiagram SVG V2 Double Any
    dia = scaleY 3 $ scaleX 10 $ vsep 1
      [ heatMap heatToColor (diffFromMilliseconds 100) (map fst heapSizeEvents)
      , stroke (linePlot meanHeapSize) # lw 0.1 -- <> stroke (barPlot meanHeapSize)
      , stroke (linePlot $ fmap (fmap $ (/1e8) . realToFrac) heapLiveEvents) # lw 0.1 # lc Colours.purple
      , stroke (gcPausesPlot
          [ gc
          | gc <- runExtractor garbageCollections events
          , gcGeneration gc == 1 ]) # fc Colours.orange # lw 0
      , stroke (durationPlot $ runExtractor nonmovingMarkIntervals events) # fc Colours.blue # lw 0
      , stroke (durationPlot $ runExtractor nonmovingSweepIntervals events) # fc Colours.grey # lw 0
      ]

