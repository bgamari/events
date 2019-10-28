{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Main where

import Data.Monoid
import Data.Foldable
import Data.Semigroup
import GHC.RTS.Events as Event
import GHC.RTS.Events.Incremental as Event
import qualified Data.ByteString.Lazy as BSL
import Diagrams.Backend.SVG
import qualified Diagrams.TwoD.Text as Diagrams
import Diagrams
import Data.Machine hiding (fold)
import Data.Coerce
import Data.Colour
import Data.Colour.Names as Colours
import Linear.Affine
import Linear

import EventExtractor
import EventLog
import Bin
import Time
import Mean

toSeconds :: Time -> Double
toSeconds (Time t) = realToFrac t / 1e9

diffToSeconds :: TimeDiff -> Double
diffToSeconds (TimeDiff t) = realToFrac t / 1e9

timeAxis :: (Renderable (Diagrams.Text Double) b, Renderable (Path V2 Double) b)
         => Interval
         -> TimeDiff
         -> QDiagram b V2 Double Any
timeAxis int@(Interval t0 t1) dt =
    stroke axis <> timeTicks int dt
  where
    axis =
      pathFromTrailAt
        (trailFromVertices [p2 (toSeconds t0,0), p2 (toSeconds t1,0)])
        (p2 (toSeconds t0, 0))

timeTicks :: (Renderable (Diagrams.Text Double) b, Renderable (Path V2 Double) b)
          => Interval -> TimeDiff
          -> QDiagram b V2 Double Any
timeTicks (Interval t0 t1) dt =
  timeTicks'
    [ (t, label)
    | t <- [t0,t0 .+^ dt..t1]
    , let Time (Nanoseconds t') = t
    , let label = show $ fmap realToFrac (t .-. zeroTime) `divTimeDiff` fmap realToFrac (diffFromMilliseconds 1000)
    ]

timeTicks' :: (Renderable (Diagrams.Text Double) b, Renderable (Path V2 Double) b)
           => [(Time, String)]
           -> QDiagram b V2 Double Any
timeTicks' = foldMap tick
  where
    tick (t,label) =
      translateX (toSeconds t) $
      vrule 0.5 === (strutY 0.5 <> text label)

eventPlot :: [Time] -> Path V2 Double
eventPlot = foldMap toEvent
  where
    toEvent x = translateX (toSeconds x) (circle 0.1 # centerXY)

durationPlot :: [Interval] -> Path V2 Double
durationPlot = foldMap toDuration
  where
    toDuration :: Interval -> Path V2 Double
    toDuration x =
        translateX x0 $ alignL $ rect width 1
      where
        x0 = toSeconds $ intervalStart x
        width = diffToSeconds $ intervalStart x .-. intervalEnd x

barPlot :: (Show a, Real a) => [(Time, a)] -> QDiagram SVG V2 Double Any
barPlot [] = mempty
barPlot xs = barPlot' (minMax $ map snd xs) xs

barPlot' :: (Show a, Real a) => (a,a) -> [(Time, a)] -> QDiagram SVG V2 Double Any
barPlot' range@(min,max) xs = yAxis' range <> stroke (foldMap toBar xs)
  where
    span = realToFrac (max - min)
    toBar (t, y) =
      pathFromTrailAt
        (trailFromOffsets [V2 0 (realToFrac (y-min) / span)])
        (p2 (toSeconds t, 0))

linePlot :: (Show a, Real a) => [(Time, a)] -> QDiagram SVG V2 Double Any
linePlot [] = mempty
linePlot xs = linePlot' (minMax $ map snd xs) xs

linePlot' :: (Show a, Real a) => (a,a) -> [(Time, a)] -> QDiagram SVG V2 Double Any
linePlot' _ [] = mempty
linePlot' range@(min,max) xs =
  yAxis' range <>
  stroke (pathFromTrailAt
    (trailFromVertices $ map toPt xs)
    (toPt (head xs)))
  where
    span = realToFrac (max - min)
    toPt (t, y) = p2 (toSeconds t, realToFrac (y - min) / span)

minMax :: (Foldable f, Ord a) => f a -> (a,a)
minMax xs = (minimum xs, maximum xs)

yAxis' :: (Show a, Real a, Renderable (Diagrams.Text Double) b, Renderable (Path V2 Double) b)
       => (a, a) -> QDiagram b V2 Double Any
yAxis' (yMin, yMax) =
    axis <> foldMap yTick [(0, yMin), (1, yMax)] # fontSize 2
  where
    axis = stroke $ pathFromTrail (trailFromVertices [ p2 (0, 0), p2 (0, 1) ])
    yTick (y,lbl) = translateY (realToFrac y)
      $ alignR $ alignR (strutX 1 <> alignedText 1 0.5 (show lbl)) ||| hrule 0.1

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

readIt :: FilePath -> IO [Event]
readIt path = do
  Right (elog, _) <- Event.readEventLog <$> BSL.readFile path
  let events = Event.sortEvents $ Event.events $ Event.dat elog
  return $ events

matchPrefixMsg :: String -> EventExtractor (Time, Double)
matchPrefixMsg prefix = sampleTimes' $ matchMessageEvent $ matchPrefix prefix

doIt :: FilePath -> IO ()
doIt path = do
  events <- readIt path
  plotIt (path++".svg") events

main :: IO ()
main = do
  events <- readIt "hi.eventlog"
  --print $ runExtractor nowLive events
  --print $ take 10 heapSizeEvents
  --print $ length $ run $ supply events (capThreads $ Capability 0)
  --mapM_ print $ runIt $ runT $ supply events capThreads'
  --mapM_ print $ [ msg | Event { evSpec  = Message msg } <-  events ]
  plotIt "hi.svg" events

plotIt :: FilePath -> [Event] -> IO ()
plotIt outPath events = renderSVG outPath size (plotDiagram events)
  where
    size :: SizeSpec V2 Double
    size = dims $ V2 600 400

timelines :: forall b. (Renderable (Diagrams.Text Double) b)
          => [(String, QDiagram b V2 Double Any)]
          -> QDiagram b V2 Double Any
timelines xs =
    vsep 1
      [ align (V2 1 0) (strutX 10 <> text label) <>
        timeline
      | (label, timeline) <- xs
      ]

plotDiagram :: [Event] -> QDiagram SVG V2 Double Any
plotDiagram events = dia
  where
    heapSizeEvents = runExtractor heapSizeSamples events
    heapLiveEvents = runExtractor heapLiveSamples events

    smoothBytesSamples :: [(Time, Bytes)] -> [(Time, Double)]
    smoothBytesSamples samples =
      map (\bin -> (binStart bin, Mean.getMean $ binValue bin))
      $ bin (diffFromMilliseconds 200)
        [ (t, Mean.singleton $ realToFrac y / 1e8)
        | (t, Bytes y) <- samples
        ]

    meanHeapSize = smoothBytesSamples heapSizeEvents
    meanHeapLive = smoothBytesSamples heapLiveEvents

    dia :: QDiagram SVG V2 Double Any
    dia = timelines
      [ ( "heap size"
        , heatMap heatToColor (diffFromMilliseconds 100) (map fst heapSizeEvents)
          # scaleY 3
        )
      , ( "heap size"
        , linePlot meanHeapSize
          # lw 0.1 # scaleY 1
        ) -- <> stroke (barPlot meanHeapSize)
      , ( "live heap"
        , linePlot' (Bytes 0, Bytes $ 100*1024^2) heapLiveEvents
          # lw 0.1 # lc Colours.purple # scaleY 1
        )
      , ( "major gc starts"
        , stroke (eventPlot
            [ intervalStart $ gcInterval gc
            | gc <- runExtractor garbageCollections events
            , gcGeneration gc == 1 ])
          # fc Colours.orange # lw 0
        )
      , ( "major gc pauses"
        , stroke (gcPausesPlot
            [ gc
            | gc <- runExtractor garbageCollections events
            , gcGeneration gc == 1 ])
          # fc Colours.orange # lw 0 # scaleY 3
        )
      , ( "marking"
        , stroke (durationPlot $ runExtractor nonmovingMarkIntervals events)
          # fc Colours.blue # lw 0
        )
      , ( "sweeping"
        , stroke (durationPlot $ runExtractor nonmovingSweepIntervals events)
          # fc Colours.grey # lw 0
        )
      , ( "syncing"
        , stroke (durationPlot $ runExtractor nonmovingSyncIntervals events)
          # fc Colours.grey # lw 0
        )
      , ( "now live"
        , barPlot' (0,300e6) (runExtractor (matchPrefixMsg "now live: ") events)
          # lw 0.2
        )
      , ( "new_allocs"
        , barPlot' (0,300e6) (runExtractor (matchPrefixMsg "new_allocs: ") events)
          # lw 0.2
        )
      , ( "correction"
        , barPlot (runExtractor (matchPrefixMsg "Major GC finished: corr=") events)
          # lw 0.2 # scaleY 1
        )
      , ( "resize(max)"
        , barPlot' (0,300e6) (runExtractor (matchPrefixMsg "resize_gen: max: ") events)
          # lw 0.2
        )
      , ( "needed"
        , barPlot' (0,300e6) (runExtractor (matchPrefixMsg "calcNeeded(1): needed=") events)
          # lw 0.2
        )
      , ( "needed max"
        , barPlot' (0,300e6) (runExtractor (matchPrefixMsg "calcNeeded(1): max=") events)
          # lw 0.2
        )
      , ( "flushes"
        , stroke (eventPlot $ runExtractor nonmovingUpdRemSetFlushes events)
          # fc Colours.pink # lw 0
        )
      , ( "time"
        , timeAxis (Interval (seconds 0) (seconds 40)) (diffFromSeconds 5)
          # lw 0.1 # fontSize 5
        )
      ]

seconds n = zeroTime .+^ (n *^ diffFromSeconds 1)
