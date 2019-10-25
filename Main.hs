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

import EventLog
import Bin
import Time
import Mean

toSeconds :: Time -> Double
toSeconds (Time t) = realToFrac t / 1e9

durationPlot :: [Interval] -> Path V2 Double
durationPlot = foldMap toDuration
  where
    toDuration = 

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
          width = toSeconds $ zeroTime `addTime` binSize
   in hcat $ map toBinRect bins

heatToColor :: Real a => a -> a -> AlphaColour Double
heatToColor m n = Colours.green `withOpacity` (realToFrac n / realToFrac m)

main :: IO ()
main = do
  Right (elog, _) <- Event.readEventLog <$> BSL.readFile "hi.eventlog"
  let events = Event.events $ Event.dat elog
  let heapSizeEvents = run $ supply events heapSizes
  --print $ length $ run $ supply events garbageCollections
  --print $ take 10 heapSizeEvents
  --print $ length $ run $ supply events (capThreads $ Capability 0)
  --mapM_ print $ runIt $ runT $ supply events capThreads'

  let n = 20000
  --let dia = foldMap event $ take n heapSizeEvents
  --    event (t,_) =
  --      translateX (toSeconds t) (vrule 10)
  --      # lc black # lw 0.5
  let size = dims $ V2 600 400 :: SizeSpec V2 Double
  renderSVG "hi.svg" size $ scaleX 10
    $ heatMap heatToColor (fromMilliseconds 100) (map fst heapSizeEvents)
      ===
      strutY 2
      ===
        (stroke $ linePlot
          [ (t, realToFrac y / 1e8)
          | (t,BytesAllocated y) <- heapSizeEvents
          ]
        <> barPlot
            (map (\bin -> (binStart bin, Mean.getMean $ binValue bin))
             $ bin (fromMilliseconds 100)
              [ (t, Mean.singleton $ realToFrac y / 1e8)
              | (t,BytesAllocated y) <- heapSizeEvents
              ])
        )
