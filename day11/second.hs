#!/usr/bin/env stack
-- stack script --resolver lts-14.16 --package containers

import Data.Maybe (fromMaybe)
import qualified Data.Map as Map
import Data.Function ((&))

main :: IO ()
main =
  let
    buildMap contents =
      let
        lines' = lines contents
        numRows = length lines'
        numCols = length $ head lines'
        map = foldl buildMapRow Map.empty $ zip [0..] $ lines'
      in
        (map, (numRows, numCols))
    buildMapRow map (row, columns) =
      let step map (col, seat) = Map.insert (row, col) seat map
      in foldl step map $ zip [0..] columns
    getSeat pos = fromMaybe '.' . Map.lookup pos
    visibleSeat pos iter map =
      case Map.lookup (iter pos) map of
        Just '#' -> '#'
        Just 'L' -> 'L'
        Just '.' -> visibleSeat (iter pos) iter map
        Nothing -> '.'
    visibleCount pos map' =
      [ visibleSeat pos (\(x, y) -> (x - 1, y - 1)) map' == '#'
      , visibleSeat pos (\(x, y) -> (x    , y - 1)) map' == '#'
      , visibleSeat pos (\(x, y) -> (x + 1, y - 1)) map' == '#'
      , visibleSeat pos (\(x, y) -> (x - 1, y    )) map' == '#'
      , visibleSeat pos (\(x, y) -> (x + 1, y    )) map' == '#'
      , visibleSeat pos (\(x, y) -> (x - 1, y + 1)) map' == '#'
      , visibleSeat pos (\(x, y) -> (x    , y + 1)) map' == '#'
      , visibleSeat pos (\(x, y) -> (x + 1, y + 1)) map' == '#'
      ] & map fromEnum & sum
    stepSeat pos map acc =
      case getSeat pos map of
        '.' -> (Map.insert pos '.' acc, False)
        'L' ->
          if visibleCount pos map == 0 then
            (Map.insert pos '#' acc, True)
          else
            (Map.insert pos 'L' acc, False)
        '#' ->
          if visibleCount pos map >= 5 then
            (Map.insert pos 'L' acc, True)
          else
            (Map.insert pos '#' acc, False)
    step (w, h) map =
      let
        step' (acc, changed) pos =
          let (acc', changed') = stepSeat pos map acc
          in (acc', changed || changed')
      in
        foldl step' (Map.empty, False) [(x, y) | y <- [0..h - 1], x <- [0..w - 1]]
    run size map =
      let (map', changed) = step size map
      in if changed then run size map' else map'
    count = Map.foldl (\acc seat -> if seat == '#' then acc + 1 else acc) 0
  in do
    (map, size) <- buildMap <$> getContents
    putStrLn $ show $ count $ run size map
