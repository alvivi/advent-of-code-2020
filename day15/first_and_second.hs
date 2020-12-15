#!/usr/bin/env stack
-- stack script --resolver lts-14.16 --package containers

import Data.Map (Map)
import Data.Maybe (fromJust)
import qualified Data.Map as Map
import Debug.Trace (trace)

main :: IO ()
main =
  let
    replaceCommas (',' : cs) = ' ' : replaceCommas cs
    replaceCommas (c : cs) = c : replaceCommas cs
    replaceCommas [] = []
    updateSpoken turn number spoken =
      case Map.lookup number spoken of
        Nothing -> Map.insert number [turn] spoken
        Just list -> Map.insert number [turn, head list] spoken
    step turn spoken last =
      case Map.lookup last spoken of
        Just (first:[]) ->
          (0, updateSpoken turn 0 spoken)
        Just (second:first:[]) ->
          let num = second - first
          in (num, updateSpoken turn num spoken)
    run until turn spoken last =
      if turn >= until then
        last
      else
        let (num, updatedSpoken) = step turn spoken last
        in run until (turn + 1) updatedSpoken num

  in do
    numbers <- (map read <$> words <$> replaceCommas <$> getContents) :: IO [Int]
    let spoken = foldl (\m (t, n) -> Map.insert n [t] m) Map.empty $ zip [1..] numbers
    putStrLn $ show spoken
    putStrLn $ show $ run (30000000 + 1) 4 spoken (last numbers)
