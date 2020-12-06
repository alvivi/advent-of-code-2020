#!/usr/bin/env stack
-- stack script --resolver lts-14.16 --package containers

import qualified Data.Set as Set

main :: IO ()
main =
  let
    accum list "" = Set.empty : list
    accum (set : list) chars = foldl (flip Set.insert) set chars : list
  in do
    contents <- getContents
    putStrLn $ show $ sum $ map Set.size $ foldl accum [Set.empty] $ lines contents
