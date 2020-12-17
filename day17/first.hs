#!/usr/bin/env stack
-- stack script --resolver lts-14.16 --package containers

import Data.List ((\\))
import Data.Set (Set)
import qualified Data.Set as Set

parse :: String -> Set (Int, Int, Int)
parse =
  let
    accRow acc (x, line) = foldl (accCol x) acc $ zip [0..] $ line
    accCol x acc (y, '.') = acc
    accCol x acc (y, '#') = Set.insert (x, y, 0) acc
  in
    foldl accRow Set.empty . zip [0..] . lines

neighbors :: (Int, Int, Int) -> [(Int, Int, Int)]
neighbors p@(x, y, z) =
  [(i, j, k) | i <- [(x - 1)..(x + 1)], j <- [(y - 1)..(y + 1)], k <- [(z - 1)..(z + 1)]] \\ [p]

step :: Set (Int, Int, Int) -> Set (Int, Int, Int)
step active =
  let
    insertNeighbors acc p = foldl (flip Set.insert) acc $ neighbors p
    allNeighbors = Set.elems $ foldl insertNeighbors Set.empty $ Set.elems active
    countActive p = length $ filter (`Set.member` active) $ neighbors p
    updateState acc p =
      let
        count = countActive p
      in
        if Set.member p active
        then if count == 2 || count == 3 then Set.insert p acc else acc
        else if count == 3 then Set.insert p acc else acc
  in
    foldl updateState Set.empty allNeighbors

run :: Int -> Set (Int, Int, Int) -> Set (Int, Int, Int)
run 0 scenario = scenario
run n scenario = run (n - 1) $ step scenario

main :: IO ()
main = do
  scenario <- parse <$> getContents
  putStrLn $ show $ length $ run 6 scenario
