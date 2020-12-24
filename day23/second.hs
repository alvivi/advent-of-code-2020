#!/usr/bin/env stack
-- stack script --resolver lts-14.16 --package containers

import Data.IntMap.Strict (IntMap)
import qualified Data.IntMap.Strict as M

type Ring = (Int, IntMap Int)

parse :: String -> Ring
parse input = (head ns, M.fromList $ zip ns (tail ns ++ [head ns]))
  where
    ns' = map (read . (:[])) input
    ns = ns' ++ [maximum ns' + 1 .. 1000000]

step :: Ring -> Ring
step (head, map) = (next, map')
  where
    c1 = map M.! head
    c2 = map M.! c1
    c3 = map M.! c2
    slice = [c1, c2, c3]
    findDst n | n <= 1 = findDst $ fst $ M.findMax map
    findDst n = let m = n - 1 in if m `notElem` slice then m else findDst m
    dest = findDst head
    destNext = map M.! dest
    map' = M.union (M.fromList [(head, next), (dest, c1), (c3, destNext)]) map
    next = map M.! c3

play :: Int -> Ring -> Ring
play 0 ring = ring
play n ring = play (n - 1) $ step ring

main :: IO ()
main = do
  print $ solve $ parse "418976235"
