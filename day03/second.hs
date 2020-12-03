#!/usr/bin/env stack
-- stack script --resolver lts-14.16 --package containers

import qualified Data.Set as Set

main :: IO ()
main =
  let
    parseForest =
      let
        joinRow y = map (\(x, c) -> (x, y, c))
        insert set (x, y, c) = if c == '#' then Set.insert (x,y) set else set
      in
        foldl insert Set.empty
        . concat
        . zipWith joinRow [0..]
        . map (zip [0..])
        . lines
    member forest width height (x, y) =
      Set.member (x `mod` width, y `mod` height) forest
    path hl h v (x, y) | y >= hl = []
    path hl h v (x, y) = (x, y) : (path hl h v (x + h, y + v))
  in do
    contents <- getContents
    let height = length $ lines contents
    let width = length $ head $ lines contents
    let forest = parseForest contents
    let steps = [(1, 1), (3, 1), (5, 1), (7, 1), (1, 2)]
    let paths = map (\(h, v) -> path height h v (0, 0)) steps
    let countTrees = length . filter id . map (member forest width height)
    putStrLn $ show $ foldl (*) 1 $ map countTrees paths
