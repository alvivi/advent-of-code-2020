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
  in do
    contents <- getContents
    let height = length $ lines contents
    let width = length $ head $ lines contents
    let path = map (\y -> (y * 3, y)) [0..(height - 1)]
    let forest = parseForest contents
    putStrLn $ show $ length $ filter id $ map (member forest width height) path
