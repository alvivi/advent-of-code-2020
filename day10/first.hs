#!/usr/bin/env stack
-- stack script --resolver lts-14.16

import qualified Data.List as List

main :: IO ()
main =
  let
    count :: [Int] -> (Int, Int, Int)
    count = foldl step (0, 0, 0)
    step (last, one, three) current =
      case current - last of
        1 -> (current, one + 1, three)
        3 -> (current, one, three + 1)
    mult (_, one, three) = one * (three + 1)
  in do
    contents <- getContents
    putStrLn $ show $ length $ lines contents
    putStrLn $ show $ count $ List.sort $ map read $ lines contents
