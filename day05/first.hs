#!/usr/bin/env stack
-- stack script --resolver lts-14.16

import Data.Char  (digitToInt)
import Data.Maybe (listToMaybe, fromMaybe)
import Numeric    (readInt)

main :: IO ()
main =
  let
    toBin 'B' = '1'
    toBin 'F' = '0'
    toBin 'R' = '1'
    toBin 'L' = '0'
    readBin = fromMaybe 0 . fmap fst . listToMaybe . readInt 2 (`elem` "01") digitToInt
    readId (row, col) = readBin row * 8 + readBin col
  in do
    contents <- getContents
    putStrLn $ show $ maximum $ map (readId . splitAt 7 . (map toBin)) $ lines $ contents
