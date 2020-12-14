#!/usr/bin/env stack
-- stack script --resolver lts-14.16 --package containers

import Data.Char (digitToInt, intToDigit)
import Data.Map (Map)
import Data.Maybe (fromJust, listToMaybe)
import Numeric (readInt, showIntAtBase)
import qualified Data.Map as Map


main :: IO ()
main =
  let
    extend' 0 _ = []
    extend' n [] = '0' : extend' (n - 1) []
    extend' n (c:cs) = c : extend' (n - 1) cs
    extend = reverse . extend' 36 . reverse
    parseDecimal value = extend $ showIntAtBase 2 intToDigit (read value) ""
    applyMask' c 'X' = c
    applyMask' _ m = m
    applyMask value mask = map (uncurry applyMask') $ zip mask value
    readBin = fromJust . fmap fst . listToMaybe . readInt 2 (`elem` "01") digitToInt
    step (_, map) ("mask":"=":mask:[]) = (mask, map)
    step (mask, map) (lhs:"=":rhs:[]) =
      let
        ('m':'e':'m':'[':plhs) = lhs
        dir = read $ init plhs
      in
        (mask, Map.insert dir (readBin $ applyMask mask $ parseDecimal rhs) map)
    initMask = "XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX"
  in do
    contents <- map words <$> lines <$> getContents
    let (_, map) = foldl step (initMask, Map.empty :: Map Int Int) contents
    putStrLn $ show $ sum $ Map.elems map

