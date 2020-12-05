#!/usr/bin/env stack
-- stack script --resolver lts-14.16 --package containers

import Data.Char  (digitToInt)
import Data.Maybe (listToMaybe, fromMaybe)
import Data.List  (maximumBy, minimumBy)
import Numeric    (readInt)
import qualified Data.Set as Set

main :: IO ()
main =
  let
    toBin 'B' = '1'
    toBin 'F' = '0'
    toBin 'R' = '1'
    toBin 'L' = '0'
    readBin = fromMaybe 0 . fmap fst . listToMaybe . readInt 2 (`elem` "01") digitToInt
    readId (row, col) = readBin row * 8 + readBin col
    tagId seat@(row, col) = (readId seat, (readBin row, readBin col))
    byId (a, _) (b, _) = compare a b
    iter (r, c) (mr, mc) | r < mr && c < 7 = (r, c + 1) : iter (r, c + 1) (mr, mc)
    iter (r, c) (mr, mc) | r < mr && c == 7 = (r + 1, 0) : iter (r + 1, 0) (mr, mc)
    iter (r, c) (mr, mc) | r == mr && c < mc = (r, c + 1) : iter (r, c + 1) (mr, mc)
    --iter (r, c) (mr, mc) | r <= mr && c <= mc && c == 7 = (r + 1, 0) : iter (r + 1, 0) (mr, mc)
    iter _ _ = []
   in do
    contents <- getContents
    let seatList = map (tagId . splitAt 7 . (map toBin)) $ lines $ contents
    let (_, (minRow, minCol)) = minimumBy byId seatList
    let (_, (maxRow, maxCol)) = maximumBy byId seatList
    let seatSet = foldl (\set (_, seat) -> Set.insert seat set) Set.empty seatList
    let [(row, col)] = filter (\s -> not $ Set.member s seatSet) $ iter (minRow, minCol) (maxRow, maxCol)
    putStrLn $ show $ row * 8 + col
