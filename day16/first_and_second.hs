#!/usr/bin/env stack
-- stack script --resolver lts-14.16

import qualified Data.Char as Char
import qualified Data.List as List
import Data.List ((\\))

parse :: String -> ([(String, [(Int, Int)])], [Int], [[Int]])
parse contents =
  let
    replaceCommas [] = []
    replaceCommas (',':cs) = ' ' : replaceCommas cs
    replaceCommas (c:cs) = c : replaceCommas cs
    splitByComma = words . replaceCommas
    splitLinesStep "" acc  = ([]:acc)
    splitLinesStep line (a:as) = ((line:a):as)
    splitLines = foldr splitLinesStep [[]]
    groupByDigit = List.groupBy (\l r -> Char.isDigit l && Char.isDigit r)
    filterDigits = filter (Char.isDigit . head)
    groupPairs [] = []
    groupPairs (a:b:cs) = (a, b) : groupPairs cs
    [firstPart, secondPart, thirdPart] = splitLines $ lines contents
    yourTicket = map read $ splitByComma $ last secondPart
    parseClass line =
      let
        (title, titleRest) = break (== ':') $ line
        groups = groupPairs $ map read $ filterDigits $ groupByDigit titleRest
      in
        (title, groups)
    parseNearby = map (map read .splitByComma) . tail
  in
    (map parseClass firstPart, yourTicket, parseNearby thirdPart)

solveFirst :: [(String, [(Int, Int)])] -> [[Int]] -> Int
solveFirst groups nearby =
  let
    checkNumberWithGroup n (l, r) = min l r <= n && max l r >= n
    ranges = concat $ map snd groups
    checkNumber n = any (checkNumberWithGroup n) ranges
  in
    sum $ filter (not . checkNumber) $ concat nearby


--solveSecond :: [(String, [(Int, Int)])] -> [Int] -> [[Int]] -> Maybe [[String]]
solveSecond groups ticket nearby =
  let
    checkNumberWithGroup n (l, r) = min l r <= n && max l r >= n
    ranges = concat $ map snd groups
    checkNumber n = any (checkNumberWithGroup n) ranges
    tickets = ticket : filter (all checkNumber) nearby
    allGroups = map fst groups
    options n =
      foldl (\acc (title, ranges) ->
        if any (checkNumberWithGroup n) ranges then title:acc else acc
      ) [] groups
    rangeOptions = map (foldl List.intersect allGroups) $ List.transpose $ map (map options) tickets
    removeUnique options | all ((== 1) . length) options = options
    removeUnique options =
      let
        removeList = concat $ filter ((== 1) . length) options
      in
        removeUnique $ map (\l ->
          let r = l \\ removeList
          in if length r <= 0 then l else r
        ) options
    actualGroups = concat $ removeUnique rangeOptions
    replaceName title | List.isPrefixOf "departure" title = 1
    replaceName title = 0
  in
    foldl (*) 1 $ filter (/= 0) $ map (\(x, y) -> replaceName x * y) $ zip actualGroups ticket


main :: IO ()
main = do
  (groups, ticket, nearby) <- parse <$> getContents
  --putStrLn $ show $ solveFirst groups nearby
  putStrLn $ show $ solveSecond groups ticket nearby
