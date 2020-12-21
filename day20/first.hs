#!/usr/bin/env stack
-- stack script --resolver lts-14.16 --package containers

import Data.Map (Map, (!))
import Data.Set (Set)
import qualified Data.Map as Map
import qualified Data.Set as Set

type Image = Set (Int, Int)

parseTile :: [String] -> Int
parseTile = read . init . last . words . head

parseImage :: [String] -> Image
parseImage = foldl foldColumns Set.empty . zip [0..] . tail
  where
    foldColumns set (j, line) = foldl (insertPoint j) set $ zip [0..] line
    insertPoint j set (i, '#') | isBorder (j, i) = Set.insert (j, i) set
    insertPoint j set (i, c) = set
    isBorder (x, y) = x == 0 || x == 9 || y == 0 || y == 9

parse :: String -> Map Int Image
parse = foldl step Map.empty . split . lines
  where
    step map lines = Map.insert (parseTile lines) (parseImage lines) map
    split = foldr splitStep [[]]
    splitStep "" acc = []:acc
    splitStep s (g:gs) = (s:g):gs
    mapZip f g v = (f v, g v)

checkBorder :: [Bool] -> Image -> Bool
checkBorder border image = border `elem` elems
  where
    elems = [topL2r, topR2l, rightL2r, rightR2l, bottomL2r, bottomR2l, leftL2r, leftR2l]
    topL2r = map (`Set.member` image) [(i, 0) | i <- [0..9]]
    topR2l = map (`Set.member` image) [(9 - i, 0) | i <- [0..9]]
    rightL2r = map (`Set.member` image) [(9, j) | j <- [0..9]]
    rightR2l = map (`Set.member` image) [(9, 9 - j) | j <- [0..9]]
    bottomL2r = map (`Set.member` image) [(i, 9) | i <- [0..9]]
    bottomR2l = map (`Set.member` image) [(9 - i, 9) | i <- [0..9]]
    leftL2r = map (`Set.member` image) [(0, j) | j <- [0..9]]
    leftR2l = map (`Set.member` image) [(0, 9 - j) | j <- [0..9]]

borders :: Image -> [[Bool]]
borders image = [top, right, bottom, left]
  where
    top = map (`Set.member` image) [(i, 0) | i <- [0..9]]
    right = map (`Set.member` image) [(9, j) | j <- [0..9]]
    bottom = map (`Set.member` image) [(i, 9) | i <- [0..9]]
    left = map (`Set.member` image) [(0, j) | j <- [0..9]]

corners :: Map Int Image -> [Int]
corners images = map fst $ filter isCorner $ map step $ Map.keys images
  where
    step key = (key, check (images ! key) (Map.delete key images))
    check image images = count $ map (checkAllBorders $ Map.elems images) $ borders image
    checkAllBorders images border = map (checkBorder border) images
    count = sum . map fromEnum . concat
    isCorner (i, c) = c == 2

main :: IO ()
main = do
  images <- parse <$> getContents
  print $ foldl (*) 1 $ corners images
