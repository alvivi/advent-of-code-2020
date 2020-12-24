#!/usr/bin/env stack
-- stack script --resolver lts-14.16 --package containers

import Data.Set (Set)
import qualified Data.Set as S

data Direction = E | NE | NW | SE | SW | W deriving (Show, Eq)

parseLine :: String -> [Direction]
parseLine ('e':cs) = E : parseLine cs
parseLine ('n':'e':cs) = NE : parseLine cs
parseLine ('n':'w':cs) = NW : parseLine cs
parseLine ('s':'e':cs) = SE : parseLine cs
parseLine ('s':'w':cs) = SW : parseLine cs
parseLine ('w':cs) = W : parseLine cs
parseLine [] = []

move :: (Int, Int) -> Direction -> (Int, Int)
move (x, y) E = (x + 2, y)
move (x, y) NE = (x + 1, y - 1)
move (x, y) NW = (x - 1, y - 1)
move (x, y) SE = (x + 1, y + 1)
move (x, y) SW = (x - 1, y + 1)
move (x, y) W = (x - 2, y)

toogle :: Set (Int, Int) -> (Int, Int) -> Set (Int, Int)
toogle set p | p `S.member` set = S.delete p set
toogle set p = S.insert p set

parse :: String -> Set (Int, Int)
parse = foldl toogle S.empty . map (foldl move (0,  0)) . map parseLine . lines

neighbors :: (Int, Int) -> [(Int, Int)]
neighbors p = map (move p) [E, NE, NW, SE, SW, W]

paint :: (Int, Int) -> Set (Int, Int) -> Bool
paint p tileMap =
  let count = sum $ map (fromEnum . (`S.member` tileMap)) $ neighbors p
  in if p `S.member` tileMap then count <= 2 else count == 2

step :: Set (Int, Int) -> Set (Int, Int)
step tileMap = S.foldl step' S.empty tileMap
  where
    step' acc p = foldl tile acc $ neighbors p
    tile acc p = if paint p tileMap then S.insert p acc else acc

main :: IO ()
main = do
  tileMap <- parse <$> getContents
  print $ S.size $ (!! 100) $ iterate step tileMap

