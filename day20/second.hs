#!/usr/bin/env stack
-- stack script --resolver lts-14.16 --package containers

import Data.Map.Strict (Map, (!))
import Data.Maybe (isJust, fromJust)
import Data.Set (Set)
import qualified Data.Map.Strict as Map
import qualified Data.Set as Set

data Direction = L2R | R2L deriving (Show)
data Side = STop | SRight | SBottom | SLeft deriving (Show, Eq)
data Transform = R90 | R180 | R270 | FlipX | FlipY deriving (Show)

type Image = Set (Int, Int)
type Neighborhood = Map Int [(Side, Int, [Transform])]

parseTile :: [String] -> Int
parseTile = read . init . last . words . head

parseImage :: [String] -> Image
parseImage = foldl foldColumns Set.empty . zip [0..] . tail
  where
    foldColumns set (j, line) = foldl (insertPoint j) set $ zip [0..] line
    insertPoint j set (i, '#') = Set.insert (j, i) set
    insertPoint j set (i, c) = set

parse :: String -> Map Int Image
parse = foldl step Map.empty . split . lines
  where
    step map lines = Map.insert (parseTile lines) (parseImage lines) map
    split = foldr splitStep [[]]
    splitStep "" acc = []:acc
    splitStep s (g:gs) = (s:g):gs
    mapZip f g v = (f v, g v)

checkBorder :: (Side, [Bool]) -> Image -> Maybe [Transform]
checkBorder (side, border) image =
    case filter (check border) elems of
      [] -> Nothing
      [(side', dir, _)] -> Just $ transforms side side' dir
  where
    check border (side, dir, border') = border' == border
    elems = [topL2r, topR2l, rightL2r, rightR2l, bottomL2r, bottomR2l, leftL2r, leftR2l]
    topL2r = (STop, L2R, map (`Set.member` image) [(i, 0) | i <- [0..9]])
    topR2l = (STop, R2L, map (`Set.member` image) [(9 - i, 0) | i <- [0..9]])
    rightL2r = (SRight, L2R, map (`Set.member` image) [(9, j) | j <- [0..9]])
    rightR2l = (SRight, R2L, map (`Set.member` image) [(9, 9 - j) | j <- [0..9]])
    bottomL2r = (SBottom, L2R, map (`Set.member` image) [(i, 9) | i <- [0..9]])
    bottomR2l = (SBottom, R2L, map (`Set.member` image) [(9 - i, 9) | i <- [0..9]])
    leftL2r = (SLeft, L2R, map (`Set.member` image) [(0, j) | j <- [0..9]])
    leftR2l = (SLeft, R2L, map (`Set.member` image) [(0, 9 - j) | j <- [0..9]])

borders :: Image -> [(Side, [Bool])]
borders image = [(STop, top), (SRight, right), (SBottom, bottom), (SLeft, left)]
  where
    top = map (`Set.member` image) [(i, 0) | i <- [0..9]]
    right = map (`Set.member` image) [(9, j) | j <- [0..9]]
    bottom = map (`Set.member` image) [(i, 9) | i <- [0..9]]
    left = map (`Set.member` image) [(0, j) | j <- [0..9]]

findNeighbors :: Map Int Image -> Neighborhood
findNeighbors images = Map.fromList $ map choicesStep $ Map.keys images
  where
    choicesStep key = (key, check (images ! key) (Map.delete key images))
    check image images = count $ map (checkAllBorders images) $ borders image
    checkAllBorders images border = map (checkAllStep images border) $ Map.keys images
    checkAllStep images border@(side, _) key = (side, key, checkBorder border (images ! key))
    count = map (\(i, s, t) -> (i, s, fromJust t)) . filter (isJust . thr) . concat
    fst' (a, _, c) = a
    thr (_, _, c) = c

transforms :: Side -> Side -> Direction -> [Transform]
transforms STop    STop    L2R = [FlipY]
transforms STop    SRight  L2R = [R90, FlipX]
transforms STop    SBottom L2R = []
transforms STop    SLeft   L2R = [R270]
transforms STop    STop    R2L = [R180]
transforms STop    SRight  R2L = [R90]
transforms STop    SBottom R2L = [FlipX]
transforms STop    SLeft   R2L = [R270, FlipX]
transforms SRight  STop    L2R = [R270, FlipY]
transforms SRight  SRight  L2R = [FlipX]
transforms SRight  SBottom L2R = [R90]
transforms SRight  SLeft   L2R = []
transforms SRight  STop    R2L = [R270]
transforms SRight  SRight  R2L = [R180]
transforms SRight  SBottom R2L = [R90, FlipY]
transforms SRight  SLeft   R2L = [FlipY]
transforms SBottom STop    L2R = []
transforms SBottom SRight  L2R = [R270]
transforms SBottom SBottom L2R = [FlipY]
transforms SBottom SLeft   L2R = [R90, FlipX]
transforms SBottom STop    R2L = [FlipX]
transforms SBottom SRight  R2L = [R270, FlipX]
transforms SBottom SBottom R2L = [R180]
transforms SBottom SLeft   R2L = [R90]
transforms SLeft   STop    L2R = [R90]
transforms SLeft   SRight  L2R = []
transforms SLeft   SBottom L2R = [R270, FlipY]
transforms SLeft   SLeft   L2R = [FlipX]
transforms SLeft   STop    R2L = [R90, FlipY]
transforms SLeft   SRight  R2L = [FlipY]
transforms SLeft   SBottom R2L = [R270]
transforms SLeft   SLeft   R2L = [R180]

applyTransforms :: [Transform] -> Image -> Image
applyTransforms ts image = foldl (flip applyTransform) image ts

applyTransform :: Transform -> Image -> Image
applyTransform R90 = rotate
applyTransform R180 = rotate . rotate
applyTransform R270 = rotate . rotate .rotate
applyTransform FlipX = flipx
applyTransform FlipY = flipy

rotate :: Image -> Image
rotate = Set.map (\(x, y) -> (y, 9 - x))

flipx :: Image -> Image
flipx = Set.map (\(x, y) -> (9 - x, y))

flipy :: Image -> Image
flipy = Set.map (\(x, y) -> (x, 9 - y))

firstNeighbor :: Neighborhood -> Int
firstNeighbor = head . Map.keys . Map.filter ((== 2) . length)

joinNeighborhood :: Map Int Image -> Neighborhood -> Int -> (Int, Int) -> [Transform] -> Set Int -> Image -> (Image, Set Int)
joinNeighborhood images neighbors idx offset ts printted result =
  let
    borderless = Set.map (\(x, y) -> (x - 1, y - 1)) . Set.filter (\(x, y) -> x >= 1 && x <= 8 && y >= 1 && y <= 8)
    applyOffset (dx, dy) = Set.map (\(x, y) -> (x + dx, y + dy))
    result' = Set.union result $ applyOffset offset $ applyTransforms [] $ borderless $ images ! idx
    printted' = Set.insert idx printted
    computeOffset (x, y) STop = (x, y - 8)
    computeOffset (x, y) SRight = (x + 8, y)
    computeOffset (x, y) SBottom = (x, y + 8)
    computeOffset (x, y) SLeft = (x - 8, y)
    step (accResult, accPrintted) (side, idx', ts') = joinNeighborhood images neighbors idx' (computeOffset offset side) ts' accPrintted accResult
  in
    if Set.member idx printted then (result, printted) else foldl step (result', printted') (neighbors ! idx)

buildImage :: Map Int Image -> Image
buildImage images =
  let neighbors = findNeighbors images
  in fst $ joinNeighborhood images neighbors (firstNeighbor neighbors) (0,0) [] Set.empty Set.empty

minMax :: Image -> ((Int, Int), (Int, Int))
minMax =
  let step ((ix, iy), (ax, ay)) (x, y) = ((min ix x, min iy y), (max ax x, max ay y))
  in Set.foldl step ((maxBound, maxBound), (minBound, minBound))


monster :: [(Int, Int)]
monster = Set.toList $ parseImage
  [ "                  # "
  , "#    ##    ##    ###"
  , " #  #  #  #  #  #   "
  ]

removeMonster :: (Int, Int) -> Image -> Image
removeMonster (ox, oy) image =
  let ps = map (\(x, y) -> (ox + x, oy + y)) monster
  in if all (`Set.member` image) ps then Set.difference image (Set.fromList ps) else image

removeAllMonsters :: Image -> Image
removeAllMonsters image =
  let ((ix, iy), (ax, ay)) = minMax image
  in foldl (flip removeMonster) image [(x, y) | x <- [ix..ax], y <- [iy..ay]]

main :: IO ()
main = do
  image <- buildImage <$> parse <$> getContents
  let images = [image, rotate image, rotate $ rotate image, rotate $ rotate $ rotate image, flipx image, flipy image]
  print $ minimum $ map (Set.size . removeAllMonsters) images
