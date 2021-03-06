#!/usr/bin/env stack
-- stack script --resolver lts-14.16 --package containers

import qualified Data.Char as Char
import qualified Data.Map as Map
import Data.Maybe (fromJust)

type Bags = Map.Map (String, String) (Map.Map (String, String) Int)

main :: IO ()
main =
  let
    tokenize' (c:cs) (g:gs) | Char.isAlphaNum c = tokenize' cs ((c:g):gs)
    tokenize' (c:cs) (g:gs) | Char.isNumber c = tokenize' cs ((c:g):gs)
    tokenize' (c:cs) gs | Char.isSpace c = tokenize' cs ([]:gs)
    tokenize' (c:cs) gs | Char.isPunctuation  c = tokenize' cs ([]:[c]:gs)
    tokenize' [] gs = gs
    tokenize input = reverse $ map reverse $ filter ((> 0) . length) $ tokenize' input [[]]
    parse :: Bags -> [String] -> Bags
    parse acc (c1:c2:"bags":"contain":"no":"other":"bags":".":[]) = Map.insert (c1, c2) Map.empty acc
    parse acc (c1:c2:"bags":"contain":contents) = Map.insert (c1, c2) (parseContents Map.empty contents) acc
    parseContents acc ("1":c1:c2:"bag":more) = Map.insert (c1, c2) 1 $ parseContents acc more
    parseContents acc (n:c1:c2:"bags":more) = Map.insert (c1, c2) (read n) $ parseContents acc more
    parseContents acc (",":more) = parseContents acc more
    parseContents acc (".":[]) = acc
    contains bags entry bag | Map.member entry bag = True
    contains bags entry bag = any (contains bags entry) $ map (fromJust . (`Map.lookup` bags)) $ Map.keys bag
  in do
    contents <- getContents
    let bags = Map.delete ("shiny","gold") $ foldl parse Map.empty $ map tokenize $ lines contents
    putStrLn $ show $ sum $ map (fromEnum . (contains bags ("shiny","gold"))) $ Map.elems bags
