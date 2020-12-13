#!/usr/bin/env stack
-- stack script --resolver lts-14.16

import qualified Data.List as List

main :: IO ()
main =
  let
    clean [] = []
    clean (',' : cs) = ' ' : clean cs
    clean (c : cs) = c : clean cs
    step (list, i) "x" = (list, i + 1)
    step (list, i) n = ((i, read n) : list, i + 1)
    mult = foldl (*) 1
    modMultInv q 1 = 1
    modMultInv q p = let n = p - modMultInv p (q `mod` p) in (n * q + 1) `div` p
    chineseRemainder list (m, q) =
      let p = mult $ List.delete q $ map snd list
       in modMultInv q p * p * m
  in do
    (firstLine : secondLine : []) <- lines <$> getContents
    let input = reverse $ fst $ foldl step ([], 0) $ words $ clean secondLine
    let offsets = zip (map (uncurry $ flip (-)) input) $ map snd input
    let cm = mult $ map snd offsets
    let cp = sum $ map (chineseRemainder offsets) offsets
    putStrLn $ show $ mod cp cm
