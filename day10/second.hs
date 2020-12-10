#!/usr/bin/env stack
-- stack script --resolver lts-14.16

import qualified Data.List as List

main :: IO ()
main =
  let
    step [] = []
    step (x:xs) =
      let ys = step xs
      in (max 1 $ sum $ zipWith const ys $ takeWhile (<= x + 3) xs) : ys
  in do
    adapters <- (0:) <$> List.sort <$> map read <$> lines <$> getContents
    putStrLn $ show $ head $ step adapters
