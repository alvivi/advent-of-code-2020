#!/usr/bin/env stack
-- stack script --resolver lts-14.16 --package containers --package ghc
import qualified Data.Map as Map
import Data.Maybe (fromMaybe)
import qualified Util as Util

main :: IO ()
main =
  let
    parse input =
      let
        [count, (required:_), passwd] = words input
        (min, ('-':max)) = break (== '-') count
      in
        (read min :: Int, read max :: Int, required, passwd)
    updateCount = pure . maybe 1 (+ 1)
    valid (min, max, char, passwd) =
      let
        index = foldl (\map char -> Map.alter updateCount char map) Map.empty passwd
        count = fromMaybe 0 $ Map.lookup char index
      in
        count >= min && count <= max
  in do
    contents <- getContents
    putStrLn $ show $ Util.count id $ map (valid . parse) $ lines $ contents
