#!/usr/bin/env stack
-- stack script --resolver lts-14.16 --package ghc
import qualified Util as Util

main :: IO ()
main =
  let
    parse input =
      let
        [indexes, (char:_), passwd] = words input
        (first, ('-':second)) = break (== '-') indexes
      in
        (read first :: Int, read second :: Int, char, passwd)
    valid (first, second, char, passwd) =
      (passwd !! (first - 1) == char) /= (passwd !! (second - 1) == char)
  in do
    contents <- getContents
    putStrLn $ show $ Util.count id $ map (valid . parse) $ lines $ contents
