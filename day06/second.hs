#!/usr/bin/env stack
-- stack script --resolver lts-14.16 --package containers

import Data.Maybe (fromMaybe)
import qualified Data.Set as Set

main :: IO ()
main =
  let
    accum list "" = Nothing : list
    accum (maybeSet : list) chars =
      let iset = foldl (flip Set.insert) Set.empty chars
      in case maybeSet of
        Nothing -> (Just iset) : list
        Just set -> (Just $ Set.intersection iset set) : list
    count = sum . map (fromMaybe 0 . fmap Set.size) . foldl accum [Nothing]
  in do
    contents <- getContents
    putStrLn $ show $ count $ lines contents
