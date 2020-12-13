#!/usr/bin/env stack
-- stack script --resolver lts-14.16

import Data.List (minimumBy)

main :: IO ()
main =
  let
    parse [] = []
    parse (',' : cs) = ' ' : parse cs
    parse ('x' : cs) = parse cs
    parse (c : cs) = c : parse cs
    waitTime timestamp id =
      let diff = mod timestamp id
      in if diff == 0 then 0 else (timestamp - diff + id) - timestamp
    minTime = minimumBy (\(_, x) (_, y) -> compare x y)
  in do
    (firstLine : secondLine : []) <- lines <$> getContents
    let timestamp = (read firstLine)
    let ids = (map read $ words $ parse secondLine)
    putStrLn $ show $ uncurry (*) $ minTime $ zip ids $ map (waitTime timestamp) ids
