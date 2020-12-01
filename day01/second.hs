#!/usr/bin/env stack
-- stack script --resolver lts-14.16

main :: IO ()
main =
  let
    findTriplet [] = Nothing
    findTriplet (x:xs) =
      case findPair x xs of
        Nothing -> findTriplet xs
        Just (y, z) -> Just [x, y, z]
    findPair _ [] = Nothing
    findPair base (x:xs) =
      case (filter (\y -> base + y + x == 2020) xs) of
        [] -> findPair base xs
        (y:_) -> Just (x, y)
  in do
    contents <- getContents
    putStrLn $ show $ fmap (foldl (*) 1) $ findTriplet $ map read $ lines contents
