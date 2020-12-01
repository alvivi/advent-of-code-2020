#!/usr/bin/env stack
-- stack script --resolver lts-14.16

main :: IO ()
main =
  let
    findPair [] = Nothing
    findPair (x:xs) =
      case (filter (\y -> y + x == 2020) xs) of
        [] -> findPair xs
        (y:_) -> Just (x, y)
  in do
    contents <- getContents
    putStrLn $ show $ fmap (uncurry (*)) $ findPair $ map read $ lines contents
