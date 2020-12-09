#!/usr/bin/env stack
-- stack script --resolver lts-14.16

import Data.Maybe (isJust, fromJust)
import qualified Data.List as List

main :: IO ()
main =
  let
    walk list target size =
      let sublist = take size list
      in if target == sum sublist then Just sublist else Nothing
    find list target = map (walk list target) [1..(length list)]
    findFirst [] target = Nothing
    findFirst list target =
      case List.find isJust $ find list target of
        Just result -> result
        Nothing -> findFirst (tail list) target
  in do
    let target = 70639851
    numbers <- takeWhile (/= target) <$> map read <$> lines <$> getContents :: IO [Int]
    let result = fromJust $ findFirst numbers target
    putStrLn $ show $ minimum result + maximum result
