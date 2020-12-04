#!/usr/bin/env stack
-- stack script --resolver lts-14.16 --package containers

import qualified Data.Map as Map

main :: IO ()
main =
  let
    parseItems lines =
      let (item, list) = foldl parseLine (Map.empty, []) lines
      in item : list
    parseLine (item, list) "" = (Map.empty, item : list)
    parseLine (item, list) line =
      let
        insert item (key, (':' : value)) = Map.insert key value item
        updatedItem = foldl insert item $ map (break (':' ==)) $ words line
      in
        (updatedItem, list)
    valid item =
      all (`Map.member` item) ["byr", "iyr", "eyr", "hgt", "hcl", "ecl", "pid"]
  in do
    contents <- getContents
    putStrLn $ show $ sum $ map (fromEnum . valid) $ parseItems $ lines contents
