#!/usr/bin/env stack
-- stack script --resolver lts-14.16 --package containers

import Data.Maybe (fromMaybe)
import qualified Data.Map as Map
import qualified Data.Set as Set

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
      let
        validHeight (cm, "cm") = (\v -> 150 <= v && v <= 193) $ read cm
        validHeight (in', "in") = (\v -> 59 <= v && v <= 76) $ read in'
        validHeight _ = False
        validHex ('#':cs) = all (\v -> (v >= '0' && v <= '9') || (v >= 'a' && v <= 'f')) cs && length cs == 6
        validHex _ = False
        validPid num = all (\v -> v >= '0' && v <= '9') num && length num == 9
        ecls = Set.fromList ["amb", "blu", "brn", "gry", "grn", "hzl", "oth"]
        required = all (`Map.member` item) ["byr", "iyr", "eyr", "hgt", "hcl", "ecl", "pid"]
        byr = (\v -> 1920 <= v && v <= 2002) $ fromMaybe 0 $ fmap read $ Map.lookup "byr" item
        iyr = (\v -> 2010 <= v && v <= 2020) $ fromMaybe 0 $ fmap read $ Map.lookup "iyr" item
        eyr = (\v -> 2020 <= v && v <= 2030) $ fromMaybe 0 $ fmap read $ Map.lookup "eyr" item
        hgt = validHeight $ span (\c -> c >= '0' && c <= '9') $ fromMaybe "" $ Map.lookup "hgt" item
        hcl = validHex $ fromMaybe "" $ Map.lookup "hcl" item
        ecl = (`Set.member` ecls) $ fromMaybe "" $ Map.lookup "ecl" item
        pid = validPid $ fromMaybe "" $ Map.lookup "pid" item
      in
        all id [required, byr, iyr, eyr, hgt, hcl, ecl, pid]
  in do
    contents <- getContents
    putStrLn $ show $ sum $ map (fromEnum . valid) $ parseItems $ lines contents
