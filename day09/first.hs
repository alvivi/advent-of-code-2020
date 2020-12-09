#!/usr/bin/env stack
-- stack script --resolver lts-14.16 --package containers

import qualified Data.Set as Set

main :: IO ()
main =
  let
    deleteAt i xs = let (lhs, (_:rhs)) = splitAt i xs in lhs ++ rhs
    buildSet = foldl (flip Set.insert) Set.empty
    alts list = map (\i -> (list !! i, deleteAt i list)) [0..(length list - 1)]
    sums (x, xs) = map (+ x) xs
    check preambleSize list index =
      let
        preamble = take preambleSize $ drop (index - preambleSize) list
        preambleSet = buildSet $ concat $ map sums $ alts preamble
      in
        Set.member (list !! index) preambleSet
  in do
    numbers <- map read <$> lines <$> getContents
    let (first:_) = dropWhile (check 25 numbers) [25..(length numbers - 1)]
    putStrLn $ show $ numbers !! first
