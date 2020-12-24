#!/usr/bin/env stack
-- stack script --resolver lts-14.16 --package containers

{-# LANGUAGE Strict #-}

import Data.Maybe (fromJust)
import Data.Sequence (Seq)
import qualified Data.Sequence as Seq
import qualified Data.Foldable as Foldable

parse :: String -> Seq Int
parse = Seq.fromList . map (read . (: []))

main :: IO ()
main = do
  print $ solve 100 $ parse "418976235"

solve :: Int -> Seq Int -> [Int]
solve count ring = tail (Foldable.toList rhs) ++ Foldable.toList lhs
  where
    ring' = play count ring
    oneIndex = fromJust $ Seq.elemIndexL 1 ring'
    (lhs, rhs) = Seq.splitAt oneIndex ring'

play :: Int -> Seq Int -> Seq Int
play count ring = play' count (Seq.index ring 0) ring
  where
    play' 0 current ring = ring
    play' n current ring =
      let
        (_, _, ring') = step current ring
        currentIndex = fromJust $ Seq.elemIndexL current ring'
        nextIndex = (currentIndex + 1) `mod` Seq.length ring'
      in
        play' (n - 1) (Seq.index ring' nextIndex) ring'

step :: Int -> Seq Int -> ([Int], Int, Seq Int)
step current ring = (slice, destination, ring'')
  where
    currentIndex = fromJust $ Seq.elemIndexL current ring
    sliceIndices = map (\d -> (currentIndex + d) `mod` Seq.length ring) [1..3]
    slice = map (Seq.index ring) sliceIndices
    destination = findDestination (current - 1)
    ring' = Seq.filter (not . (`elem` slice)) ring
    destinationIndex = fromJust $ Seq.elemIndexL destination ring'
    insertAfter _ [] acc = acc
    insertAfter at (x:xs) acc =  insertAfter target xs $ Seq.insertAt target x acc
      where target = (at + 1) `mod` Seq.length acc
    ring'' = insertAfter destinationIndex slice ring'
    findDestination n | n < 0 = findDestination 9
    findDestination n =
      case Seq.elemIndexL n ring of
        Just i | i `elem` sliceIndices -> findDestination (n - 1)
        Just i -> Seq.index ring i
        Nothing -> findDestination (n - 1)

