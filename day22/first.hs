#!/usr/bin/env stack
-- stack script --resolver lts-14.16 --package containers

import Data.Sequence (Seq, (|>))
import qualified Data.Sequence as Seq
import qualified Data.Foldable as Foldable

parse :: String -> (Seq Int, Seq Int)
parse contents = (toSeq $ tail lhs, toSeq $ drop 2 rhs)
  where
    (lhs, rhs) = break ((== 0) . length) $ lines contents
    toSeq = Seq.fromList . map read

step :: (Seq Int, Seq Int) -> Maybe (Seq Int, Seq Int)
step (first, second) =
  case (seqHead first, seqHead second) of
    (Nothing, _) -> Nothing
    (_, Nothing) -> Nothing
    (Just fHead, Just sHead) | fHead > sHead -> Just (seqTail first |> fHead |> sHead, seqTail second)
    (Just fHead, Just sHead) | fHead < sHead -> Just (seqTail first, seqTail second |> sHead |> fHead)

play :: (Seq Int, Seq Int) -> (Seq Int, Seq Int)
play state =
  case step state of
    Nothing -> state
    Just state' -> play state'

seqHead :: Seq a -> Maybe a
seqHead seq =
  case Seq.viewl seq of
    Seq.EmptyL -> Nothing
    ((Seq.:<) head _) -> Just head

seqTail :: Seq a -> Seq a
seqTail seq | Seq.length seq <= 1 = Seq.empty
seqTail seq = Seq.index (Seq.tails seq) 1

computeResult :: (Seq Int, Seq Int) -> Int
computeResult (seq, _) | Seq.length seq > 0 = countResult seq
computeResult (_, seq) | Seq.length seq > 0 = countResult seq

countResult :: Seq Int -> Int
countResult = foldl mult 0 . zip [1..] . reverse . Foldable.toList
  where mult acc (n, m) = acc + n * m

main :: IO ()
main = do
  (first, second) <- parse <$> getContents
  print $ computeResult $ play (first, second)
