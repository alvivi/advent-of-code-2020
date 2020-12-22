#!/usr/bin/env stack
-- stack script --resolver lts-14.16 --package containers

import Data.Sequence (Seq, (|>))
import Data.Set (Set)
import qualified Data.Foldable as Foldable
import qualified Data.Sequence as Seq
import qualified Data.Set as Set

type State = (Seq Int, Seq Int)
type PlayedSet = Set ([Int], [Int])

parse :: String -> (Seq Int, Seq Int)
parse contents = (toSeq $ tail lhs, toSeq $ drop 2 rhs)
  where
    (lhs, rhs) = break ((== 0) . length) $ lines contents
    toSeq = Seq.fromList . map read

step :: PlayedSet -> State -> Maybe (PlayedSet, State)
step playedSet (first, second) =
  let
    played = (Foldable.toList first, Foldable.toList second)
    playedSet' = Set.insert played playedSet
  in
    if Set.member played playedSet then
      Just (playedSet', (first, Seq.empty))
    else
      case (seqHead first, seqHead second) of
        (Nothing, _) ->
          Nothing
        (_, Nothing) ->
          Nothing
        (Just fHead, Just sHead) | fHead <= (Seq.length first - 1) && sHead <= (Seq.length second - 1) ->
          case play Set.empty (Seq.take fHead $ seqTail first, Seq.take sHead $ seqTail second) of
            (_, (seq, _)) | Seq.length seq > 0 ->
              Just (playedSet', (seqTail first |> fHead |> sHead, seqTail second))
            (_, (_, seq)) | Seq.length seq > 0 ->
              Just (playedSet', (seqTail first, seqTail second |> sHead |> fHead))
        (Just fHead, Just sHead) | fHead > sHead ->
          Just (playedSet', (seqTail first |> fHead |> sHead, seqTail second))
        (Just fHead, Just sHead) | fHead < sHead ->
          Just (playedSet', (seqTail first, seqTail second |> sHead |> fHead))

play :: PlayedSet -> State -> (PlayedSet, State)
play playedSet state =
  case step playedSet state of
    Nothing -> (playedSet, state)
    Just (playedSet', state') -> play playedSet' state'

seqHead :: Seq a -> Maybe a
seqHead seq =
  case Seq.viewl seq of
    Seq.EmptyL -> Nothing
    ((Seq.:<) head _) -> Just head

seqTail :: Seq a -> Seq a
seqTail seq | Seq.length seq <= 1 = Seq.empty
seqTail seq = Seq.index (Seq.tails seq) 1

computeResult :: State -> Int
computeResult (seq, _) | Seq.length seq > 0 = countResult seq
computeResult (_, seq) | Seq.length seq > 0 = countResult seq

countResult :: Seq Int -> Int
countResult = foldl mult 0 . zip [1..] . reverse . Foldable.toList
  where mult acc (n, m) = acc + n * m

main :: IO ()
main = do
  (first, second) <- parse <$> getContents
  print $ computeResult $ snd $play Set.empty (first, second)
