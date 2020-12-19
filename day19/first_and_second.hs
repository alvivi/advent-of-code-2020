#!/usr/bin/env stack
-- stack script --resolver lts-14.16 --package containers

import Data.Char (isDigit)
import Data.IntMap.Strict (IntMap, (!))
import Text.ParserCombinators.ReadP
import qualified Data.IntMap.Strict as IntMap

data Rule = Chr Char | Go Int | And Rule Rule | Or Rule Rule
  deriving Show

rule :: ReadP (Int, Rule)
rule = ((,) <$> (num <* char ':' <* skipSpaces)) <*> (chr +++ go +++ and +++ or)
  where
    num = read <$> many1 (satisfy isDigit)
    chr = Chr <$> between (char '"') (char '"') get
    go = Go <$> num
    and = And <$> go <*> (char ' ' *> (go +++ and))
    or = Or <$> (go +++ and) <*> (string " | " *> go +++ and)

parse :: [String] -> IntMap Rule
parse = foldl step IntMap.empty
  where
    step acc line =
      let (idx, rule') = fst $ last $ readP_to_S rule line
      in IntMap.insert idx rule' acc

countMatches :: IntMap Rule -> [String] -> Int
countMatches rules = length . filter (("" `elem`) . step (Go 0))
  where
    step (Go i) s = step (rules ! i) s
    step (Chr c) (c':cs)
      | c == c' = [cs]
      | otherwise = []
    step (And l r) s = step l s >>= step r
    step (Or l r) s = step l s ++ step r s
    step _ [] = []

main :: IO ()
main = do
  (ruleContents, msgContents) <- break ((<= 0) . length) <$> lines <$> getContents
  let rules = parse ruleContents
  let messages = tail msgContents
  let l11 = (Or (And (Go 42) (Go 31)) (And (Go 42) (And (Go 11) (Go 31))))
  let l8 = (Or (Go 42) (And (Go 42) (Go 8)))
  let rules2 = IntMap.insert 11 l11 $ IntMap.insert 8 l8 rules
  let rr = snd . fst . last . readP_to_S rule
  print $ countMatches rules messages
  print $ countMatches rules2 messages
