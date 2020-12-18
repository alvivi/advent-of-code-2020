#!/usr/bin/env stack
-- stack script --resolver lts-14.16

import Data.Char
import Text.ParserCombinators.ReadP

num :: ReadP Int
num = read <$> many1 (satisfy isDigit)

parser :: ReadP Int
parser = prod +++ add
  where
    prod = (*) <$> add <*> (skipSpaces *> string "*" *> skipSpaces *> parser)
    add = ((+) <$> group <*> (skipSpaces *> string "+" *> skipSpaces *> add)) +++ group
    group = (between (char '(') (char ')') parser) +++ num

main :: IO ()
main = do
  results <- map (fst . last .readP_to_S parser) <$> lines <$> getContents
  putStrLn $ show $ sum results
