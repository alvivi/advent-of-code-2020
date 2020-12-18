#!/usr/bin/env stack
-- stack script --resolver lts-14.16

import Data.Char
import Data.Function ((&))
import Text.ParserCombinators.ReadP

num :: ReadP Int
num = read <$> many1 (satisfy isDigit)

parser :: ReadP Int
parser = foldl (&) <$> group <*> (many (($) <$> op <*> group))
  where
    op = (((+) <$ string "+") +++ ((*) <$ string "*")) <* skipSpaces
    group = ((between (char '(') (char ')') parser) +++ num) <* skipSpaces

main :: IO ()
main = do
  results <- map (fst . last .readP_to_S parser) <$> lines <$> getContents
  putStrLn $ show $ sum results
