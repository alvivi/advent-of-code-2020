#!/usr/bin/env stack
-- stack script --resolver lts-14.16

{-# LANGUAGE Strict #-}

cardPublicKey = 14082811
doorPublicKey = 5249543

step subject num = (num * subject) `mod` 20201227

find target subject count num | target == num = count
find target subject count num = find target subject (count + 1) $ step subject num

main :: IO ()
main = do
  let values = zip [0..] $ iterate (step 7) 1
  let cardLoopSize = find cardPublicKey 7 0 1
  print $ (!! cardLoopSize) $ iterate (step doorPublicKey) 1
