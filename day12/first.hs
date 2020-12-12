#!/usr/bin/env stack
-- stack script --resolver lts-14.16

main :: IO ()
main =
  let
    toLeft 90 ( 1,  0) = ( 0,  1)
    toLeft 90 ( 0,  1) = (-1,  0)
    toLeft 90 (-1,  0) = ( 0, -1)
    toLeft 90 ( 0, -1) = ( 1,  0)
    toLeft deg pos = toLeft (deg - 90) $ toLeft 90 pos
    toRight 90 ( 1,  0) = ( 0, -1)
    toRight 90 ( 0, -1) = (-1,  0)
    toRight 90 (-1,  0) = ( 0,  1)
    toRight 90 ( 0,  1) = ( 1,  0)
    toRight deg pos = toRight (deg - 90) $ toRight 90 pos
    step ((px, py), dir) ('N':dist) = ((px, py + read dist), dir)
    step ((px, py), dir) ('S':dist) = ((px, py - read dist), dir)
    step ((px, py), dir) ('E':dist) = ((px + read dist, py), dir)
    step ((px, py), dir) ('W':dist) = ((px - read dist, py), dir)
    step (pos, dir) ('L':deg) = (pos, toLeft (read deg) dir)
    step (pos, dir) ('R':deg) = (pos, toRight (read deg) dir)
    step ((px, py), dir@(dx, dy)) ('F':dist) = let dist' = read dist in ((px + dx * dist', py + dy * dist'), dir)
  in do
    contents <- getContents
    let ((x, y), _) = foldl step ((0, 0), (1, 0)) $ lines contents
    putStrLn $ show $ abs x + abs y
