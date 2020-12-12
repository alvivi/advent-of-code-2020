#!/usr/bin/env stack
-- stack script --resolver lts-14.16

main :: IO ()
main =
  let
    step (pos, (wx, wy)) ('N':dist) = (pos, (wx, wy + read dist))
    step (pos, (wx, wy)) ('S':dist) = (pos, (wx, wy - read dist))
    step (pos, (wx, wy)) ('E':dist) = (pos, (wx + read dist, wy))
    step (pos, (wx, wy)) ('W':dist) = (pos, (wx - read dist, wy))
    step (pos, way) ('L':deg) = (pos, rotate (read deg) way)
    step (pos, way) ('R':deg) = (pos, rotate (negate $ read deg) way)
    step ((px, py), way@(wx, wy)) ('F':dist) = let dist' = read dist in ((px + wx * dist', py + wy * dist'), way)
    toRad deg = deg * pi / 180
    rotate deg (x, y) =
      let
        deg' = fromInteger deg
        x' = fromInteger x
        y' = fromInteger y
        rx = toInteger $ round $ cos (toRad deg') * x' - sin (toRad deg') * y'
        ry = toInteger $ round $ sin (toRad deg') * x' + cos (toRad deg') * y'
      in
        (rx, ry)
  in do
    contents <- getContents
    let ((x, y), _) = foldl step ((0, 0), (10, 1)) $ lines contents
    putStrLn $ show $ abs x + abs y
