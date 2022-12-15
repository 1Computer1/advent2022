module Main (main, away) where

import Advent (input)
import Data.List (find)
import Linear (V2(..))

main :: IO ()
main = do
    i <- $(input)
    print $ p1 i
    print $ p2 i

parse :: String -> [(V2 Int, V2 Int)]
parse inp = ixs
    where
        ixs = map (f . words) . lines $ inp
        f [ _, _
          , read . init . drop 2 -> x1
          , read . init . drop 2 -> y1
          , _, _, _, _
          , read . init . drop 2 -> x2
          , read . drop 2 -> y2
          ] = (V2 x1 y1, V2 x2 y2)

dist :: V2 Int -> V2 Int -> Int
dist (V2 x1 y1) (V2 x2 y2) = abs (x2 - x1) + abs (y2 - y1)

p1 :: String -> Int
p1 inp = length [i | i <- [minX..maxX], any (\(s, b, d) -> V2 i j /= b && dist s (V2 i j) <= d) ranges]
    where
        j = 2000000
        ranges = map (\(s, b) -> (s, b, dist s b)) (parse inp)
        minX = minimum . map (\(V2 sx _, _, d) -> sx - d) $ ranges
        maxX = maximum . map (\(V2 sx _, _, d) -> sx + d) $ ranges

p2 :: String -> Int
p2 inp = go 1
    where
        e = 4000000
        ranges = map (\(s, b) -> (s, dist s b)) (parse inp)
        inRange (V2 i j) = 0 <= i && 0 <= j && i <= e && j <= e
        go k =
            let kAway = filter inRange $ concatMap (\(s, d) -> s `away` (d + k)) ranges
            in case find (\ix -> all (\(s, d) -> dist s ix > d) ranges) kAway of
                Nothing -> go (k + 1)
                Just (V2 i j) -> i * e + j

away :: V2 Int -> Int -> [V2 Int]
away s d = map (+ s) $ concat [if abs i == d then [V2 i 0] else [V2 i (d - abs i), V2 i (abs i - d)] | i <- [-d..d]]
