module Main (main) where

import Advent (input)
import Data.List.Split (splitOn)
import Numeric.Interval (Interval, (...), (==?), contains) -- math is hard

main :: IO ()
main = do
    i <- $(input)
    print $ p1 i
    print $ p2 i

-- psychotic, i know
parse :: String -> (Interval Int, Interval Int)
parse (splitOn "," -> [splitOn "-" -> [read -> ax, read -> ay], splitOn "-" -> [read -> bx, read -> by]]) = (ax ... ay, bx ... by)

p1 :: String -> Int
p1 = length . filter (\(a, b) -> a `contains` b || b `contains` a) . map parse . lines

p2 :: String -> Int
p2 = length . filter (\(a, b) -> a ==? b) . map parse . lines
