module Main (main) where

import Advent (input)
import Data.List (sort)
import Data.List.Split (splitOn)

main :: IO ()
main = do
    i <- $(input)
    print $ p1 i
    print $ p2 i

p1 :: String -> Int
p1 = maximum . map (sum . map read . lines) . splitOn "\n\n"

p2 :: String -> Int
p2 = sum . take 3 . reverse . sort . map (sum . map read . lines) . splitOn "\n\n"
