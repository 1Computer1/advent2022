module Main (main) where

import Advent (input)
import Data.Char (ord, isUpper, isLower)
import Data.List.Split (chunksOf)
import Data.Set qualified as S

main :: IO ()
main = do
    i <- $(input)
    print $ p1 i
    print $ p2 i

prio :: Char -> Int
prio c | isLower c = ord c - ord 'a' + 1
prio c | isUpper c = ord c - ord 'A' + 27

p1 :: String -> Int
p1 = sum . map f . lines
    where f x = let (a, b) = splitAt (length x `div` 2) x in prio . S.elemAt 0 $ S.fromList a `S.intersection` S.fromList b

p2 :: String -> Int
p2 = sum . map (\xs -> prio . S.elemAt 0 . foldr1 S.intersection $ map S.fromList xs) . chunksOf 3 . lines
