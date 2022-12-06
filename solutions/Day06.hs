module Main (main) where

import Advent (input)
import Data.List (tails, transpose, findIndex)
import Data.Maybe (fromJust)
import Data.Set qualified as S

main :: IO ()
main = do
    i <- $(input)
    print $ p1 i
    print $ p2 i

p1 :: String -> Int
p1 = (+4) . fromJust . findIndex (\xs -> length (S.fromList xs) == 4) . transpose . take 4 . tails

p2 :: String -> Int
p2 = (+14) . fromJust . findIndex (\xs -> length (S.fromList xs) == 14) . transpose . take 14 . tails
