module Main (main) where

import Advent (input)

main :: IO ()
main = do
    i <- $(input)
    print $ p1 i
    print $ p2 i

data RPS = R | P | S deriving (Eq, Ord, Enum)

-- oh no all the functions are partial

parse :: String -> RPS
parse "A" = R
parse "B" = P
parse "C" = S
parse "X" = R
parse "Y" = P
parse "Z" = S

withResult :: RPS -> String -> RPS
withResult a "X" = toEnum $ (fromEnum a - 1) `mod` 3
withResult a "Y" = a
withResult a "Z" = toEnum $ (fromEnum a - 2) `mod` 3

score :: RPS -> Int
score = succ . fromEnum

result :: RPS -> RPS -> Int
result opp you = case (fromEnum opp - fromEnum you) `mod` 3 of
    0 -> 3
    1 -> 0
    2 -> 6

p1 :: String -> Int
p1 = sum . map f . lines
    where f x = let [parse -> opp, parse -> you] = words x in result opp you + score you

p2 :: String -> Int
p2 = sum . map f . lines
    where f x = let [parse -> opp, withResult opp -> you] = words x in result opp you + score you
