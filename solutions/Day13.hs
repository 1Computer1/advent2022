module Main (main) where

import Advent (input)
import Control.Applicative ((<|>), some)
import Data.Either (fromRight)
import Data.List.Split (splitOn)
import Data.List (sort, findIndices)
import Text.Parsec qualified as P

main :: IO ()
main = do
    i <- $(input)
    print $ p1 i
    print $ p2 i

data P = L [P] | I Int
    deriving (Eq)

instance Ord P where
    compare :: P -> P -> Ordering
    compare (I x)  (I y)  = compare x y
    compare (L xs) (L ys) = compare xs ys
    compare (L xs) (I x)  = compare (L xs) (L [I x])
    compare (I x)  (L xs) = compare (L [I x]) (L xs)

parse :: String -> [(P, P)]
parse = map (\[x, y] -> (p x, p y)) . map lines . splitOn "\n\n"
    where
        p = fromRight (error "uh oh") . P.parse f ""
        f = (L <$> (P.char '[' *> P.sepBy f (P.char ',') <* P.char ']')) <|> (I . read <$> some P.digit)

p1 :: String -> Int
p1 = sum . map fst . filter (uncurry (<) . snd) . zip [1..] . parse

p2 :: String -> Int
p2 inp = product . map succ . findIndices (`elem` [a, b]) $ xs
    where
        a = L [L [I 2]]
        b = L [L [I 6]]
        xs = sort . concatMap (\(x, y) -> [x, y]) $ (a, b) : parse inp
