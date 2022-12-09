module Main (main) where

import Advent (input)
import Data.List (mapAccumR)
import Data.Set qualified as S

main :: IO ()
main = do
    i <- $(input)
    print $ p1 i
    print $ p2 i

data Move = L | R | U | D
    deriving (Show, Read, Eq, Ord)

parse :: String -> [(Move, Int)]
parse = map (f . words) . lines
    where f [a, b] = (read a, read b)

move :: Move -> (Int, Int) -> (Int, Int)
move L (x, y) = (x - 1, y)
move R (x, y) = (x + 1, y)
move U (x, y) = (x, y - 1)
move D (x, y) = (x, y + 1)

moveTail :: (Int, Int) -> (Int, Int) -> (Int, Int) -> (Int, Int)
moveTail (hx, hy) (hx', hy') t@(x, y) = if
    | dx == 2 && dy == 2 -> (x + hx' - hx, y + hy' - hy)
    | dx == 2            -> (x + hx' - hx, hy')
    | dy == 2            -> (hx', y + hy' - hy)
    | otherwise          -> t
    where
        dx = abs $ hx' - x
        dy = abs $ hy' - y

p1 :: String -> Int
p1 = length . go (S.singleton (0, 0)) (0, 0) (0, 0) . parse
    where
        go s _ _ [] = s
        go s h t ((_, 0):ms) = go s h t ms
        go s h t ((m, n):ms) =
            let h' = move m h
                t' = moveTail h h' t
                s' = S.insert t' s
            in go s' h' t' ((m, n-1):ms)

p2 :: String -> Int
p2 = length . go (S.singleton (0, 0)) (0, 0) (replicate 9 (0, 0)) . parse
    where
        go s _ _ [] = s
        go s h ts ((_, 0):ms) = go s h ts ms
        go s h ts ((m, n):ms) =
            let h' = move m h
                ts' = snd $ mapAccumR (\(x, x') t -> let t' = moveTail x x' t in ((t, t'), t')) (h, h') ts
                s' = S.insert (head ts') s
            in go s' h' ts' ((m, n-1):ms)
