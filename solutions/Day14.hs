module Main (main) where

import Advent (input)
import Data.List (foldl', find)
import Data.List.Split (splitOn)
import Data.Set qualified as S
import Linear.V2 (V2(..))

main :: IO ()
main = do
    i <- $(input)
    print $ p1 i
    print $ p2 i

source :: V2 Int
source = V2 500 0

parse :: String -> (S.Set (V2 Int), Int)
parse inp = (solids, maxY)
    where
        ess = map (map ((\[x, y] -> (x, y)) . map read . splitOn ",") . splitOn " -> ") $ lines inp

        solids = foldl' setRocks S.empty ess
        setRocks a es = foldl' setWall a (zip es (tail es))
        setWall a (s, t) = a `S.union` S.fromList (dir (uncurry V2 s) (uncurry V2 t))

        maxY = (+1) . maximum . map snd . concat $ ess

dir :: V2 Int -> V2 Int -> [V2 Int]
dir a b = take (n + 1) $ iterate (+ u) a
    where
        u = fmap signum $ b - a
        n = abs $ sum $ b - a

dropSand :: S.Set (V2 Int) -> Int -> V2 Int
dropSand solids limit = go source
    where
        go i = case spaceUnder solids i of
            Just j | V2 _ y <- j, y <= limit -> go j
            _ -> i

spaceUnder :: S.Set (V2 Int) -> V2 Int -> Maybe (V2 Int)
spaceUnder solids i = find (`S.notMember` solids) [i + V2 0 1, i + V2 (-1) 1, i + V2 1 1]

p1 :: String -> Int
p1 inp = go solids1 0
    where
        (solids1, maxY) = parse inp
        go s !n = if y >= maxY - 2
            then n
            else go (S.insert end s) (n + 1)
            where
                end@(V2 _ y) = dropSand s maxY

p2 :: String -> Int
p2 inp = go solids1 0
    where
        (solids1, maxY) = parse inp
        go s !n = if end == source
            then n + 1
            else go (S.insert end s) (n + 1)
            where
                end = dropSand s maxY
