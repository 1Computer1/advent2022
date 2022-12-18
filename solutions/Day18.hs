module Main (main) where

import Advent (input)
import Data.List.Split (splitOn)
import Data.Set qualified as S
import Data.Sequence qualified as Q
import Data.Ix (inRange)
import Linear (V3(..))

main :: IO ()
main = do
    i <- $(input)
    print $ p1 i
    print $ p2 i

parse :: String -> S.Set (V3 Int)
parse = S.fromList . map ((\[a, b, c] -> V3 a b c) . map read . splitOn ",") . lines

neighbors :: V3 Int -> [V3 Int]
neighbors (V3 x y z) =
    [ V3 (x+1) y z
    , V3 (x-1) y z
    , V3 x (y+1) z
    , V3 x (y-1) z
    , V3 x y (z+1)
    , V3 x y (z-1)
    ]

p1 :: String -> Int
p1 inp = sum . map (length . filter (`S.notMember` lava) . neighbors) $ S.toList lava
    where lava = parse inp

p2 :: String -> Int
p2 inp = sum . map (length . filter (`S.member` exposedAir) . neighbors) $ S.toList surfaceLava
    where
        lava = parse inp
        minCoord l = pred . minimum . map l $ S.toList lava
        maxCoord l = succ . maximum . map l $ S.toList lava
        bounds =
            ( V3 (minCoord (.x)) (minCoord (.y)) (minCoord (.z))
            , V3 (maxCoord (.x)) (maxCoord (.y)) (maxCoord (.z))
            )

        search visited Q.Empty = visited
        search visited (here Q.:<| queue) =
            let xs = filter (\x -> x `S.notMember` visited && inRange bounds x) $ neighbors here
                next = filter (`S.notMember` lava) xs
                visited' = visited `S.union` S.fromList xs
                queue' = queue <> Q.fromList next
            in search visited' queue'

        outside = search (S.singleton (fst bounds)) (Q.singleton (fst bounds))
        (surfaceLava, exposedAir) = S.partition (`S.member` lava) outside
