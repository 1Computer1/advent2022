module Main (main) where

import Advent (input)
import Data.Bifunctor (second)
import Data.Foldable (foldl')
import Data.Maybe (fromJust)
import Data.Sequence qualified as S

main :: IO ()
main = do
    i <- $(input)
    print $ p1 i
    print $ p2 i

parse :: String -> S.Seq (Int, Int)
parse = S.fromList . zip [0..] . map read . lines

moveAll :: S.Seq (Int, Int) -> S.Seq (Int, Int) -> S.Seq (Int, Int)
moveAll original xs = foldl' f xs original
    where
        n = length xs - 1
        f acc e@(_, x) =
            let i = fromJust (S.elemIndexL e acc)
                j = (i + x) `rem` n
                k = if
                    | j < 0     -> j + n
                    | otherwise -> j
            in S.insertAt k e (S.deleteAt i acc)

p1 :: String -> Int
p1 inp = sum [snd $ S.index ys ((z + 1000) `mod` n), snd $ S.index ys ((z + 2000) `mod` n), snd $ S.index ys ((z + 3000) `mod` n)]
    where
        xs = parse inp
        ys = moveAll xs xs
        n = length xs
        z = fromJust (S.findIndexL ((== 0) . snd) ys)

p2 :: String -> Int
p2 inp = sum [snd $ S.index ys ((z + 1000) `mod` n), snd $ S.index ys ((z + 2000) `mod` n), snd $ S.index ys ((z + 3000) `mod` n)]
    where
        xs = fmap (second (* 811589153)) $ parse inp
        ys = iterate (moveAll xs) xs !! 10
        n = length xs
        z = fromJust (S.findIndexL ((== 0) . snd) ys)
