module Main (main) where

import Advent (input)
import GHC.Arr (Array, listArray, assocs, bounds, (!))

main :: IO ()
main = do
    i <- $(input)
    print $ p1 i
    print $ p2 i

parse :: String -> Array (Int, Int) Int
parse inp = listArray ((0, 0), (n-1, n-1)) [read (pure x) | l <- lines inp, x <- l]
    where n = length (lines inp)

withLos :: Array (Int, Int) Int -> [(Int, [Int], [Int], [Int], [Int])]
withLos a = map f (assocs a)
    where
        n = snd . snd $ bounds a
        f ((i, j), x) =
            -- it doesn't actually matter what directions these correspond to...
            -- hooray for not having to figure out what to name them!
            let q = [a ! (i, k) | k <- [j-1,j-2..0]]
                w = [a ! (i, k) | k <- [j+1,j+2..n]]
                e = [a ! (k, j) | k <- [i-1,i-2..0]]
                r = [a ! (k, j) | k <- [i+1,i+2..n]]
            in (x, q, w, e, r)

p1 :: String -> Int
p1 inp = n * n - length (filter p $ withLos a)
    where
        a = parse inp
        n = length (lines inp)
        p (x, q, w, e, r) = any (>= x) q && any (>= x) w && any (>= x) e && any (>= x) r

p2 :: String -> Int
p2 inp = maximum $ map f (withLos a)
    where
        a = parse inp
        f (x, q, w, e, r) = l (span (< x) q) * l (span (< x) w) * l (span (< x) e) * l (span (< x) r)
        l (xs, ys) = length xs + (min 1 $ length ys)
