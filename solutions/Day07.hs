module Main (main) where

import Advent (input)
import Data.List (inits, foldl')
import Data.Map.Strict qualified as M
import Text.Read (readMaybe)

main :: IO ()
main = do
    i <- $(input)
    print $ p1 i
    print $ p2 i

parse :: String -> M.Map [String] Int
parse inp = fst $ foldl' (flip $ f . words) (M.empty, ["/"]) (lines inp)
    where
        f ["$", "cd", x] (m, p)
            | x == "/"  = (m, ["/"])
            | x == ".." = (m, init p)
            | otherwise = (m, p <> [x])
        f [readMaybe -> Just size, _] (m, p)
            = (foldl' (flip $ M.alter (Just . maybe size (+ size))) m (tail $ inits p), p)
        f _ (m, p) = (m, p)

p1 :: String -> Int
p1 = sum . M.filter (<= 100000) . parse

p2 :: String -> Int
p2 inp = minimum $ M.filter (\x -> unused + x >= 30000000) fs
    where
        fs = parse inp
        unused = 70000000 - fs M.! ["/"]
