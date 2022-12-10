module Main (main) where

import Advent (input)
import Data.Set qualified as S

main :: IO ()
main = do
    i <- $(input)
    print $ p1 i
    putStrLn $ p2 i

data Op = Addx Int | Noop

parse :: String -> [(Op, Int)]
parse = map (f . words) . lines
    where
        f ["noop"] = (Noop, 0)
        f ["addx", read -> x] = (Addx x, 1)

doOp :: Op -> Int -> Int
doOp (Addx n) x = x + n
doOp Noop x = x

p1 :: String -> Int
p1 = go 1 1 0 . parse
    where
        go _ _ n [] = n
        go x c n ((op, k):ops) =
            let c' = c + 1
                x' = if k == 0 then doOp op x else x
                n' = if c' `elem` [20, 60..220] then n + (c' * x') else n
                ops' = if k == 0 then ops else (op, k-1) : ops
            in go x' c' n' ops'

p2 :: String -> String
p2 = pretty . go 1 1 [] . parse
    where
        go _ _ ss [] = ss
        go x c ss ((op, k):ops) =
            let pixel = (c-1) `mod` 40
                (row:rows) = if pixel == 0 then S.empty : ss else ss
                ss' = if pixel `elem` [x-1, x, x+1] then (S.insert pixel row) : rows else ss
                c' = c + 1
                x' = if k == 0 then doOp op x else x
                ops' = if k == 0 then ops else (op, k-1) : ops
            in go x' c' ss' ops'

        pretty s = unlines [[if col `S.member` row then '#' else '.' | col <- [0..39]] | row <- reverse s]
