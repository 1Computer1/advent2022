module Main (main) where

import Advent (input)
import Data.List.NonEmpty qualified as N
import Data.Map qualified as M
import Data.Set qualified as S
import Data.Stream.Infinite (Stream(..))
import Data.Stream.Infinite qualified as I
import Linear (V2(..))

main :: IO ()
main = do
    i <- $(input)
    print $ p1 i
    print $ p2 i

type Move = Int
type Block = [V2 Int]
type Solids = S.Set (V2 Int)

findRepeat :: Ord b => (a -> b) -> Stream a -> a
findRepeat f = go M.empty
    where
        go m (x :> xs) =
            let y = f x
            in if y `M.member` m
                then x
                else go (M.insert y x m) xs

blocks :: Stream (Int, Block)
blocks = I.cycle . N.fromList . zip [0..] $
    [ [V2 0 0, V2 1 0, V2 2 0, V2 3 0]
    , [V2 1 2, V2 0 1, V2 1 1, V2 2 1, V2 1 0]
    , [V2 2 2, V2 2 1, V2 0 0, V2 1 0, V2 2 0]
    , [V2 0 3, V2 0 2, V2 0 1, V2 0 0]
    , [V2 0 1, V2 1 1, V2 0 0, V2 1 0]
    ]

parse :: String -> Stream (Int, Move)
parse = I.cycle . N.fromList . zip [0..] . map (\case '>' -> 1; '<' -> -1) . head . lines

move :: Move -> Solids -> Block -> Block
move m solids xs =
    let ys = map (\(V2 x y) -> V2 (min 6 . max 0 $ x + m) y) xs
    in if any (\(x, y) -> x == y || y `S.member` solids) (zip xs ys)
        then xs
        else ys

initSolids :: Solids
initSolids = S.fromList [V2 x 0 | x <- [0..6]]

maxY :: Solids -> Int
maxY solids = maximum . map (\(V2 _ y) -> y) $ S.toList solids

simulateBlock :: Block -> Stream (Int, Move) -> Solids -> (Stream (Int, Move), Solids)
simulateBlock block moves solids = go blockInit moves
    where
        topY = maxY solids + 4
        blockInit = map (+ V2 2 topY) block
        go bl (m :> ms) =
            let bl'  = move (snd m) solids bl
                bl'' = map (+ V2 0 (-1)) bl'
                hit  = any (`S.member` solids) bl''
            in if hit
                then (ms, solids `S.union` S.fromList bl')
                else go bl'' ms

simulate :: Stream (Int, Block) -> Stream (Int, Move) -> Solids -> Stream (Int, Int, Int, Solids)
simulate = go 0
    where
        go !n (bl :> bls) ms@(m :> _) solids =
            let st = (n, fst bl, fst m, solids)
                (ms', solids') = simulateBlock (snd bl) ms solids
            in st :> go (n + 1) bls ms' solids'

p1 :: String -> Int
p1 inp = maxY solids
    where
        moves = parse inp
        states = simulate blocks moves initSolids
        ((_, _, _, solids) :> _) = I.filter (\(n, _, _, _) -> n == 2022) states

p2 :: String -> Int
p2 inp = (maxY solids3 - maxY solids2) * cycles + maxY solidsX
    where
        moves = parse inp
        states = simulate blocks moves initSolids

        (_, bi, mi, _) = findRepeat (\(_, x, y, _) -> (x, y)) states
        statesR = I.filter (\(_, x, y, _) -> x == bi && y == mi) states
        (_ :> (n2, _, _, solids2) :> (n3, _, _, solids3) :> _) = statesR

        period = n3 - n2
        (cycles, extra) = (1000000000000 - n2) `divMod` period

        statesX = simulate (I.drop bi blocks) (I.drop mi moves) solids2
        ((_, _, _, solidsX) :> _) = I.filter (\(n, _, _, _) -> n == extra) statesX
