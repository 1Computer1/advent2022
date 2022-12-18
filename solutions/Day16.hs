module Main (main) where

import Advent (input)
import Control.Arrow (first)
import Data.Char (isUpper)
import Data.Function ((&))
import Data.Graph.Inductive hiding ((&))
import Data.List (foldl', tails)
import Data.List.Split (splitOn)
import Data.Map.Strict qualified as M
import Data.Set qualified as S
import Data.Tree qualified as T

main :: IO ()
main = do
    i <- $(input)
    print $ p1 i
    print $ p2 i

each :: [a] -> (a -> b) -> [b]
each = flip map

parse :: String -> (Node, M.Map Node Int, M.Map Node (M.Map Node Int))
parse inp = (start, valves, dists)
    where
        xs = M.fromList . map (\(i, (v, r, vs)) -> (v, (i, r, vs))) . zip [0..] . map pLine $ lines inp
        pLine (splitOn ";" ->
            [ words -> [_, v, _, _, read . drop 5 -> r]
            , splitOn ", " . dropWhile (not . isUpper) -> vs
            ]) = (v, r, vs)

        gr = undir $ foldl' f empty xs :: Gr Int ()
        f acc (i, r, vs) = acc
            & insNode (i, r)
            & insEdges [(i, j, ()) | w <- vs, let (j, _, _) = xs M.! w]

        (start, _, _) = xs M.! "AA"
        valves = M.fromList . map (\(u, r, _) -> (u, r)) . filter (\(_, r, _) -> r > 0) $ M.elems xs
        dists = M.fromList . each (start : M.keys valves) $ \i ->
            ( i
            , M.fromList . each (bft i gr) $ \p ->
                (head p, length p - 1)
            )

stateTree :: Int -> M.Map Node (M.Map Node Int) -> Node -> M.Map Node Int -> T.Tree (Node, Int)
stateTree maxT dists start valves = T.unfoldTree buildNode (1, start, M.empty)
    where
        dist v w = dists M.! v M.! w

        nextStates (t, v, zs) =
            [ (t', w, M.insert w ((maxT - t' + 1) * valves M.! w) zs)
            | w <- M.keys (valves `M.difference` zs)
            , let t' = t + dist v w + 1
            , t' <= maxT
            ]

        buildNode (t, v, zs) =
            let succs = nextStates (t, v, zs)
            in ((v, foldl' (+) 0 zs), succs)

withPath :: T.Tree (Node, Int) -> T.Tree ([Node], Int)
withPath = go []
    where
        go p (T.Node (n, v) xs) = T.Node (n:p, v) (map (go (n:p)) xs)

bestBySubset :: T.Tree (Node, Int) -> M.Map (S.Set Node) Int
bestBySubset = M.fromListWith max . map (first S.fromList) . T.flatten . withPath

p1 :: String -> Int
p1 inp = maximum bests
    where
        (s, vs, dists) = parse inp
        tr = stateTree 30 dists s vs
        bests = bestBySubset tr

-- my old solution was very greedy...
-- it would take the optimal path for just the human and then give the unvisited rooms to the elephant to handle
-- it failed for the test input but gives the correct answer for the real input!
-- was fun while it lasted (ran pretty fast too)

p2 :: String -> Int
p2 inp = maximum [f1 + f2 | (p, f1) : rest <- tails (M.assocs bests), (q, f2) <- rest, p `S.intersection` q == S.singleton s]
    where
        (s, vs, dists) = parse inp
        tr = stateTree 26 dists s vs
        bests = bestBySubset tr
