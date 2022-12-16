module Main (main) where

import Advent (input)
import Data.Char (isUpper)
import Data.Function ((&))
import Data.Graph.Inductive hiding ((&))
import Data.List (foldl', maximumBy)
import Data.List.Split (splitOn)
import Data.Map.Strict qualified as M
import Data.Ord (comparing)
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
        nextStates (t, v, zs) =  if
            | t >= maxT ->
                []
            | v /= start && v `M.notMember` zs ->
                [(t + 1, v, M.insert v ((maxT - t) * valves M.! v) zs)]
            | otherwise ->
                [(t + dists M.! v M.! w, w, zs) | w <- M.keys valves, w `M.notMember` zs]
        buildNode (t, v, zs) =
            let succs = nextStates (t, v, zs)
            in ((v, if null succs then foldl' (+) 0 zs else 0), succs)

leaves :: T.Tree (a, b) -> [([a], b)]
leaves (T.Node (n, v) []) = [([n], v)]
leaves (T.Node (n, _) xs) = map (\(ns, v) -> (n:ns, v)) $ concatMap leaves xs

p1 :: String -> Int
p1 inp = maximum . map snd . leaves $ tr
    where
        (s, vs, dists) = parse inp
        tr = stateTree 30 dists s vs

p2 :: String -> Int
p2 inp = f1 + f2
    where
        (s, vs1, dists) = parse inp
        (p, f1) = maximumBy (comparing snd) . leaves $ stateTree 26 dists s vs1
        vs2 = vs1 `M.withoutKeys` S.fromList p
        (_, f2) = maximumBy (comparing snd) . leaves $ stateTree 26 dists s vs2
