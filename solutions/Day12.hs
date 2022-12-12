module Main (main) where

import Advent (input)
import Data.Char (ord)
import Data.Function ((&))
import Data.Graph.Inductive hiding ((&))
import Data.List (find, foldl')
import Data.Maybe (fromJust)
import GHC.Arr (listArray, inRange, assocs, (!))

main :: IO ()
main = do
    i <- $(input)
    print $ p1 i
    print $ p2 i

parse :: String -> (Gr Char (), Node, Node)
parse inp = (gr, s, t)
    where
        gr = foldl' f empty (assocs arr)
        f acc (ix, h) = acc
            & insNode (conv ix, h)
            & insEdges (map ((conv ix,, ()) . conv) $ outs ix h)

        outs (i, j) h = filter
            (\ix -> inRange b ix && ord (arr ! ix) - ord h <= 1)
            [(i + 1, j), (i - 1, j), (i, j + 1), (i, j - 1)]

        (s, t, arr) =
            let o = listArray b . concat . lines $ inp
                o' = fmap (\case 'S' -> 'a'; 'E' -> 'z'; x -> x) o
                indexOf x = conv . fst . fromJust . find ((== x) . snd) $ assocs o
            in (indexOf 'S', indexOf 'E', o')

        n = length (lines inp)
        m = length (head $ lines inp)
        b = ((0, 0), (n-1, m-1))
        conv (i, j) = i * m + j

p1 :: String -> Int
p1 (parse -> (gr, s, t)) = length (esp s t gr) - 1

p2 :: String -> Int
p2 (parse -> (gr, _, t)) = pred . length . head . filter ((== Just 'a') . lab gr . head) . bft t $ grev gr
