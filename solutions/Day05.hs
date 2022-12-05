module Main (main) where

import Advent (input)
import Data.List.Split (chunksOf, splitOn)
import Data.IntMap qualified as M
import Data.List (find, transpose)
import Data.Char (isLetter)
import Data.Maybe (catMaybes, fromJust)

main :: IO ()
main = do
    i <- $(input)
    print $ p1 i
    print $ p2 i

parse :: String -> (M.IntMap [Char], [(Int, Int, Int)])
parse inp =
    let [stackInp, opInp] = splitOn "\n\n" inp
        stacks = map catMaybes . transpose $ map (map (find isLetter) . chunksOf 4) . init $ lines stackInp
        ops = map (\[_, x, _, a, _, b] -> (read x, read a, read b)) . map words $ lines opInp
    in (M.fromList $ zip [1..] stacks, ops)

p1 :: String -> String
p1 inp = map head . M.elems $ uncurry go (parse inp)
    where
        go m [] = m
        go m ((n, x, y):ops) =
            let (reverse -> sx, sx') = fromJust $ splitAt n <$> M.lookup x m
                m' = M.alter (Just . maybe sx (sx <>)) y m
                m'' = M.insert x sx' m'
            in go m'' ops

p2 :: String -> String
p2 inp = map head . M.elems $ uncurry go (parse inp)
    where
        go m [] = m
        go m ((n, x, y):ops) =
            let (sx, sx') = fromJust $ splitAt n <$> M.lookup x m
                m' = M.alter (Just . maybe sx (sx <>)) y m
                m'' = M.insert x sx' m'
            in go m'' ops
