module Main (main) where

-- today is the day the power tools get thrown in

import Advent (input)
import Control.Monad
import Control.Monad.State.Strict
import Data.Bifunctor (bimap)
import Data.Char (isNumber)
import Data.List (stripPrefix)
import Data.List.Split (splitOn)
import Data.Maybe (fromMaybe)
import Data.Ord (comparing, Down (..))
import Data.Sequence qualified as S
import GHC.Generics (Generic)
import Text.Read (readMaybe)
import Optics
import Optics.State.Operators

main :: IO ()
main = do
    i <- $(input)
    print $ p1 i
    print $ p2 i

data Monkey = Monkey
    { items       :: [Int]
    , numInspects :: Int
    , affectWorry :: Int -> Int
    , getTarget   :: Int -> Int
    }
    deriving (Generic)

-- more psychotic parsing
-- maybe i should just disable incomplete pattern matches
parse :: String -> (Int, S.Seq Monkey)
parse = bimap product S.fromList . unzip . map f . splitOn "\n\n"
    where
        f (lines ->
            [ _
            , stripPrefix "  Starting items: "      -> Just (map read . splitOn ", "           -> items)
            , stripPrefix "  Operation: new = old " -> Just (words                             -> [operator, operand])
            , stripPrefix "  Test: divisible by "   -> Just (read                              -> divisor)
            , stripPrefix "    If true: "           -> Just (read . dropWhile (not . isNumber) -> targetT)
            , stripPrefix "    If false: "          -> Just (read . dropWhile (not . isNumber) -> targetF)
            ]) =
                ( divisor
                , Monkey
                    { items
                    , numInspects = 0
                    , affectWorry = \x ->
                        let g = case operator of "*" -> (*); "+" -> (+)
                            y = fromMaybe x $ readMaybe operand
                        in g x y
                    , getTarget = \x ->
                        if x `mod` divisor == 0
                            then targetT
                            else targetF
                    }
                )

result :: S.Seq Monkey -> Int
result = product . S.take 2 . S.sortBy (comparing Down) . fmap numInspects

-- who says you cant mix optics labels and record dot syntax?
-- its so ugly

p1 :: String -> Int
p1 inp = result $ execState (replicateM_ 20 runRound) (snd $ parse inp)
    where
        runRound = do
            n <- gets length
            forM_ [0..n-1] $ \i -> do
                m <- gets (`S.index` i)
                forM_ m.items $ \w -> do
                    let w' = m.affectWorry w `div` 3
                    let target = m.getTarget w'
                    ix target % #items %= cons w'
                ix i % #numInspects %= (+ length m.items)
                ix i % #items .= []

p2 :: String -> Int
p2 inp = result $ execState (replicateM_ 10000 runRound) monkeys
    where
        (d, monkeys) = parse inp
        runRound = do
            n <- gets length
            forM_ [0..n-1] $ \i -> do
                m <- gets (`S.index` i)
                forM_ m.items $ \w -> do
                    let w' = m.affectWorry w `mod` d
                    let target = m.getTarget w'
                    ix target % #items %= cons w'
                ix i % #numInspects %= (+ length m.items)
                ix i % #items .= []
