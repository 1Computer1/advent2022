module Main (main) where

import Advent (input)
import Data.Foldable (toList)
import Data.Map.Strict qualified as M

main :: IO ()
main = do
    i <- $(input)
    print $ p1 i
    print $ p2 i

data Monkey = MI Int | MO Op String String

data Op = A | S | M | D

data Expr k = O k Op (Expr k) (Expr k) | I k Int | X k
    deriving (Functor, Foldable)

ann :: Expr k -> k
ann = head . toList

eval :: Expr a -> Int
eval (X _)       = error "X in eval"
eval (I _ n)     = n
eval (O _ o a b) = f o (eval a) (eval b)
    where f = \case A -> (+); S -> (-); M -> (*); D -> div

annHasX :: Expr a -> Expr Bool
annHasX (X _)       = X True
annHasX (I _ n)     = I False n
annHasX (O _ o a b) =
    let a' = annHasX a; b' = annHasX b
    in O (ann a' || ann b') o a' b'

equal :: Expr Bool -> Expr Bool -> Expr Bool
equal (X _)       e = e
equal (O _ A a b) e = if ann a then equal a (O False S e b) else equal b (O False S e a)
equal (O _ S a b) e = if ann a then equal a (O False A e b) else equal b (O False S a e)
equal (O _ M a b) e = if ann a then equal a (O False D e b) else equal b (O False D e a)
equal (O _ D a b) e = if ann a then equal a (O False M e b) else equal b (O False D a e)
equal _             _ = error "not possible"

parse :: String -> M.Map String Monkey
parse = M.fromList . map (f . words) . lines
    where
        f [take 4 -> k, read -> n] = (k, MI n)
        f [take 4 -> k, k1, op, k2] = (k, MO (make op) k1 k2)
        make = \case "+" -> A; "-" -> S; "*" -> M; "/" -> D

p1 :: String -> Int
p1 inp = eval $ es M.! "root"
    where
        xs = parse inp
        es = m where m = M.map (f m) xs

        f _ (MI n)       = I () n
        f m (MO o k1 k2) = O () o (m M.! k1) (m M.! k2)

p2 :: String -> Int
p2 inp = eval $ if ann el then equal el er else equal er el
    where
        xs = parse inp
        es = m where m = M.mapWithKey (f m) xs

        f _ "humn" _           = X ()
        f _ _     (MI n)       = I () n
        f m _     (MO o k1 k2) = O () o (m M.! k1) (m M.! k2)

        MO _ l r = xs M.! "root"
        el = annHasX (es M.! l)
        er = annHasX (es M.! r)
