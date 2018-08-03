module H4 where

import Data.Bool
import Data.List

fun1 :: [Integer] -> Integer
fun1 []     = 1
fun1 (x:xs)
  | even x    = (x - 2) * fun1 xs
  | otherwise = fun1 xs
fun1' :: [Integer] -> Integer
fun1' = product . map ((-) 2) . filter even

fun2 :: Integer -> Integer
fun2 1 = 0
fun2 n | even n    = n + fun2 (n `div` 2)
       | otherwise = fun2 (3 * n + 1)
fun2' :: Integer -> Integer
fun2' = sum . filter even . takeWhile (>1) . iterate (\n -> (bool (1 + 3 * n) (n `div` 2) (even n)))
-- fun2' = sum . unfoldr (\i -> case (i `divMod` 2) of
--                         (0, _) -> Nothing
--                         (n, 0) -> (Just (i, n))
--                         (n, _) -> (Just (0, 1 + 3 * i)))

data Tree a = Leaf
            | Node Integer (Tree a) a (Tree a)
  deriving (Eq, Show)

foldTree :: [a] -> Tree a
foldTree xs = foldr insertNode Leaf xs
  where
    maxDepth = (floor . (logBase 2) . fromIntegral . length) xs
    insertNode x Leaf = Node 0 Leaf x Leaf
    insertNode x (Node d Leaf x' Leaf) = Node (d + 1) (Node d Leaf x Leaf) x' Leaf
    insertNode x (Node d Leaf x' r@(Node _ _ _ _)) = Node d (Node (d - 1) Leaf x Leaf) x' r
    insertNode x (Node d l@(Node _ _ _ _) x' Leaf) = Node d l x' (Node (d - 1) Leaf x Leaf)
    insertNode x (Node d l@(Node dl _ _ _) x' r@(Node dr _ _ _))
      | dr > dl  = Node d (insertNode x l) x' r
      | dl > dr  = Node d l x' (insertNode x r)
      | dl == dr = Node (bool d (d+1) (d < maxDepth)) l x' (insertNode x r)

xor :: [Bool] -> Bool
-- xor = odd . length . filter id
xor = foldr (\b r -> bool r (not r) b) False

map' :: (a -> b) -> [a] -> [b]
map' f = foldr (\a bs -> (((flip (:)) bs) . f) a) []

myFoldl :: (a -> b -> a) -> a -> [b] -> a
myFoldl f base xs = foldr (flip f) base (reverse xs)

-- Start with a list of the integers from 1 to n. From this list, remove all numbers of the form i + j + 2ij where:
--     i , j ∈ N ,   1 ≤ i ≤ j
--     i + j + 2 i j ≤ n
-- The remaining numbers are doubled and incremented by one,
-- giving a list of the odd prime numbers (i.e., all primes except 2) below 2n + 2. 

sieveSundaram :: Integer -> [Integer]
sieveSundaram n = map ((+1) . (*2)) ([1..n] \\ eliminated n)
  where
    eliminated n' = [i + j + 2 * i * j | i <- [1..n' - 1]
                                       , j <- [i..n']]
