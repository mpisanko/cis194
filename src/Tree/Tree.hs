module Tree.Tree where

import Data.Bool
import Data.List

import Data.Map

data BinaryTree a =
    Leaf
  | Node (BinaryTree a) a (BinaryTree a)
  deriving (Eq, Ord, Show)

insert' :: Ord a => a -> BinaryTree a -> BinaryTree a
insert' b Leaf = Node Leaf b Leaf
insert' b (Node left a right)
  | b == a = Node left a right
  | b < a  = Node (insert' b left) a right
  | b > a  = Node left a (insert' b right)

mapTree :: (a -> b) -> BinaryTree a -> BinaryTree b
mapTree _ Leaf = Leaf
mapTree f (Node left a' right) =
  Node (mapTree f left) (f a') (mapTree f right)

preorder :: BinaryTree a -> [a]
preorder Leaf = []
preorder (Node left b right) =
  [b] ++ preorder left ++ preorder right
inorder :: BinaryTree a -> [a]
inorder Leaf = []
inorder (Node left b right) =
  inorder left ++ [b] ++ inorder right
postorder :: BinaryTree a -> [a]
postorder Leaf = []
postorder (Node left b right) =
  postorder left ++ postorder right ++ [b]

foldTree :: (a -> b -> b) -> b -> BinaryTree a -> b
foldTree _ b' Leaf = b'
foldTree f b' (Node left a' right) =
  foldTree f (foldTree f (f a' b') left) right

sample :: BinaryTree Int
sample = Node (Node Leaf 1 Leaf) 2 (Node Leaf 3 Leaf)

someFunc :: IO ()
someFunc = do
  print $ mapTree (+1) sample
  print $ preorder sample
  print $ inorder sample
  print $ postorder sample
  print $ foldTree (+) 0 sample


myIterate :: (a -> a) -> a -> [a]
myIterate f x = x : myIterate f (f x)

myUnfoldr :: (b -> Maybe (a, b)) -> b -> [a]
myUnfoldr f x = case f x of
  Just (y, x') -> y : myUnfoldr f x'
  Nothing      -> []

m :: [Integer] -> [Integer]
m (x : y : z : []) = bool [] [y] (y > x && y > z)
m _ = []

localMaxima :: [Integer] -> [Integer]
localMaxima xs@(_:y:zs) = concatMap m $ transpose [xs, y : zs, zs]
localMaxima _ = []
-- localMaxima xs = concatMap m $ map (\i -> (take 3 (drop i xs))) [0 .. length xs]

hm :: [Integer] -> Map Integer Integer
hm = foldr (\i m -> alter ((+1).Just) i m) (fromList (zip [0..9] (repeat 0)))
-- histogram :: [Integer] -> String
-- histogram xs = foldr
