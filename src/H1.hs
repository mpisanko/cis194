module H1
  ( someFunc, toDigits, toDigitsRev, doubleEveryOther, sumDigits, validate, hanoi
    ) where

import Data.List
import Data.Bool

someFunc :: IO ()
someFunc = putStrLn "someFunc"

toDigits :: Integer -> [Integer]
toDigits = reverse . toDigitsRev

toDigsChris :: Integer -> [Integer]
toDigsChris = unfoldr (\n -> bool (Just (n `mod` 10, n `div` 10)) Nothing (n == 0))

toDigitsRev :: Integer -> [Integer]
toDigitsRev x | x < 1 = []
              | otherwise = mod' : toDigitsRev div'
              where (div', mod') = x `divMod` 10

doubleEveryOtherLeft :: [Integer] -> [Integer]
doubleEveryOtherLeft [] = []
doubleEveryOtherLeft (x : []) = [x]
doubleEveryOtherLeft (x : y : xs) = x : 2 * y : (doubleEveryOtherLeft xs)

-- doubleEveryOther :: [Integer] -> [Integer]
-- doubleEveryOther xs = reverse $ go $ reverse xs
--     where
--         go :: [Integer] -> [Integer]
--         go []       = []
--         go (x:[])   = [x]
--         go (x:y:xs) = x:y*2:go xs

doubleEveryOther :: [Integer] -> [Integer]
doubleEveryOther = reverse . zipWith (*) (cycle [1, 2]) . reverse

sumDigits :: [Integer] -> Integer
sumDigits = (foldr (+) 0) . concat . (map toDigits)

validate :: Integer -> Bool
validate x = ((flip mod 10) . sumDigits . doubleEveryOther . toDigits $ x) == 0

type Peg = String
type Move = (Peg, Peg)
hanoi :: Integer -> Peg -> Peg -> Peg -> [Move]
hanoi n a b c
  | n <= 0 = []
  | n == 1 = [(a, b)]
  | otherwise = hanoi (n-1) a c b ++ hanoi 1 a b c ++ hanoi (n-1) c b a
