{-# LANGUAGE OverloadedStrings #-}

module Main where

import Hedgehog
import qualified Hedgehog.Gen as Gen
import qualified Hedgehog.Range as Range

fun2 :: Integer -> Integer
fun2 1 = 0
fun2 n | even n    = n + fun2 (n `div` 2)
       | otherwise = fun2 (3 * n + 1)

prop_is_fun2 :: (Integer -> Integer) -> Property
prop_is_fun2 f = property $ do
  x <- forAll . Gen.integral $ Range.linear 1 100
  f x === fun2 x

testF :: (Integer -> Integer) -> IO Bool
testF f = checkParallel $ Group "Main" [("prop_is_fun2", prop_is_fun2 f)]

main :: IO ()
main = undefined
