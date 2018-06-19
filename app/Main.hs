module Main where

import H1.H1

main :: IO ()
main = print $ hanoi 5 "a" "b" "c"
