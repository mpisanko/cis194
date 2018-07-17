module Golf where

import Data.List
import Data.Bool
import Data.Map.Strict as Map (adjust, fromList, assocs, elems, Map)
import Control.Arrow

skips :: [a] -> [[a]]
skips xs = map (\i -> snd <$> filter ((==0).(`mod` i).fst) (zip [1 ..] xs)) [1 .. length xs]

localMaxima :: [Integer] -> [Integer]
localMaxima xs@(_:y:zs) = concatMap (\(a,b,c) -> bool [] [b] (2 * b > a + c)) $ zip3 xs (y : zs) zs
localMaxima _ = []

hm :: [Integer] -> (Map Integer Int)
hm = foldr (adjust (+1)) (fromList (zip [0..9] (repeat 0)))
-- hm = (map (head &&& length)) . group . sort
toString :: (Map Integer Int) -> [String]
toString m = map (\(i,n) -> (replicate ((maximum $ elems m) - n) ' ') ++ (replicate n '*') ++ "=" ++ show i) $ assocs m
histogram :: [Integer] -> String
histogram = concatMap (++ "\n") . transpose . toString . hm
