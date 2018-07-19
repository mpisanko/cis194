module Golf where

import Data.List
import Data.Bool
import Data.Map.Strict as Map (adjust, fromList, assocs, elems, Map)
import Data.Maybe
import Control.Arrow

skips :: [a] -> [[a]]
skips xs = map (\i -> snd <$> filter ((==0).(`mod` i).fst) (zip [1 ..] xs)) [1 .. length xs]

localMaxima :: [Integer] -> [Integer]
localMaxima x@(_:y:z) = concatMap (\(a,b,c) -> bool [] [b] (2 * b > a + c)) $ zip3 x (y : z) z
localMaxima _ = []
-- localMaxima = catMaybes . unfoldr (case splitAt 3 of
--                                           ((a:b:[c]),t) -> bool (Just (Nothing, t)) (Just (Just b,t)) (2 * b > a + c)
--                                           _ -> Nothing)

hm :: [Integer] -> (Map Integer String)
hm = foldr (adjust (++ "*")) (fromList (zip [0..9] (repeat "=")))
-- hm :: [Integer] -> ((Map Integer String), Int)
-- hm = foldr ((fst <$> (adjust (++ "*"))), (snd)) ((fromList (zip [0..9] (repeat "="))), 0)
-- hm = (map (head &&& length)) . group . sort
toString :: (Map Integer String) -> [String]
toString m = map (\(i,n) -> (reverse (take (maximum . (map length) $ elems m) $ n ++ (repeat ' '))) ++ show i) $ assocs m
histogram :: [Integer] -> String
histogram =  intercalate "\n" . transpose . toString . hm
