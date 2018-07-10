module Golf where

import Data.List
import Data.Bool
import Data.Map.Strict (update, fromList, toList)

skips :: [a] -> [[a]]
skips xs = map (\i -> snd <$> filter ((==0).(`mod` i).fst) (zip [1 ..] xs)) [1 .. length xs]

m :: [Integer] -> [Integer]
m (x : y : z : []) = bool [] [y] (y > x && y > z)
m _ = []

localMaxima :: [Integer] -> [Integer]
localMaxima xs@(_:y:zs) = concatMap m $ transpose [xs, y : zs, zs]
localMaxima _ = []
-- localMaxima xs = concatMap m $ map (\i -> (take 3 (drop i xs))) [0 .. length xs]

hm :: [Integer] -> [(Integer, Int)]
hm xs = toList $ foldr (update (Just.(+1))) (fromList (zip [0..9] (repeat 0))) xs
toString :: [(Integer, Int)] -> [String]
toString ms = map (\(i,n)-> reverse $ (show i) ++ "=" ++ (take n (repeat '*')) ++ (take ((maximum (map snd ms)) - n) (repeat ' '))) ms
histogram :: [Integer] -> String
histogram xs = (foldr (\i j-> i ++ "\n" ++ j) []) . transpose . toString . hm $ xs
