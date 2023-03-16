module Solver.Brute (
  brute
) where

import Helper.Functions ( getSolution )
import Helper.Types (Solution, Knapsack(..), Item(..))

brute :: Knapsack -> Solution
brute k = brute' (maxWeight k) (minCost k) (items k)

brute' :: Int -> Int -> [Item] -> Solution
brute' maxW minC is = getSolution is minC maxW$ itemsCombinations $ length is
  where 
    itemsCombinations :: Int -> [[Int]]
    itemsCombinations 0 = [[]]
    itemsCombinations n 
      | weight (is !! (length is - n)) > maxW = map (0 :) (itemsCombinations (n-1)) -- Filter out items with too much weight
      | otherwise = map (0 :) (itemsCombinations (n-1)) ++ map (1 :) (itemsCombinations (n-1))
