module Solver.Brute (
  brute
) where

import Helper.Functions (getSolution)
import Helper.Types (Subset, Solution, Knapsack(..), Item)

brute :: Knapsack -> Solution
brute k = brute' (maxWeight k) (minCost k) (items k)

brute' :: Int -> Int -> [Item] -> Solution
brute' maxW minC xss = getSolution minC maxW $ allSubsets xss

allSubsets :: [a] -> [Subset a]
allSubsets xs = allSubsets' (length xs) xs
  where
    allSubsets' 0 _ = [[]]
    allSubsets' _ [] = [[]]
    allSubsets' n (y : ys) = map ((1, y) :) (allSubsets' (n - 1) ys) ++ map ((0, y) :) (allSubsets' (n - 1) ys)
