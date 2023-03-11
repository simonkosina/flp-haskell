module Solver.Brute (
  brute
) where

import Helper.Functions (sumWeights, sumCosts, getSolution)
import Helper.Types (Subset, Solution, Knapsack(..), Item)

brute :: Knapsack -> Solution
brute k = brute' (maxWeight k) (minCost k) (items k)

brute' :: Int -> Int -> [Item] -> Solution
brute' maxW minC xss = getSolution minC maxW $ allSubsets xss

allSubsets :: [a] -> [Subset a]
allSubsets xs = allSubsets' (length xs) xs
  where
    allSubsets' 0 _ = [[]]
    allSubsets' n (x : xs) = map ((1, x) :) (allSubsets' (n - 1) xs) ++ map ((0, x) :) (allSubsets' (n - 1) xs)
