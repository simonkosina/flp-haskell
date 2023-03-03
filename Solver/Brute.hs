module Solver.Brute (
  brute
) where

import Data.List (maximumBy)
import Data.Function (on)
import Helper.Functions (toSolution, sumWeights, sumCosts)
import Helper.Types (Subset, Solution, Knapsack(..), Item)

brute :: Knapsack -> Maybe Solution
brute k = brute' (maxWeight k) (minCost k) (items k)

brute' :: Int -> Int -> [Item] -> Maybe Solution
brute' maxW minC xss = toSolution $ findBest $ filterFeasible $ subsets xss
  where
    findBest :: [Subset Item] -> Maybe (Subset Item)
    findBest [] = Nothing
    findBest all@(x:xs) = Just $ maximumBy (compare `on` sumCosts) all

    filterFeasible :: [Subset Item] -> [Subset Item]
    filterFeasible = filter (\xs -> sumCosts xs >= minC && sumWeights xs <= maxW)

subsets :: [a] -> [Subset a]
subsets xs = subsets' (length xs) xs
  where
    subsets' 0 _ = [[]]
    subsets' n (x : xs) = map ((1, x) :) (subsets' (n - 1) xs) ++ map ((0, x) :) (subsets' (n - 1) xs)
