module Helper.Functions (
  toSolution,
  showSolution,
  sumCosts,
  sumWeights
) where

import Helper.Types ( Subset, Solution, Item(..) )

toSolution :: Maybe (Subset Item) -> Solution
toSolution Nothing = Nothing
toSolution (Just xs) = Just $ map fst xs

showSolution :: Solution -> String
showSolution Nothing = "Couldn't find a solution for the given knapsack instance! :("
showSolution (Just s) = show s

sumCosts :: Subset Item -> Int
sumCosts xs = sum (map (cost . snd) (filter (\x -> fst x /= 0) xs))

sumWeights :: Subset Item -> Int
sumWeights xs = sum (map (weight . snd) (filter (\x -> fst x /= 0) xs))
