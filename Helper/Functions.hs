module Helper.Functions (
  toSolution,
  sumCosts,
  sumWeights
) where

import Helper.Types ( Subset, Solution, Item(..) )

toSolution :: Maybe (Subset Item) -> Maybe Solution
toSolution Nothing = Nothing
toSolution (Just xs) = Just $ map fst xs

sumCosts :: Subset Item -> Int
sumCosts xs = sum (map (cost . snd) (filter (\x -> fst x /= 0) xs))

sumWeights :: Subset Item -> Int
sumWeights xs = sum (map (weight . snd) (filter (\x -> fst x /= 0) xs))
