module Helper.Functions (
  sumCosts,
  sumWeights,
  finiteRandoms,
  differentRandomRs,
  getSolution
) where

import Data.Function (on)
import Data.List (maximumBy)

import System.Random ( Random(randomR, random), RandomGen )

import Helper.Types ( Subset, Solution, Item(..) )

getSolution :: Int -> Int -> [Subset Item] -> Solution
getSolution minC maxW = toSolution . bestSolution . feasibleSolutions
  where
    feasibleSolutions :: [Subset Item] -> [Subset Item]
    feasibleSolutions = filter (\xs -> sumCosts xs >= minC && sumWeights xs <= maxW)

    toSolution :: Maybe (Subset Item) -> Solution
    toSolution Nothing = Nothing
    toSolution (Just xs) = Just $ map fst xs

    bestSolution :: [Subset Item] -> Maybe (Subset Item)
    bestSolution [] = Nothing
    bestSolution xs = Just $ maximumBy (compare `on` sumCosts) xs

sumCosts :: Subset Item -> Int
sumCosts xs = sum (map (cost . snd) (filter (\x -> fst x /= 0) xs))

sumWeights :: Subset Item -> Int
sumWeights xs = sum (map (weight . snd) (filter (\x -> fst x /= 0) xs))

-- Generate n random samples
finiteRandoms :: (RandomGen g, Random a) => Int -> g -> ([a], g)
finiteRandoms 0 gen = ([], gen)
finiteRandoms n gen =
  let (value, newGen) = random gen
      (restOfList, finalGen) = finiteRandoms (n - 1) newGen
   in (value : restOfList, finalGen)

-- Generate n different samples in a given range
-- Doesn't terminate if it's not possible to fit n samples into the given range
differentRandomRs :: (RandomGen g, Random a, Eq a) => (a, a) -> Int -> g -> ([a], g)
differentRandomRs (lower, upper) num = differentRandoms' num []
  where
    differentRandoms' n list gen
      | n == length list = (list, gen)
      | otherwise =
          let (value, newGen) = randomR (lower, upper) gen
              newList = if value `notElem` list then value : list else list
           in differentRandoms' n newList newGen
