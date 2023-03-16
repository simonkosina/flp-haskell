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

import Helper.Types ( Solution, Item(..), Individual )

getSolution :: [Item] -> Int -> Int -> [Individual] -> Solution
getSolution is minC maxW xs = bestSolution $ feasibleSolutions xs
  where
    feasibleSolutions :: [Individual] -> [Individual]
    feasibleSolutions = filter (\ys -> sumCosts is ys >= minC && sumWeights is ys <= maxW)

    bestSolution :: [Individual] -> Maybe Individual
    bestSolution [] = Nothing
    bestSolution ys = Just $ maximumBy (compare `on` sumCosts is) ys

sumCosts :: [Item] -> Individual -> Int
sumCosts is xs = sum $ zipWith (\i x -> x * cost i) is xs

sumWeights :: [Item] -> Individual -> Int
sumWeights is xs = sum $ zipWith (\i x -> x * weight i) is xs

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
