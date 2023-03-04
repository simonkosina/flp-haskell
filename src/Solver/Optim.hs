module Solver.Optim (
  optim
) where

import System.Random

import Helper.Types (Subset, Solution, Knapsack(..), Item(..))
import Helper.Functions (sumCosts, sumWeights)

-- population size? 100-1000
-- tournament selection  (pick 2 (could be parametrized) solutions at random winner is the parent - do twice)
-- crossover rate (split in half) -> 2 new children
-- mutation rate 0.05, 0.01 for each bit
-- generate new tournaments to swap to whole population

-- TODO: Export some stuff to Helper.Functions, Helper.Types

type Individual = Subset Item
type Population = [Individual]

-- a = random (mkStdGen 10)

k = Knapsack {
            maxWeight = 46,
            minCost = 324,
            items = [
              Item { weight = 36, cost = 3 },
              Item { weight = 43, cost = 1129 },
              Item { weight = 202, cost = 94 },
              Item { weight = 149, cost = 2084 }
            ]
          }

optim :: Knapsack -> Int
-- optim :: Knapsack -> Solution
optim k = 0

fitness :: Int -> Int -> Individual -> Int
fitness mW mC i
  | sumWeights i > mW || sumCosts i < mC = 0
  | otherwise = sumCosts i

initIndividual :: Knapsack -> StdGen -> (Individual, StdGen)
initIndividual k gen =
  let (randomBools, newGen) = finiteRandoms (length (items k)) gen :: ([Bool], StdGen)
      randomInts = map fromEnum randomBools
  in (zip randomInts (items k), newGen)

initPopulation :: Knapsack -> Int -> StdGen -> (Population, StdGen)
initPopulation _ 0 gen = ([], gen)
initPopulation k n gen =
  let (individual, newGen) = initIndividual k gen
      (restOfPopulation, finalGen) = initPopulation k (n-1) newGen
  in (individual : restOfPopulation, finalGen)

-- tournament :: Population -> Int -> StdGen -> Individual
-- tournament p n gen = 

-- cituj http://learnyouahaskell.com/input-and-output#randomness
finiteRandoms :: (RandomGen g, Random a) => Int -> g -> ([a], g)
finiteRandoms 0 gen = ([], gen)
finiteRandoms n gen =
  let (value, newGen) = random gen
      (restOfList, finalGen) = finiteRandoms (n - 1) newGen
  in (value : restOfList, finalGen)

differentRandomRs :: (RandomGen g, Random a, Eq a) => (a,a) -> Int -> g -> ([a], g)
differentRandomRs (lower, upper) n gen = differentRandoms' n [] gen
  where
  differentRandoms' n list gen
    | n == length list = (list, gen)
    | otherwise = 
      let (value, newGen) = randomR (lower, upper) gen
          newList = if value `notElem` list then value:list else list
      in differentRandoms' n newList newGen

-- chooseRandom :: [a] -> StdGen -> (a, StdGen)
-- chooseRandom l gen = 
--   let (index, newGen) = randomR (0, length l - 1) gen :: (Int, StdGen)
--   in (l !! index, newGen)