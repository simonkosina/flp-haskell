module Solver.Optim (
  optim
) where

import System.Random ( mkStdGen, Random(randomR), StdGen )

import Data.Function (on)
import Data.List (maximumBy)

import Helper.Types (Subset, Solution, Knapsack(..), Item(..), Individual, Population)
import Helper.Functions (sumCosts, sumWeights, differentRandomRs, finiteRandoms, getSolution)
import Data.Maybe ( isJust )

-- TODO: Play with hyperparams
mutationRate :: Float
mutationRate = 0.05 :: Float
numPlayers :: Int
numPlayers = 8
populationSize :: Int
populationSize = 1000
iterations :: Int
iterations = 100 :: Int
seed :: Int
seed = 7

optim :: Knapsack -> Solution
optim k = 
  let fitness' = fitness (minCost k) (maxWeight k)
      (population, newGen) = initPopulation (items k) populationSize (mkStdGen seed)
  in getSolution (minCost k) (maxWeight k) $ optim' fitness' population newGen iterations

optim' :: (Individual -> Int) -> Population -> StdGen -> Int -> Population
optim' fit pop gen it
  | it == 0 = pop
  | otherwise = 
    let (newP, newGen) = newPopulation fit pop gen
    in optim' fit newP newGen (it - 1)

fitness :: Int -> Int -> Individual -> Int
fitness minC maxW i
  | sumWeights i > maxW || sumCosts i < minC = 0
  | otherwise = sumCosts i

-- Randomly assign items to a new individual
initIndividual :: [Item] -> StdGen -> (Individual, StdGen)
initIndividual items gen =
  let (randomBools, newGen) = finiteRandoms (length items) gen :: ([Bool], StdGen)
      randomInts = map fromEnum randomBools
  in (zip randomInts items, newGen)

-- Initialize n individuals to create a population
initPopulation :: [Item] -> Int -> StdGen -> (Population, StdGen)
initPopulation _ 0 gen = ([], gen)
initPopulation items n gen =
  let (individual, newGen) = initIndividual items gen
      (restOfPopulation, finalGen) = initPopulation items (n-1) newGen
  in (individual : restOfPopulation, finalGen)

-- Create a new population by mating individuals and creating new children
newPopulation :: (Individual -> Int) -> Population -> StdGen -> (Population, StdGen)
newPopulation fit pop gen = newPopulation' fit pop [] gen
  where
    newPopulation' fit oldPop newPop gen
      | length oldPop == length newPop = (newPop, gen)
      | otherwise = 
        let (parent1, newGen1) = pickWinner fit oldPop gen
            (parent2, newGen2) = pickWinner fit oldPop newGen1
            (child, newGen3) = createChild parent1 parent2 newGen2
        in newPopulation' fit oldPop (child:newPop) newGen3

-- Split parent's genes in half and mutate them to create a child
createChild :: Individual -> Individual -> StdGen -> (Individual, StdGen)
createChild ind1 ind2 gen =
  let
    half = length ind1 `div` 2
    child = take half ind1 ++ drop half ind2
  in mutate child gen

mutate :: Individual -> StdGen -> (Individual, StdGen)
mutate [] gen = ([], gen)
mutate (i : is) gen =
  let (value, newGen) = randomR (0, 1) gen :: (Float, StdGen)
      flipBit b = if b == 0 then 1 else 0
      newTuple = (if value < mutationRate then flipBit (fst i) else fst i, snd i)
      (restOfTuples, finalGen) = mutate is newGen
   in (newTuple : restOfTuples, finalGen)

-- Tournament selection
pickWinner :: (Individual -> Int) -> Population -> StdGen -> (Individual, StdGen)
pickWinner fit pop gen =
  let
    (sample, newGen) = samplePopulation pop numPlayers gen
    winner = maximumBy (compare `on` fit) sample
  in (winner, newGen)

-- Sample n different random individuals out of a population
samplePopulation :: Population -> Int -> StdGen -> (Population, StdGen)
samplePopulation pop n gen =
  let (indices, newGen) = differentRandomRs (0, length pop - 1) n gen
      samples = map (pop !!) indices
  in (samples, newGen)
