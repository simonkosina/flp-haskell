module Solver.Optim (
  optim
) where

import System.Random

import Data.Function (on)
import Data.List (maximumBy)

import Helper.Types (Subset, Solution, Knapsack(..), Item(..))
import Helper.Functions (sumCosts, sumWeights, toSolution)
import Data.Maybe

-- population size? 100-1000
-- tournament selection  (pick 2 (could be parametrized) solutions at random winner is the parent - do twice)
-- crossover rate (split in half) -> 2 new children
-- mutation rate 0.05, 0.01 for each bit
-- generate new tournaments to swap to whole population

-- TODO: Find best solution or just a solution? Same for Brute.hs.
-- TODO: Export some stuff to Helper.Functions, Helper.Types
-- TODO: Refactor variable names, Type definitions

type Individual = Subset Item
type Population = [Individual]

-- a = random (mkStdGen 10)
mutationRate :: Float
mutationRate = 0.05 :: Float
numPlayers :: Int
numPlayers = 4
populationSize :: Int
populationSize = 1000
seed :: Int
seed = 7

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

-- TODO: Check if we need to find the best solution.
-- https://moodle.vut.cz/mod/forum/discuss.php?d=1882
-- Get rid of (Ord a) => (Individual -> a), just use Int
optim :: Knapsack -> Solution
optim k = 
  let fitness' = fitness (maxWeight k) (minCost k)
      (population, newGen) = initPopulation (items k) populationSize (mkStdGen seed)
  in optim' fitness' newGen population

optim' :: (Individual -> Int) -> StdGen -> Population -> Solution
optim' f gen p =
  let solution = checkSolution f p
      (newP, newGen) = newPopulation f p gen
  in
    if isJust solution
      then toSolution solution
      else optim' f newGen newP

fitness :: Int -> Int -> Individual -> Int
fitness mW mC i
  | sumWeights i > mW || sumCosts i < mC = 0
  | otherwise = sumCosts i

initIndividual :: [Item] -> StdGen -> (Individual, StdGen)
initIndividual items gen =
  let (randomBools, newGen) = finiteRandoms (length items) gen :: ([Bool], StdGen)
      randomInts = map fromEnum randomBools
  in (zip randomInts items, newGen)

initPopulation :: [Item] -> Int -> StdGen -> (Population, StdGen)
initPopulation _ 0 gen = ([], gen)
initPopulation items n gen =
  let (individual, newGen) = initIndividual items gen
      (restOfPopulation, finalGen) = initPopulation items (n-1) newGen
  in (individual : restOfPopulation, finalGen)

-- TODO: same order of args (f, p, gen -> f, gen, p)
newPopulation :: (Ord a) => (Individual -> a) -> Population -> StdGen -> (Population, StdGen)
newPopulation f p gen = newPopulation' f p [] gen
  where
    newPopulation' f oldP newP gen
      | length oldP == length newP = (newP, gen)
      | otherwise = 
        let (parent1, newGen1) = pickWinner f oldP gen
            (parent2, newGen2) = pickWinner f oldP newGen1
            (child, newGen3) = createChild parent1 parent2 newGen2
        in newPopulation' f oldP (child:newP) newGen3

-- TODO: Potential to optimize
checkSolution :: (Individual -> Int) -> Population -> Maybe Individual
checkSolution f p = 
  let best = maximumBy (compare `on` f) p
  in if f best > 0 then Just best else Nothing

mutate :: Individual -> StdGen -> (Individual, StdGen)
mutate [] gen = ([], gen)
mutate (i:is) gen = 
  let (value, newGen) = randomR (0,1) gen :: (Float, StdGen)
      flipBit b = if b == 0 then 1 else 0
      newTuple = (if value < mutationRate then flipBit (fst i) else fst i, snd i)
      (restOfTuples, finalGen) = mutate is newGen
  in (newTuple : restOfTuples, finalGen)

createChild :: Individual -> Individual -> StdGen -> (Individual, StdGen)
createChild p1 p2 gen=
  let
    half = length p1 `div` 2
    child = take half p1 ++ drop half p2
  in mutate child gen

pickWinner :: (Ord a) => (Individual -> a) -> Population -> StdGen -> (Individual, StdGen)
pickWinner f p gen =
  let
    (sample, newGen) = samplePopulation p numPlayers gen
    winner = maximumBy (compare `on` f) sample
  in (winner, newGen)

samplePopulation :: Population -> Int -> StdGen -> (Population, StdGen)
samplePopulation p n gen =
  let (indices, newGen) = differentRandomRs (0, length p - 1) n gen
      samples = map (p !!) indices
  in (samples, newGen)

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