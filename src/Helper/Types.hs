module Helper.Types (
  Item(..),
  Knapsack(..),
  Solution,
  Subset,
  Individual,
  Population
) where

data Item = Item
  { weight :: Int,
    cost :: Int
  }

instance Show Item where
  show i = "\tItem {\n" ++ showWeight i ++ showCost i ++ "\t}\n"
    where
      showWeight i = "\tweight: " ++ show (weight i) ++ "\n"
      showCost i = "\tcost: " ++ show (cost i) ++ "\n"

data Knapsack = Knapsack
  { maxWeight :: Int,
    minCost :: Int,
    items :: [Item]
  }

instance Show Knapsack where
  show k = "Knapsack {\n" ++ showWeight k ++ showCost k ++ showItems k ++ "}"
    where
      showWeight k = "maxWeight: " ++ show (maxWeight k) ++ "\n"
      showCost k = "minCost: " ++ show (minCost k) ++ "\n"
      showItems k = "items: [\n" ++ showItems' (items k) ++ "]\n"
      showItems' [] = ""
      showItems' (x:xs) = show x ++ showItems' xs

type Solution = Maybe [Int]

type Subset a = [(Int, a)]

type Individual = Subset Item

type Population = [Individual]
