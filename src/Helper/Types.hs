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
  show :: Item -> String
  show i = "\tItem {\n" ++ showWeight i ++ showCost i ++ "\t}\n"
    where
      showWeight x = "\tweight: " ++ show (weight x) ++ "\n"
      showCost x = "\tcost: " ++ show (cost x) ++ "\n"

data Knapsack = Knapsack
  { maxWeight :: Int,
    minCost :: Int,
    items :: [Item]
  }

instance Show Knapsack where
  show :: Knapsack -> String
  show k = "Knapsack {\n" ++ showWeight k ++ showCost k ++ showItems k ++ "}"
    where
      showWeight x = "maxWeight: " ++ show (maxWeight x) ++ "\n"
      showCost x = "minCost: " ++ show (minCost x) ++ "\n"
      showItems x = "items: [\n" ++ showItems' (items x) ++ "]\n"
      showItems' [] = ""
      showItems' (x:xs) = show x ++ showItems' xs

type Solution = Maybe [Int]

type Subset a = [(Int, a)]

type Individual = Subset Item

type Population = [Individual]
