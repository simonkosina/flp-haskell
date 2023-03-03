module Helper.Types (
  Item(..),
  Knapsack(..),
  Solution,
  Subset
) where

data Item = Item
  { weight :: Int,
    cost :: Int
  }
  deriving (Show, Read)

data Knapsack = Knapsack
  { maxWeight :: Int,
    minCost :: Int,
    items :: [Item]
  }
  deriving (Show, Read)

type Solution = [Int]

type Subset a = [(Int, a)]
