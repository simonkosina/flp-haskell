import Distribution.Simple.Utils (xargs)
-- FIXME: Export types from common file
-- FIXME: Extract the helper functions to a new file.

data Item = Item {
  weight :: Int,
  cost :: Int
} deriving (Show, Read)

data Knapsack = Knapsack {
  maxWeight :: Int,
  minCost :: Int,
  items :: [Item]
} deriving (Show, Read)

knapsack = Knapsack {
            maxWeight = 46,
            minCost = 324,
            items = [
              Item { weight = 36, cost = 3 },
              Item { weight = 43, cost = 1129 },
              Item { weight = 202, cost = 94 },
              Item { weight = 149, cost = 2084 }
            ]
          }

i = items knapsack

type Solution = [Int]

type Subset a = [(Int, a)]

brute :: Knapsack -> Solution
brute k = brute' (maxWeight k) (minCost k) (subsets (items k)) 

brute' :: Int -> Int -> [Subset Item] -> Solution
brute' _ _ [] = [] -- FIXME: Print False
brute' mw mc (x:xs)
  | mw >= itemsWeight x && mc <= itemsCost x = toSolution x
  | otherwise = brute' mw mc xs

toSolution :: Subset Item -> Solution
toSolution = map fst

itemsCost :: Subset Item -> Int
itemsCost xs = sum (map (cost . snd) (filter (\x -> fst x /= 0) xs))

itemsWeight :: Subset Item -> Int
itemsWeight xs = sum (map (weight . snd) (filter (\x -> fst x /= 0) xs))

subsets :: [a] -> [Subset a]
subsets xs = subsets' (length xs) xs

subsets' :: Int -> [a] -> [Subset a]
subsets' 0 _ = [[]]
subsets' n (x : xs) = map ((1, x) :) (subsets' (n - 1) xs) ++ map ((0, x) :) (subsets' (n - 1) xs)
