import System.Environment
import System.Directory
import System.IO
import Data.List

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

dispatch :: [(String, [String] -> IO())]
dispatch = [ ("-i", info),
             ("-b", brute),
             ("-o", optim)
           ]

-- TODO: Expect incorrect arguments? Check #flp discord question.
-- TODO: Print output on multiple lines instead of one?
main :: IO ()
main = do
  (switch:args) <- getArgs
  let (Just action) = lookup switch dispatch
  action args

info _ = putStrLn "info"

brute _ = putStrLn "brute"

optim _ = putStrLn "optim"
