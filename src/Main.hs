import System.Environment (getArgs)
import System.Exit (die)
import Solver.Brute (brute)

import Control.Applicative ((<|>))
import Control.Monad (when)

import Helper.Types (Knapsack(..), Item(..), Solution)
import Helper.Functions (showSolution)

-- TODO: Extract parsing to a file
import qualified Text.Parsec as Parsec
import Text.Parsec((<?>))
import qualified Data.Char as Parsec

-- knapsack = Knapsack {
--             maxWeight = 46,
--             minCost = 324,
--             items = [
--               Item { weight = 36, cost = 3 },
--               Item { weight = 43, cost = 1129 },
--               Item { weight = 202, cost = 94 },
--               Item { weight = 149, cost = 2084 }
--             ]
--           }

dispatch :: [(String, Knapsack -> IO ())]
dispatch = [
            --  ("-i", info),
             ("-b", showSolution . brute)
            --  ("-o", optim)
           ]

parse rule = Parsec.parse rule "(source)"

-- knapsackParser :: Parsec.Parsec String () Knapsack
knapsackParser :: Parsec.Parsec String () Int
knapsackParser = do
  Parsec.string "Knapsack {"
  Parsec.newline
  return 0

maxWeightParser :: Parsec.Parsec String () Int
maxWeightParser = do
  Parsec.string "maxWeight: "
  digits <- Parsec.many1 Parsec.digit
  Parsec.newline
  return (read digits :: Int)

minCostParser :: Parsec.Parsec String () Int
minCostParser = do
  Parsec.string "minCost: "
  digits <- Parsec.many1 Parsec.digit
  Parsec.newline
  return (read digits :: Int)

-- TODO: Expect incorrect arguments? Check #flp discord question.
main :: IO ()
main = do
  args <- getArgs
  let action = head args

  when (null args || length args > 2) $ die "usage: flp22-fun option [input]"
  when (action `notElem` ["-i", "-b", "-o"]) $ die $ "Unknown option '" ++ action ++ "', only -i, -b and -o are supported."

  -- if length args == 2
  --   then do
  --     let filePath = last args
  --     knapsackString <- readFile filePath
  --     return ()
  --   else do
  --     knapsackString <- getContents
  --     return ()

  knapsackString <- readFile "knapsack.txt"

  print $ parse knapsackParser knapsackString

  -- let (Just action) = lookup switch dispatch
  -- putStrLn (action args)
  -- putStrLn (action knapsack)

-- info _ = putStrLn "info"

-- optim _ = putStrLn "optim"
