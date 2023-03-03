-- TODO: Refactor imports
import System.Environment
import System.Directory
import System.IO
import Solver.Brute (brute)
import Helper.Types (Knapsack(..), Item(..), Solution)
import Helper.Functions (showSolution)

-- System.Exit fcia die pre chybu
-- if length args == 0 then die "asdlfkajsdf" else return ()

-- TODO: Print the Knapsack on multiple line
-- instance Show TrafficLight where
-- show Red = "Red light"
-- show Yellow = "Yellow light"
-- show Green = "Green light"

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

dispatch :: [(String, Knapsack -> String)]
-- TODO: Show solutions properly
dispatch = [ 
            --  ("-i", info),
             ("-b", showSolution . brute)
            --  ("-o", optim)
           ]

-- TODO: Catch err when opening files (http://learnyouahaskell.com/input-and-output#exceptions)
-- TODO: brute results in error if no solution exists (use try/catch https://stackoverflow.com/questions/6009384/exception-handling-in-haskell0)
-- TODO: Expect incorrect arguments? Check #flp discord question.
-- TODO: Print output on multiple lines instead of one! (Priklad cviko)
-- TODO: src folder, make file, build?
main :: IO ()
main = do
  (switch:args) <- getArgs
  let (Just action) = lookup switch dispatch
  -- putStrLn (action args)
  putStrLn (action knapsack)

-- info _ = putStrLn "info"

-- optim _ = putStrLn "optim"
