-- TODO: Refactor imports
import System.Environment
import System.Directory
import System.IO
import Control.Monad (when)
import System.Exit
import Solver.Brute (brute)
import Helper.Types (Knapsack(..), Item(..), Solution)
import Helper.Functions (showSolution)

-- System.Exit fcia die pre chybu
-- if length args == 0 then die "asdlfkajsdf" else return ()

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

dispatch :: [(String, Knapsack -> IO ())]
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
-- TODO: take input from STDIN
-- TODO: proper error statements
main :: IO ()
main = do
  args <- getArgs
  let action = head args
  
  when (null args || length args > 2) $ die "usage: flp22-fun option [input]"
  when (action `notElem` ["-i", "-b", "-o"]) $ die $ "Unknown option '" ++ action ++ "', only -i, -b and -o are supported."

  if length args == 2
    then do
      let filePath = last args
      knapsackString <- readFile filePath
      return ()
    else do
      knapsackString <- getContents
      return ()


  -- let (Just action) = lookup switch dispatch
  -- putStrLn (action args)
  -- putStrLn (action knapsack)

-- info _ = putStrLn "info"

-- optim _ = putStrLn "optim"
