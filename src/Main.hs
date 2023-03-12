import System.Environment (getArgs)
import System.Exit (die)

import Solver.Brute (brute)
import Solver.Optim (optim)

import Control.Monad (when)

import Helper.Types (Knapsack(..), Solution)

import Parser (knapsackParser, parse)

dispatch :: [(String, Knapsack -> IO ())]
dispatch = [
             ("-i", print),
             ("-b", showSolution . brute),
             ("-o", showSolution . optim)
           ]

showSolution :: Solution -> IO ()
showSolution Nothing = putStrLn "False"
showSolution (Just s) = putStrLn $ "Solution " ++ show s

main :: IO ()
main = do
  args <- getArgs
  let option = head args

  when (null args || length args > 2) $ die "usage: flp22-fun option [file]"

  knapsackString <- 
    if length args == 2
      then
        readFile $ last args
      else
        getContents

  case lookup option dispatch of
    (Just action) ->
      case parse knapsackParser knapsackString of
        (Right knapsack) -> action knapsack
        (Left e) -> error $ show e
    Nothing -> error $ "Unknown option: " ++ option    
