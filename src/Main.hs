import System.Environment (getArgs)
import System.Exit (die)

import Solver.Brute (brute)
import Solver.Optim (optim)

import Control.Applicative ((<|>))
import Control.Monad (when)

import Helper.Types (Knapsack(..), Item(..), Solution)

import Parser (knapsackParser, parse)

dispatch :: [(String, Knapsack -> IO ())]
dispatch = [
             ("-i", print),
             ("-b", showSolution . brute),
             ("-o", showSolution . optim)
           ]

showSolution :: Solution -> IO ()
showSolution Nothing = putStrLn "False"
showSolution (Just s) = print s

main :: IO ()
main = do
  args <- getArgs
  let option = head args

  when (null args || length args > 2) $ die "usage: flp22-fun option [file]"
  when (option `notElem` ["-i", "-b", "-o"]) $ die $ "Unknown option '" ++ option ++ "', only -i, -b and -o are supported."

  let (Just action) = lookup option dispatch

  if length args == 2
    then do
      let filePath = last args
      knapsackString <- readFile filePath
      case parse knapsackParser knapsackString of
        (Right knapsack) -> action knapsack
        (Left e) -> print e
    else do
      knapsackString <- getContents
      case parse knapsackParser knapsackString of
        (Right knapsack) -> action knapsack
        (Left e) -> print e
