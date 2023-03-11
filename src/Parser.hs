module Parser (
  knapsackParser,
  parse
) where

import Control.Applicative

import Text.Parsec ((<?>))
import Text.Parsec qualified as Parsec

import Helper.Types ( Knapsack(Knapsack), Item(Item) )

parse rule = Parsec.parse rule "(source)"

-- FIXME: End of file as }EOF or }\nEOF?
knapsackParser :: Parsec.Parsec String () Knapsack
knapsackParser = do
  Parsec.string "Knapsack {" >> Parsec.newline
  mW <- maxWeightParser
  mC <- minCostParser
  is <- itemsParser
  Parsec.char '}' >> (Parsec.eof <|> (Parsec.newline >> Parsec.eof))
  return (Knapsack mW mC is)

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

itemsParser :: Parsec.Parsec String () [Item]
itemsParser = do
  Parsec.string "items: [" >> Parsec.newline
  items <- Parsec.many1 itemParser
  Parsec.char ']' >> Parsec.newline
  return items

itemParser :: Parsec.Parsec String () Item
itemParser = do
  Parsec.tab >> Parsec.string "Item {" >> Parsec.newline
  weight <- weightParser
  cost <- costParser
  Parsec.tab >> Parsec.char '}' >> Parsec.newline
  return (Item weight cost)

weightParser :: Parsec.Parsec String () Int
weightParser = do
  Parsec.tab >> Parsec.string "weight: "
  digits <- Parsec.many1 Parsec.digit
  Parsec.newline
  return (read digits :: Int)

costParser :: Parsec.Parsec String () Int
costParser = do
  Parsec.tab >> Parsec.string "cost: "
  digits <- Parsec.many1 Parsec.digit
  Parsec.newline
  return (read digits :: Int)
