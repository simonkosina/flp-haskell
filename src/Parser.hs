module Parser (
  knapsackParser,
  parse
) where

import Control.Applicative ( Alternative((<|>)) )

import Text.Parsec qualified as Parsec

import Helper.Types ( Knapsack(Knapsack), Item(Item) )
import qualified Data.Functor.Identity

parse :: Parsec.Stream s Data.Functor.Identity.Identity t => Parsec.Parsec s () a -> s -> Either Parsec.ParseError a
parse rule = Parsec.parse rule "(source)"

knapsackParser :: Parsec.Parsec String () Knapsack
knapsackParser = do
  _ <- Parsec.string "Knapsack {" >> Parsec.newline
  mW <- maxWeightParser
  mC <- minCostParser
  is <- itemsParser
  _ <- Parsec.char '}' >> (Parsec.eof <|> (Parsec.newline >> Parsec.eof))
  return (Knapsack mW mC is)

maxWeightParser :: Parsec.Parsec String () Int
maxWeightParser = do
  _ <- Parsec.string "maxWeight: "
  digits <- Parsec.many1 Parsec.digit
  _ <- Parsec.newline
  return (read digits :: Int)

minCostParser :: Parsec.Parsec String () Int
minCostParser = do
  _ <- Parsec.string "minCost: "
  digits <- Parsec.many1 Parsec.digit
  _ <- Parsec.newline
  return (read digits :: Int)

itemsParser :: Parsec.Parsec String () [Item]
itemsParser = do
  _ <- Parsec.string "items: [" >> Parsec.newline
  items <- Parsec.many1 itemParser
  _ <- Parsec.char ']' >> Parsec.newline
  return items

itemParser :: Parsec.Parsec String () Item
itemParser = do
  _ <- Parsec.tab >> Parsec.string "Item {" >> Parsec.newline
  weight <- weightParser
  cost <- costParser
  _ <- Parsec.tab >> Parsec.char '}' >> Parsec.newline
  return (Item weight cost)

weightParser :: Parsec.Parsec String () Int
weightParser = do
  _ <- Parsec.tab >> Parsec.string "weight: "
  digits <- Parsec.many1 Parsec.digit
  _ <- Parsec.newline
  return (read digits :: Int)

costParser :: Parsec.Parsec String () Int
costParser = do
  _ <- Parsec.tab >> Parsec.string "cost: "
  digits <- Parsec.many1 Parsec.digit
  _ <- Parsec.newline
  return (read digits :: Int)
