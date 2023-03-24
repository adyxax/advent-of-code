-- requires cabal install --lib megaparsec parser-combinators
module Main (main) where
import Control.Monad (void, when)
import Data.List (elemIndex)
import Data.Maybe (catMaybes, fromJust)
import Data.Void (Void)
import Text.Megaparsec
import Text.Megaparsec.Char
import System.Exit (die)

exampleExpectedOutput = 295

type Input = (Int, [Maybe Int])

type Parser = Parsec Void String

parseShuttle :: Parser (Maybe Int)
parseShuttle = do
  num <- (Just . read <$> some digitChar) <|> (char 'x' *> return Nothing)
  void . optional $ char ','
  return $ num

parseOps :: Parser Input
parseOps = do
  time <- some digitChar
  void $ char '\n'
  shuttles <- some parseShuttle
  void $ char '\n'
  void $ eof
  return $ ((read time), shuttles)

parseInput :: String -> IO Input
parseInput filename = do
  input <- readFile filename
  case runParser parseOps filename input of
    Left bundle -> die $ errorBundlePretty bundle
    Right ops -> return ops

compute :: Input -> Int
compute (time, shuttles) = departure * busId
  where
    busId = allShuttles !! (fromJust $ elemIndex departure nextDepartures)
    departure = minimum nextDepartures
    nextDepartures = map (\a -> (-time) `mod` a) allShuttles
    allShuttles = catMaybes shuttles

main :: IO ()
main = do
  example <- parseInput "example"
  let exampleOutput = compute example
  when  (exampleOutput /= exampleExpectedOutput)  (die $ "example failed: got " ++ show exampleOutput ++ " instead of " ++ show exampleExpectedOutput)
  input <- parseInput "input"
  print $ compute input
