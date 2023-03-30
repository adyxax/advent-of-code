-- requires cabal install --lib megaparsec parser-combinators
module Main (main) where
import Control.Monad (void, when)
import Data.List (foldl')
import Data.Map qualified as M
import Data.Void (Void)
import Text.Megaparsec
import Text.Megaparsec.Char
import System.Exit (die)

exampleExpectedOutput = 71

data Range = Range { field :: String
                   , ll :: Int
                   , lh :: Int
                   , hl :: Int
                   , hh :: Int
                   } deriving (Show)
type Ticket = [Int]
data Input = Input { ranges :: [Range]
                   , myTicket :: Ticket
                   , nearbyTickets :: [Ticket]
                   } deriving (Show)

type Parser = Parsec Void String

parseInt :: Parser Int
parseInt = do
  n <- some digitChar
  void $ optional (char ',')
  return $ read n

parseRange :: Parser Range
parseRange = do
  f <- some (letterChar <|> char ' ')
  void $ string ": "
  ll <- parseInt
  void $ char '-'
  lh <- parseInt
  void $ string " or "
  hl <- parseInt
  void $ char '-'
  hh <- parseInt
  void $ char '\n'
  return $ Range f ll lh hl hh

parseTicket :: Parser Ticket
parseTicket = some parseInt <* char '\n'

parseInput' :: Parser Input
parseInput' = do
  r <- some parseRange
  void $ string "\nyour ticket:\n"
  t <- parseTicket
  void $ string "\nnearby tickets:\n"
  nts <- some parseTicket
  void eof
  return $ Input r t nts

parseInput :: String -> IO Input
parseInput filename = do
  input <- readFile filename
  case runParser parseInput' filename input of
    Left bundle -> die $ errorBundlePretty bundle
    Right ops -> return ops

compute :: Input -> Int
compute (Input ranges ticket nearbyTickets) = foldl' compute' 0 nearbyTickets
  where
    compute' :: Int -> Ticket -> Int
    compute' acc nt = acc + (sum $ map isValid nt)
    isValid :: Int -> Int
    isValid n = minimum $ map (isValid' n) ranges
    isValid' :: Int -> Range -> Int
    isValid' n (Range{ll, lh, hl, hh})
      | (n >= ll && n <= lh) || (n >= hl && n <= hh) = 0
      | otherwise = n

main :: IO ()
main = do
  example <- parseInput "example"
  let exampleOutput = compute example
  when  (exampleOutput /= exampleExpectedOutput)  (die $ "example failed: got " ++ show exampleOutput ++ " instead of " ++ show exampleExpectedOutput)
  input <- parseInput "input"
  print $ compute input
