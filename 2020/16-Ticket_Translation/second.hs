-- requires cabal install --lib megaparsec parser-combinators
module Main (main) where
import Control.Monad (void, when)
import Data.List (foldl', isPrefixOf, transpose)
import Data.Map qualified as M
import Data.Void (Void)
import Text.Megaparsec
import Text.Megaparsec.Char
import System.Exit (die)

exampleExpectedOutput = 132

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

filterInvalid :: Input -> Input
filterInvalid (Input ranges ticket nearbyTickets) = Input ranges ticket (foldl' compute' [] nearbyTickets)
  where
    compute' :: [Ticket] -> Ticket -> [Ticket]
    compute' acc nt
      | and $ map isValid nt = nt : acc
      | otherwise = acc
    isValid :: Int -> Bool
    isValid n = or $ map (isValid' n) ranges
    isValid' :: Int -> Range -> Bool
    isValid' n (Range{ll, lh, hl, hh}) = (n >= ll && n <= lh) || (n >= hl && n <= hh)

compute' :: Input -> Int
compute' Input{ranges, myTicket, nearbyTickets} = product . map (myTicket !!) . map snd . filter importantRange . snd $ iterate (potentialFieldIDsByRange, [])
  where
    importantRange :: (Range, Int) -> Bool
    importantRange (Range{field}, _) = isPrefixOf "departure" field
    nearbyTicketsByField = transpose nearbyTickets
    iterate :: ([(Range, [Int])], [(Range, Int)]) -> ([(Range, [Int])], [(Range, Int)])
    iterate ([], res) = ([], res)
    iterate ((r, [i]):ps, res) = iterate (map (removeID i) ps, (r, i):res)
    iterate (p:ps, res) = iterate (ps ++ [p], res)
    removeID :: Int -> (Range, [Int]) -> (Range, [Int])
    removeID i (r, l) = (r, filter (/= i) l)
    potentialFieldIDsByRange :: [(Range, [Int])]
    potentialFieldIDsByRange = zip ranges $ map (getMatchingFieldIDs myTicket nearbyTicketsByField) ranges
    getMatchingFieldIDs :: Ticket -> [Ticket] -> Range -> [Int]
    getMatchingFieldIDs [] [] _ = []
    getMatchingFieldIDs (_:ts) (fields:fieldss) range
      | and (map (match range) fields) = myId : getMatchingFieldIDs ts fieldss range
      | otherwise = getMatchingFieldIDs ts fieldss range
      where
        myId :: Int
        myId = (length myTicket) - (length ts) - 1
    match :: Range -> Int -> Bool
    match Range{ll, lh, hl, hh} n = (n >= ll && n <= lh) || (n >= hl && n <= hh)

compute :: Input -> Int
compute input = compute' $ filterInvalid input

main :: IO ()
main = do
  example <- parseInput "example2"
  let exampleOutput = compute example
  when  (exampleOutput /= exampleExpectedOutput)  (die $ "example failed: got " ++ show exampleOutput ++ " instead of " ++ show exampleExpectedOutput)
  input <- parseInput "input"
  print $ compute input
