-- requires cabal install --lib megaparsec parser-combinators
module Main (main) where
import Control.Monad (void, when)
import Data.List (foldl')
import qualified Data.Map as M
import Data.Void (Void)
import Text.Megaparsec
import Text.Megaparsec.Char
import System.Exit (die)

exampleExpectedOutput = 32
exampleExpectedOutput2 = 126

data Bag = Bag { name :: String
               , contents :: [(Int, String)]
               } deriving (Show)

type Parser = Parsec Void String

parseName :: Parser String
parseName = do
  adjective <- some letterChar
  void (char ' ')
  color <- some letterChar
  return $ adjective ++ " " ++ color

parseInt :: Parser Int
parseInt = do
  i <- some digitChar
  return $ read i

parseContent :: Parser (Int, String)
parseContent = do
  i <- parseInt
  void (char ' ')
  name <- parseName
  void (string " bag")
  void (optional $ char 's')
  void (string ", " <|> string ".")
  return (i, name)

parseContents :: Parser [(Int, String)]
parseContents = some parseContent <|> (string "no other bags." *> return [])

parseBag :: Parser Bag
parseBag = do
  name <- parseName
  void $ string " bags contain "
  contents <- parseContents
  void (char '\n')
  return $ Bag { name=name, contents=contents }

parseBags :: Parser [Bag]
parseBags = some parseBag <* eof

parseInput :: String -> IO [Bag]
parseInput filename = do
  input <- readFile filename
  case runParser parseBags filename input of
    Left bundle -> die $ errorBundlePretty bundle
    Right bags -> return bags

compute :: [Bag]-> Int
compute bags = (compute' "shiny gold") - 1
  where
    bagsMap :: M.Map String Bag
    bagsMap = foldl' (\acc b -> M.insert (name b) b acc) M.empty bags
    compute' :: String -> Int
    compute' s = foldl' (\acc (i, s') -> acc + i * (compute' s')) 1 (contents $ bagsMap M.! s)

main :: IO ()
main = do
  example <- parseInput "example"
  let exampleOutput = compute example
  when  (exampleOutput /= exampleExpectedOutput)  (die $ "example failed: got " ++ show exampleOutput ++ " instead of " ++ show exampleExpectedOutput)
  example2 <- parseInput "example2"
  let exampleOutput2 = compute example2
  when  (exampleOutput2 /= exampleExpectedOutput2)  (die $ "example2 failed: got " ++ show exampleOutput2 ++ " instead of " ++ show exampleExpectedOutput2)
  input <- parseInput "input"
  print $ compute input
