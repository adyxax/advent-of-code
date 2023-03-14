-- requires cabal install --lib megaparsec parser-combinators
module Main (main) where
import Control.Monad (void, when)
import qualified Data.Set as S
import Data.Void (Void)
import Text.Megaparsec
import Text.Megaparsec.Char
import System.Exit (die)

exampleExpectedOutput = 4

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
compute bags = (length . compute' $ S.fromList ["shiny gold"]) - 1
  where
    bagsContaining :: S.Set String -> S.Set String
    bagsContaining s = S.union s $ S.fromList . (map name) $ filter (\x -> not . S.null . S.intersection s $ S.fromList . (map snd) . contents $ x) bags
    compute' :: S.Set String -> S.Set String
    compute' s = if s == s' then s else compute' s'
      where
        s' = bagsContaining s

main :: IO ()
main = do
  example <- parseInput "example"
  let exampleOutput = compute example
  when  (exampleOutput /= exampleExpectedOutput)  (die $ "example failed: got " ++ show exampleOutput ++ " instead of " ++ show exampleExpectedOutput)
  input <- parseInput "input"
  print $ compute input
