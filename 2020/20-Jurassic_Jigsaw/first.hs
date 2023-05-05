-- requires cabal install --lib megaparsec parser-combinators
module Main (main) where
import Control.Monad (void, when)
import Data.List (intersect, transpose)
import Data.Void (Void)
import Text.Megaparsec
import Text.Megaparsec.Char
import System.Exit (die)

exampleExpectedOutput = 20899048083289

type TileID = Int
data Tile = Tile { tileID :: TileID
                 , image :: [[Bool]]
                 , edgesPermutations :: [[Bool]]
                 } deriving Show
type Input = [Tile]

type Parser = Parsec Void String

parseInt :: Parser Int
parseInt = do
  n <- some digitChar
  return $ read n

parseLine :: Parser [Bool]
parseLine = do
  elts <- some (char '#' <|> char '.')
  void $ char '\n'
  return $ map (== '#') elts

parseTile :: Parser Tile
parseTile = do
  void $ string "Tile "
  n <- parseInt
  void $ string ":\n"
  image <- some parseLine
  let timage = transpose image
      edges = [head image, last image, head timage, last timage]
      inverted = map reverse edges
  return $ Tile n image (edges ++ inverted)

parseInput' :: Parser Input
parseInput' = do
  tiles <- some $ parseTile <* (optional $ char '\n')
  void eof
  return tiles

parseInput :: String -> IO Input
parseInput filename = do
  input <- readFile filename
  case runParser parseInput' filename input of
    Left bundle -> die $ errorBundlePretty bundle
    Right input' -> return input'

compute :: Input -> Int
compute tiles = product . map fst . filter ((== 4) . snd) $ map matchingEdges tiles
  where
    matchingEdges :: Tile -> (TileID, Int)
    matchingEdges tile = (tileID tile, sum $ map (matches tile) tiles)
    matches :: Tile -> Tile -> Int
    matches Tile{tileID=a, image=_, edgesPermutations=e} Tile{tileID=b, image=_, edgesPermutations=f}
      | a == b = 0
      | otherwise = length $ intersect e f

main :: IO ()
main = do
  example <- parseInput "example"
  let exampleOutput = compute example
  when  (exampleOutput /= exampleExpectedOutput)  (die $ "example failed: got " ++ show exampleOutput ++ " instead of " ++ show exampleExpectedOutput)
  input <- parseInput "input"
  print $ compute input
