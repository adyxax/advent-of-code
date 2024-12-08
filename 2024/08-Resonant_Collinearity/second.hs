-- requires cabal install --lib megaparsec parser-combinators heap vector
module Main (main) where

import           Control.Monad        (void, when)
import qualified Data.Set             as S
import           Data.Void            (Void)
import           Text.Megaparsec
import           Text.Megaparsec.Char

exampleExpectedOutput = 34

data Antenna = Antenna Char Int Int deriving Show
type Input = [Antenna]
type Input' = (Int, Input)

type Parser = Parsec Void String

parseAntenna :: Parser Antenna
parseAntenna = do
  (SourcePos _ y x) <- getSourcePos
  c <- alphaNumChar
  pure $ Antenna c (unPos x - 1) (unPos y - 1)

skipDots :: Parser ()
skipDots = skipMany (char '.' <|> char '\n')

parseInput' :: Parser Input
parseInput' = some (skipDots *> parseAntenna <* skipDots) <* eof

parseInput :: String -> IO Input'
parseInput filename = do
  input <- readFile filename
  case runParser parseInput' filename input of
    Left bundle  -> error $ errorBundlePretty bundle
    Right input' -> return (length (lines input), input')

type Antinodes = S.Set (Int, Int)

compute :: Input' -> Int
compute (size, input) = S.size $ compute' input S.empty
  where
    valid (x, y) = x >= 0 && x < size && y >= 0 && y < size
    compute' :: Input -> Antinodes -> Antinodes
    compute' [_] acc    = acc
    compute' (x:xs) acc = compute' xs . S.unions $ acc : map (antinodes x) xs
    antinodes :: Antenna -> Antenna -> Antinodes
    antinodes (Antenna c1 x1 y1) (Antenna c2 x2 y2) | c1 /= c2 = S.empty
                                                    | otherwise = let (dx, dy) = (x2 - x1, y2 - y1)
                                                                  in S.fromList $ [(x1, y1), (x2, y2)]
                                                                               ++ takeWhile valid [(x1 - i*dx, y1 - i*dy)|i<-[1..]]
                                                                               ++ takeWhile valid [(x2 + i*dx, y2 + i*dy)|i<-[1..]]

main :: IO ()
main = do
  example <- parseInput "example"
  let exampleOutput = compute example
  when  (exampleOutput /= exampleExpectedOutput)  (error $ "example failed: got " ++ show exampleOutput ++ " instead of " ++ show exampleExpectedOutput)
  input <- parseInput "input"
  print $ compute input
