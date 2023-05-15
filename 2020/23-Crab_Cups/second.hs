-- requires cabal install --lib megaparsec parser-combinators
module Main (main) where
import Control.Monad (void, when)
import Data.List qualified as L
import Data.Map qualified as M
import Data.Maybe (fromJust)
import Data.Set qualified as S
import Data.Void (Void)
import Text.Megaparsec
import Text.Megaparsec.Char
import System.Exit (die)

exampleExpectedOutput = 149245887792

type Cup = Int
type Input = [Cup]
type Input' = M.Map Cup Cup

type Parser = Parsec Void String

parseCup :: Parser Cup
parseCup = do
  n <- digitChar
  return $ read [n] - 1 -- so that there is a 0 in there and then we can use modulo

parseInput' :: Parser Input
parseInput' = some parseCup <* optional (char '\n') <* eof

parseInput :: String -> IO Input
parseInput filename = do
  input <- readFile filename
  case runParser parseInput' filename input of
    Left bundle -> die $ errorBundlePretty bundle
    Right input' -> return input'

score :: Input' -> Int
score input = (one + 1) * (two + 1)
  where
    one = input M.! 0
    two = input M.! one

compute' :: Cup -> Input' -> Int -> Int
compute' _ input 0 = score input
compute' current input remainingMoves = compute' next step (remainingMoves - 1)
  where
    one = input M.! current
    two = input M.! one
    three = input M.! two
    next = input M.! three
    cups = [one, two, three]
    mone = (current - 1) `mod` 1_000_000
    mtwo = (current - 2) `mod` 1_000_000
    mthree = (current - 3) `mod` 1_000_000
    mfour = (current - 4) `mod` 1_000_000
    destination :: Int
    destination = if L.elem mone cups then (if L.elem mtwo cups then (if L.elem mthree cups then mfour else mthree) else mtwo) else mone
    afterDestination = input M.! destination
    step :: Input'
    step = M.insert current next $ M.insert destination one $ M.insert three afterDestination input

compute :: Input -> Int
compute input = compute' (head input) input' 10_000_000
  where
    input' = M.fromList (zip input (tail input) ++ [(last input, 9)] ++ (zip [9..999_998] [10..999_999]) ++ [(999_999, head input)])

main :: IO ()
main = do
  example <- parseInput "example"
  let exampleOutput = compute example
  when  (exampleOutput /= exampleExpectedOutput)  (die $ "example failed: got " ++ show exampleOutput ++ " instead of " ++ show exampleExpectedOutput)
  input <- parseInput "input"
  print $ compute input
