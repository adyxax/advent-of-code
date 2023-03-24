-- requires cabal install --lib megaparsec parser-combinators
module Main (main) where
import Control.Monad (void, when, zipWithM)
import Data.Either (fromRight)
import Data.List (foldl')
import Data.Void (Void)
import Text.Megaparsec
import Text.Megaparsec.Char
import System.Exit (die)

exampleExpectedOutput = 1068781

type Input = [Maybe Int]

type Parser = Parsec Void String

parseShuttle :: Parser (Maybe Int)
parseShuttle = do
  num <- (Just . read <$> some digitChar) <|> (char 'x' *> return Nothing)
  void . optional $ char ','
  return $ num

parseOps :: Parser Input
parseOps = do
  void $ some digitChar
  void $ char '\n'
  shuttles <- some parseShuttle
  void $ char '\n'
  void $ eof
  return $ shuttles

parseInput :: String -> IO Input
parseInput filename = do
  input <- readFile filename
  case runParser parseOps filename input of
    Left bundle -> die $ errorBundlePretty bundle
    Right ops -> return ops

-- From rosetta code https://rosettacode.org/wiki/Chinese_remainder_theorem#Haskell
egcd :: Int -> Int -> (Int, Int)
egcd _ 0 = (1, 0)
egcd a b = (t, s - q * t)
  where
    (s, t) = egcd b r
    (q, r) = a `quotRem` b

modInv :: Int -> Int -> Either String Int
modInv a b =
  case egcd a b of
    (x, y)
      | a * x + b * y == 1 -> Right x
      | otherwise ->
        Left $ "No modular inverse for " ++ show a ++ " and " ++ show b

chineseRemainder :: [Int] -> [Int] -> Either String Int
chineseRemainder residues modulii =
  zipWithM modInv crtModulii modulii >>=
  (Right . (`mod` modPI) . sum . zipWith (*) crtModulii . zipWith (*) residues)
  where
    modPI = product modulii
    crtModulii = (modPI `div`) <$> modulii
-- end of snippet from rosetta code

compute :: Input -> Int
compute input = fromRight 0 . uncurry chineseRemainder $ unzip $ fst $ foldl' accumul ([], 0) input
  where
    accumul :: ([(Int, Int)], Int) -> Maybe Int -> ([(Int, Int)], Int)
    accumul (acc, i) Nothing = (acc, i+1)
    accumul (acc, i) (Just v) = (acc ++ [(-i, v)], i+1)

main :: IO ()
main = do
  example <- parseInput "example"
  let exampleOutput = compute example
  when  (exampleOutput /= exampleExpectedOutput)  (die $ "example failed: got " ++ show exampleOutput ++ " instead of " ++ show exampleExpectedOutput)
  input <- parseInput "input"
  print $ compute input
