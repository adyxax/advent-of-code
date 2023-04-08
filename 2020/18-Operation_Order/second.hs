-- requires cabal install --lib megaparsec parser-combinators
module Main (main) where
import Control.Monad (void, when)
import Data.Either
import Data.Void (Void)
import Text.Megaparsec
import Text.Megaparsec.Char
import System.Exit (die)

exampleExpectedOutput = 231 + 51

data Operation = Add | Mul deriving (Eq, Show)
data Chain = Chain [Either Int Chain] [Operation] deriving Show
type Input = [Chain]

type Parser = Parsec Void String

parseInt :: Parser Int
parseInt = do
  n <- some digitChar
  void $ optional (char ' ')
  return $ read n

parseOp :: Parser Operation
parseOp = do
  op <- (char '+' *> return Add) <|> (char '*' *> return Mul)
  void $ char ' '
  return op

parseParenthesis :: Parser Chain
parseParenthesis = do
  void $ char '('
  a <- parseChain
  void $ char ')'
  void $ optional (char ' ')
  return $ a

parseChain :: Parser Chain
parseChain = do
  a <- ((Left <$> parseInt) <|> (Right <$> parseParenthesis))
  next <- (Just <$> parseOp) <|> (return Nothing)
  case next of
    Nothing -> return $ Chain [a] []
    Just op -> do
      Chain elems ops <- parseChain
      return $ Chain (a:elems) (op:ops)

parseInput' :: Parser Input
parseInput' = do
  input <- some (parseChain <* void (optional (char '\n')))
  void eof
  return $ input

parseInput :: String -> IO Input
parseInput filename = do
  input <- readFile filename
  case runParser parseInput' filename input of
    Left bundle -> die $ errorBundlePretty bundle
    Right input' -> return input'

compute' :: Chain -> Int
compute' (Chain [Left x] []) = x
compute' (Chain [Right x] []) = compute' x
compute' (Chain (x:y:xs) (op:ops))
  | op == Add = compute' $ Chain (Left (cx+cy):xs) ops
  | op == Mul = cx * (compute' $ Chain (Left cy:xs) ops)
  where
    cx = if isLeft x then fromLeft 0 x else compute' (fromRight (Chain [] []) x)
    cy = if isLeft y then fromLeft 0 y else compute' (fromRight (Chain [] []) y)

compute :: Input -> Int
compute input = sum $ map compute' input

main :: IO ()
main = do
  example <- parseInput "example"
  let exampleOutput = compute example
  when  (exampleOutput /= exampleExpectedOutput)  (die $ "example failed: got " ++ show exampleOutput ++ " instead of " ++ show exampleExpectedOutput)
  input <- parseInput "input"
  print $ compute input
