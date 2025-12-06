-- requires cabal install --lib megaparsec parser-combinators heap vector
module Main (main) where

import           Control.Monad        (void, when)
import           Data.Functor
import qualified Data.List            as L
import qualified Data.Vector          as V
import           Data.Void            (Void)
import           Text.Megaparsec
import           Text.Megaparsec.Char

import Debug.Trace

exampleExpectedOutput :: Int
exampleExpectedOutput = 4277556

data Op = Add | Mul deriving Show
data Input = Input [[Int]] [Op] deriving Show
type Parser = Parsec Void String

parseNumber :: Parser Int
parseNumber = read <$> some digitChar <* optional hspace

parseOp :: Parser Op
parseOp = char '+' $> Add
      <|> char '*' $> Mul

parseInput' :: Parser Input
parseInput' = Input <$> some (optional hspace *> some parseNumber <* eol)
                    <*> some (parseOp <* optional hspace) <* eol <* eof

parseInput :: String -> IO Input
parseInput filename = do
  input <- readFile filename
  case runParser parseInput' filename input of
    Left bundle  -> error $ errorBundlePretty bundle
    Right input' -> return input'

compute :: Input -> Int
compute (Input ns ops) = sum $ map eval (zip tns ops)
  where
    eval :: ([Int], Op) -> Int
    eval (l, Add) = sum l
    eval (l, Mul) = product l
    tns = L.transpose ns

main :: IO ()
main = do
  example <- parseInput "example"
  let exampleOutput = compute example
  when  (exampleOutput /= exampleExpectedOutput)  (error $ "example failed: got " ++ show exampleOutput ++ " instead of " ++ show exampleExpectedOutput)
  input <- parseInput "input"
  print $ compute input
