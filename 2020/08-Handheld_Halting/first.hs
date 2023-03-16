-- requires cabal install --lib megaparsec parser-combinators
module Main (main) where
import Control.Monad (void, when)
import qualified Data.Set as S
import Data.Void (Void)
import Text.Megaparsec
import Text.Megaparsec.Char
import System.Exit (die)

exampleExpectedOutput = 5

data Op = Nop Int | Acc Int | Jmp Int

type Parser = Parsec Void String

parseOp :: Parser Op
parseOp = do
  op <- string "nop " <|> string "acc " <|> string "jmp "
  sign <- (char '+' *> return 1) <|> (char '-' *> return (-1))
  num <- some digitChar
  void $ char '\n'
  let n = sign * (read num)
  return $ case op of
    "nop " -> Nop n
    "acc " -> Acc n
    "jmp " -> Jmp n

parseOps :: Parser [Op]
parseOps = some parseOp <* eof

parseInput :: String -> IO [Op]
parseInput filename = do
  input <- readFile filename
  case runParser parseOps filename input of
    Left bundle -> die $ errorBundlePretty bundle
    Right ops -> return ops

compute :: [Op]-> Int
compute ops = run 0 0 S.empty
  where
    run :: Int -> Int -> S.Set Int -> Int
    run pointer acc visited
      | S.member pointer visited = acc
      | otherwise = case ops !! pointer of
          Nop _ -> run (pointer + 1) acc visited'
          Acc i -> run (pointer + 1) (acc + i) visited'
          Jmp i -> run (pointer + i) acc visited'
      where
        visited' = S.insert pointer visited

main :: IO ()
main = do
  example <- parseInput "example"
  let exampleOutput = compute example
  when  (exampleOutput /= exampleExpectedOutput)  (die $ "example failed: got " ++ show exampleOutput ++ " instead of " ++ show exampleExpectedOutput)
  input <- parseInput "input"
  print $ compute input
