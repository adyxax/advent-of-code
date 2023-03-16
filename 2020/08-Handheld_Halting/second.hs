-- requires cabal install --lib megaparsec parser-combinators
module Main (main) where
import Control.Monad (void, when)
import Data.Maybe (fromJust)
import qualified Data.Set as S
import Data.Void (Void)
import Text.Megaparsec
import Text.Megaparsec.Char
import System.Exit (die)

exampleExpectedOutput = 8

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

run :: [Op] -> Int -> Int -> S.Set Int -> Bool -> Maybe Int
run ops pointer acc visited mut
  | pointer == last = Just acc
  | S.member pointer visited = Nothing
  | otherwise = case ops !! pointer of
      Nop i -> case runNop mut of
        Nothing -> if mut then Nothing else runJmp i True
        Just n -> Just n
      Acc i -> runAcc i mut
      Jmp i -> case runJmp i mut of
        Nothing -> if mut then Nothing else runNop True
        Just n -> Just n
  where
    last = length ops
    runAcc i = run ops (pointer + 1) (acc + i) visited'
    runJmp i = run ops (pointer + i) acc visited'
    runNop = run ops (pointer + 1) acc visited'
    visited' = S.insert pointer visited

compute :: [Op] -> Int
compute ops = fromJust $ run ops 0 0 S.empty False

main :: IO ()
main = do
  example <- parseInput "example"
  let exampleOutput = compute example
  when  (exampleOutput /= exampleExpectedOutput)  (die $ "example failed: got " ++ show exampleOutput ++ " instead of " ++ show exampleExpectedOutput)
  input <- parseInput "input"
  print $ compute input
