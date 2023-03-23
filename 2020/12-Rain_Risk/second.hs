-- requires cabal install --lib megaparsec parser-combinators
module Main (main) where
import Control.Monad (void, when)
import Data.List (foldl')
import Data.Void (Void)
import Text.Megaparsec
import Text.Megaparsec.Char
import System.Exit (die)

exampleExpectedOutput = 286

data Action = N | S | E | W | L | R | F
data Op = Op Action Int

type Parser = Parsec Void String

parseOp :: Parser Op
parseOp = do
  action <- (char 'N' *> return N)
        <|> (char 'S' *> return S)
        <|> (char 'E' *> return E)
        <|> (char 'W' *> return W)
        <|> (char 'L' *> return L)
        <|> (char 'R' *> return R)
        <|> (char 'F' *> return F)
  num <- some digitChar
  void $ char '\n'
  return $ Op action (read num)

parseOps :: Parser [Op]
parseOps = some parseOp <* eof

parseInput :: String -> IO [Op]
parseInput filename = do
  input <- readFile filename
  case runParser parseOps filename input of
    Left bundle -> die $ errorBundlePretty bundle
    Right ops -> return ops

compute :: [Op]-> Int
compute ops = (\((x, y), _) -> abs(x)+abs(y)) $ foldl' step ((0, 0), (10, 1)) ops
  where
    step :: ((Int, Int), (Int, Int)) -> Op -> ((Int, Int), (Int, Int))
    step (p, (wx, wy)) (Op N i) = (p, (wx, wy+i))
    step (p, (wx, wy)) (Op S i) = (p, (wx, wy-i))
    step (p, (wx, wy)) (Op E i) = (p, (wx+i, wy))
    step (p, (wx, wy)) (Op W i) = (p, (wx-i, wy))
    step ((x, y), (wx, wy)) (Op F i) = ((x+i*wx, y+i*wy), (wx, wy))
    step (p, w) (Op a i) = (p, turn w a i)
    turn :: (Int, Int) -> Action -> Int -> (Int, Int)
    turn w a i = turn' w a mi
      where mi = (i `mod` 360) `div` 90
    turn' (wx, wy) L 1 = (-wy, wx)
    turn' (wx, wy) _ 2 = (-wx, -wy)
    turn' (wx, wy) L 3 = (wy, -wx)
    turn' (wx, wy) R 1 = (wy, -wx)
    turn' (wx, wy) R 3 = (-wy, wx)
    turn' w _ _ = w

main :: IO ()
main = do
  example <- parseInput "example"
  let exampleOutput = compute example
  when  (exampleOutput /= exampleExpectedOutput)  (die $ "example failed: got " ++ show exampleOutput ++ " instead of " ++ show exampleExpectedOutput)
  input <- parseInput "input"
  print $ compute input
