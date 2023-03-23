-- requires cabal install --lib megaparsec parser-combinators
module Main (main) where
import Control.Monad (void, when)
import Data.List (foldl')
import Data.Void (Void)
import Text.Megaparsec
import Text.Megaparsec.Char
import System.Exit (die)

exampleExpectedOutput = 25

data Action = N | S | E | W | L | R | F
data Heading = HN | HS | HE | HW
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
compute ops = (\(x, y, _) -> abs(x)+abs(y)) $ foldl' step (0, 0, HE) ops
  where
    step :: (Int, Int, Heading) -> Op -> (Int, Int, Heading)
    step (x, y, h) (Op N i) = (x, y+i, h)
    step (x, y, h) (Op S i) = (x, y-i, h)
    step (x, y, h) (Op E i) = (x+i, y, h)
    step (x, y, h) (Op W i) = (x-i, y, h)
    step (x, y, HN) (Op F i) = (x, y+i, HN)
    step (x, y, HS) (Op F i) = (x, y-i, HS)
    step (x, y, HE) (Op F i) = (x+i, y, HE)
    step (x, y, HW) (Op F i) = (x-i, y, HW)
    step (x, y, h) (Op a i) = (x, y, turn h a i)
    turn :: Heading -> Action -> Int -> Heading
    turn h a i = turn' h a mi
      where mi = (i `mod` 360) `div` 90
    turn' HN L 1 = HW
    turn' HN L 2 = HS
    turn' HN L 3 = HE
    turn' HS L 1 = HE
    turn' HS L 2 = HN
    turn' HS L 3 = HW
    turn' HE L 1 = HN
    turn' HE L 2 = HW
    turn' HE L 3 = HS
    turn' HW L 1 = HS
    turn' HW L 2 = HE
    turn' HW L 3 = HN
    turn' HN R 1 = HE
    turn' HN R 2 = HS
    turn' HN R 3 = HW
    turn' HS R 1 = HW
    turn' HS R 2 = HN
    turn' HS R 3 = HE
    turn' HE R 1 = HS
    turn' HE R 2 = HW
    turn' HE R 3 = HN
    turn' HW R 1 = HN
    turn' HW R 2 = HE
    turn' HW R 3 = HS
    turn' h _ _ = h

main :: IO ()
main = do
  example <- parseInput "example"
  let exampleOutput = compute example
  when  (exampleOutput /= exampleExpectedOutput)  (die $ "example failed: got " ++ show exampleOutput ++ " instead of " ++ show exampleExpectedOutput)
  input <- parseInput "input"
  print $ compute input
