-- requires cabal install --lib megaparsec parser-combinators heap vector
module Main (main) where

import           Control.Monad        (void, when)
import           Data.Bits
import           Data.Functor
import qualified Data.List            as L
import           Data.Maybe
import           Data.Void            (Void)
import           Text.Megaparsec
import           Text.Megaparsec.Char

type Register = Int
type Op = Int
data Input = Input [Register] [Op] deriving Show

type Parser = Parsec Void String

parseNumber :: Parser Int
parseNumber = read <$> some digitChar

parseRegister :: Parser Register
parseRegister = string "Register " *> (char 'A' <|> char 'B' <|> char 'C') *> string ": " *> parseNumber

parseInput' :: Parser Input
parseInput' = Input <$> some (parseRegister <* eol) <* eol
                    <*> (string "Program: " *> some (parseNumber <* optional (char ',')) <* eol <* eof)

parseInput :: String -> IO Input
parseInput filename = do
  input <- readFile filename
  case runParser parseInput' filename input of
    Left bundle  -> error $ errorBundlePretty bundle
    Right input' -> return input'

type Pointer = Int
type PState = (Pointer, [Register], [Int])

compute :: Input -> Int
compute (Input start ops) = minimum $ findA 0 (tail $ L.reverse $ L.tails ops)
  where
    findA :: Int -> [[Int]] -> [Int]
    findA a [] = [a]
    findA a (l:ls) = concatMap (\next -> findA next ls) nexts
      where
        candidates :: [Int]
        candidates = [a*8..a*8+7]
        steps :: [PState]
        steps = map (\x -> step (0, [x, 0, 0], [])) candidates
        outputs :: [[Int]]
        outputs = map (\(_, [_, _, _], x') -> reverse x') steps
        valids :: [Int]
        valids = L.elemIndices l outputs
        nexts :: [Int]
        nexts = map (candidates L.!!) valids
    end = length ops
    step :: PState -> PState
    step ps@(p, [a, b, c], _) | p == end = ps
                              | otherwise = step $ i ps co
      where
        i = [adv, bxl, bst, jnz, bxc, out, bdv, cdv] L.!! (ops L.!! p)
        co = [0, 1, 2, 3, a, b, c] L.!! (ops L.!! (p+1))
    adv (p, [a, b, c], acc) n = (p+2, [a `div` (2 ^ n), b, c], acc)
    bxl (p, [a, b, c], acc) n = (p+2, [a, xor b n, c], acc)
    bst (p, [a, b, c], acc) n = (p+2, [a, n `mod` 8, c], acc)
    jnz (p, [0, b, c], acc) n = (p+2, [0, b, c], acc)
    jnz (p, [a, b, c], acc) n = (n, [a, b, c], acc)
    bxc (p, [a, b, c], acc) _ = (p+2, [a, xor b c, c], acc)
    out (p, [a, b, c], acc) n = (p+2, [a, b, c], (n `mod` 8) : acc)
    bdv (p, [a, b, c], acc) n = (p+2, [a, a `div` (2 ^ n), c], acc)
    cdv (p, [a, b, c], acc) n = (p+2, [a, b, a `div` (2 ^ n)], acc)

main :: IO ()
main = do
  input <- parseInput "input"
  print $ compute input
