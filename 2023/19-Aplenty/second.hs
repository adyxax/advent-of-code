-- requires cabal install --lib megaparsec parser-combinators heap vector
module Main (main) where

import           Control.Applicative.Permutations
import           Control.Monad                    (void, when)
import qualified Data.Char                        as C
import           Data.Either
import           Data.Functor
import qualified Data.Heap                        as H
import qualified Data.List                        as L
import qualified Data.Map                         as M
import           Data.Maybe
import qualified Data.Set                         as S
import qualified Data.Vector                      as V
import qualified Data.Vector.Unboxed              as VU
import           Data.Void                        (Void)
import           Text.Megaparsec
import           Text.Megaparsec.Char

import           Debug.Trace

exampleExpectedOutput = 167409079868000

data Category = X | M | A | S deriving (Eq, Show)
data Op = Gt | Lt deriving (Eq, Show)
data Action = Accept | Reject | Jmp String deriving (Eq, Show)
data Rule = Cmp Category Op Int Action | RuleAction Action deriving (Eq, Show)
type Workflow = (String, [Rule])
type Workflows = M.Map String [Rule]
data Part = Part Int Int Int Int deriving (Eq, Show)
type Parts = [Part]
data Input = Input Workflows Parts deriving (Eq, Show)

type Parser = Parsec Void String

parseCategory :: Parser Category
parseCategory = char 'x' $> X
           <|> char 'm' $> M
           <|> char 'a' $> A
           <|> char 's' $> S

parseOp :: Parser Op
parseOp = char '>' $> Gt
      <|> char '<' $> Lt

parseNumber :: Parser Int
parseNumber = read <$> some digitChar

parseLabel :: Parser String
parseLabel = try $ count' 2 4 letterChar

parseAction :: Parser Action
parseAction = char 'A' $> Accept
          <|> char 'R' $> Reject
          <|> (Jmp <$> parseLabel)

parseRule :: Parser Rule
parseRule = (RuleAction <$> parseAction)
        <|> (Cmp <$> parseCategory <*> parseOp <*> parseNumber <* char ':' <*> parseAction)

parseWorkflow :: Parser Workflow
parseWorkflow = (,) <$> parseLabel <* char '{'
                    <*> some (parseRule <* optional (char ',')) <* char '}'

parseWorkflows :: Parser Workflows
parseWorkflows = M.fromList <$> some (parseWorkflow <* eol)

parsePart :: Parser Part
parsePart = Part <$> (string "{x=" *> parseNumber)
                 <*> (string ",m=" *> parseNumber)
                 <*> (string ",a=" *> parseNumber)
                 <*> (string ",s=" *> parseNumber <* char '}')

parseParts :: Parser Parts
parseParts = some (parsePart <* eol)

parseInput' :: Parser Input
parseInput' = Input <$> (parseWorkflows <* eol)
                    <*> (parseParts <* eof)

parseInput :: String -> IO Input
parseInput filename = do
  input <- readFile filename
  case runParser parseInput' filename input of
    Left bundle  -> error $ errorBundlePretty bundle
    Right input' -> return input'

type Interval = (Int, Int)
data Combination = Combination Interval Interval Interval Interval deriving (Eq, Show)

compute :: Input -> Int
compute (Input workflows _) = compute' (Combination (1, 4000) (1, 4000) (1, 4000) (1, 4000)) [RuleAction (Jmp "in")]
  where
    compute' :: Combination -> [Rule] -> Int
    compute' comb (RuleAction Accept:_) = score comb
    compute' comb (RuleAction Reject:_) = 0
    compute' comb (RuleAction (Jmp s):_) = compute' comb (workflows M.! s)
    compute' comb@(Combination (xl, xr) m a s) (Cmp X Lt n act:xs) | xr < n = compute' comb [(RuleAction act)]
                                                                   | xl >= n = compute' comb xs
                                                                   | otherwise = compute' (Combination (xl, n - 1) m a s) [(RuleAction act)] + compute' (Combination (n, xr) m a s) xs
    compute' comb@(Combination x (ml, mr) a s) (Cmp M Lt n act:xs) | mr < n = compute' comb [(RuleAction act)]
                                                                   | ml >= n = compute' comb xs
                                                                   | otherwise = compute' (Combination x (ml, n - 1) a s) [(RuleAction act)] + compute' (Combination x (n, mr) a s) xs
    compute' comb@(Combination x m (al, ar) s) (Cmp A Lt n act:xs) | ar < n = compute' comb [(RuleAction act)]
                                                                   | al >= n = compute' comb xs
                                                                   | otherwise = compute' (Combination x m (al, n - 1) s) [(RuleAction act)] + compute' (Combination x m (n, ar) s) xs
    compute' comb@(Combination x m a (sl, sr)) (Cmp S Lt n act:xs) | sr < n = compute' comb [(RuleAction act)]
                                                                   | sl >= n = compute' comb xs
                                                                   | otherwise = compute' (Combination x m a (sl, n - 1)) [(RuleAction act)] + compute' (Combination x m a (n, sr)) xs
    compute' comb@(Combination (xl, xr) m a s) (Cmp X Gt n act:xs) | xl > n = compute' comb [(RuleAction act)]
                                                                   | xr <= n = compute' comb xs
                                                                   | otherwise = compute' (Combination (xl, n) m a s) xs + compute' (Combination (n + 1, xr) m a s) [(RuleAction act)]
    compute' comb@(Combination x (ml, mr) a s) (Cmp M Gt n act:xs) | ml > n = compute' comb [(RuleAction act)]
                                                                   | mr <= n = compute' comb xs
                                                                   | otherwise = compute' (Combination x (ml, n) a s) xs + compute' (Combination x (n + 1, mr) a s) [(RuleAction act)]
    compute' comb@(Combination x m (al, ar) s) (Cmp A Gt n act:xs) | al > n = compute' comb [(RuleAction act)]
                                                                   | ar <= n = compute' comb xs
                                                                   | otherwise = compute' (Combination x m (al, n) s) xs + compute' (Combination x m (n + 1, ar) s) [(RuleAction act)]
    compute' comb@(Combination x m a (sl, sr)) (Cmp S Gt n act:xs) | sl > n = compute' comb [(RuleAction act)]
                                                                   | sr <= n = compute' comb xs
                                                                   | otherwise = compute' (Combination x m a (sl, n)) xs + compute' (Combination x m a (n + 1, sr)) [(RuleAction act)]
    score (Combination (xl, xr) (ml, mr) (al, ar) (sl, sr)) = (xr - xl + 1) * (mr - ml + 1) * (ar - al + 1) * (sr - sl + 1)

main :: IO ()
main = do
  example <- parseInput "example"
  let exampleOutput = compute example
  when  (exampleOutput /= exampleExpectedOutput)  (error $ "example failed: got " ++ show exampleOutput ++ " instead of " ++ show exampleExpectedOutput)
  input <- parseInput "input"
  print $ compute input
