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

exampleExpectedOutput = 19114

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

compute :: Input -> Int
compute (Input workflows parts) = sum $ map compute' parts
  where
    compute' :: Part -> Int
    compute' (Part x m a s) = case evaluate entryPoint of
      Accept -> x + m + a + s
      Reject -> 0
      where
        evaluate :: [Rule] -> Action
        evaluate (RuleAction (Jmp s):_) = evaluate $ workflows M.! s
        evaluate (RuleAction a:_) = a
        evaluate (Cmp cat op n r:xs) | matches cat op n x m a s = evaluate [RuleAction r]
                                     | otherwise = evaluate xs
        matches X Lt n x _ _ _ = x < n
        matches X Gt n x _ _ _ = x > n
        matches M Lt n _ m _ _ = m < n
        matches M Gt n _ m _ _ = m > n
        matches A Lt n _ _ a _ = a < n
        matches A Gt n _ _ a _ = a > n
        matches S Lt n _ _ _ s = s < n
        matches S Gt n _ _ _ s = s > n
    entryPoint = workflows M.! "in"

main :: IO ()
main = do
  example <- parseInput "example"
  let exampleOutput = compute example
  when  (exampleOutput /= exampleExpectedOutput)  (error $ "example failed: got " ++ show exampleOutput ++ " instead of " ++ show exampleExpectedOutput)
  input <- parseInput "input"
  print $ compute input
