-- requires cabal install --lib megaparsec parser-combinators
module Main (main) where
import Control.Monad (void, when)
import Data.Either
import Data.List (any, foldl')
import Data.Maybe (catMaybes)
import Data.Map qualified as M
import Data.Void (Void)
import Text.Megaparsec
import Text.Megaparsec.Char
import System.Exit (die)

exampleExpectedOutput = 12

type Match = Char
type RuleID = Int
type Action = [RuleID]
type Rule = Either Match [Action]
type Message = String
data Input = Input { rules :: M.Map RuleID Rule
                   , messages :: [Message]
                   } deriving (Show)

type Parser = Parsec Void String

parseInt :: Parser Int
parseInt = do
  n <- some digitChar
  void $ optional (char ' ')
  return $ read n

parseMatch :: Parser Match
parseMatch = do
  void $ char '"'
  c <- letterChar
  void $ char '"'
  return c

parseAction :: Parser Action
parseAction = some (parseInt <* optional (char ' '))

parseRule :: Parser (RuleID, Rule)
parseRule = do
  id <- some digitChar
  void $ string ": "
  rule <- (Left <$> parseMatch) <|> (Right <$> some (parseAction <* optional (string "| ")))
  void $ char '\n'
  return $ (read id, rule)

parseInput' :: Parser Input
parseInput' = do
  rules <- M.fromList <$> some parseRule
  void $ char '\n'
  messages <- some (some letterChar <* (optional $ char '\n'))
  void eof
  let rules' = M.insert 8 (Right $ reverse [take i (repeat 42)|i<-[1..5]]) rules
      rules'' = M.insert 11 (Right $ reverse [take i (repeat 42) ++ take i (repeat 31)|i<-[1..5]]) rules'
  return $ Input rules'' messages

parseInput :: String -> IO Input
parseInput filename = do
  input <- readFile filename
  case runParser parseInput' filename input of
    Left bundle -> die $ errorBundlePretty bundle
    Right input' -> return input'

compute :: Input -> Int
compute (Input rules messages) = length . filter id $ map isValid messages
  where
    isValid :: Message -> Bool
    isValid msg = any (== "") $ matchAll 0 [msg]
    matchAll :: RuleID -> [Message] -> [Message]
    matchAll ruleId msgs = concat $ map (matches ruleId) msgs
    matches :: RuleID -> Message -> [Message]
    matches _ [] = []
    matches ruleId msg = case rules M.! ruleId of
      Left c -> if head msg == c then [tail msg] else []
      Right actions -> concat $ map (processAction msg) actions
    processAction :: Message -> Action -> [Message]
    processAction msg action = foldl' step [msg] action
    step :: [Message] -> RuleID -> [Message]
    step msgs ruleID = matchAll ruleID msgs

main :: IO ()
main = do
  example <- parseInput "example2"
  let exampleOutput = compute example
  when  (exampleOutput /= exampleExpectedOutput)  (die $ "example failed: got " ++ show exampleOutput ++ " instead of " ++ show exampleExpectedOutput)
  input <- parseInput "input"
  print $ compute input
