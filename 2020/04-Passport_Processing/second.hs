-- requires cabal install --lib megaparsec parser-combinators
module Main (main) where

import Control.Applicative.Permutations
import Control.Monad (void, when)
import Data.Maybe (catMaybes)
import Data.Void (Void)
import Text.Megaparsec
import Text.Megaparsec.Char
-- import Text.Megaparsec.Debug
import System.Exit (die)

exampleExpectedOutput = 2

data Passport = Passport { byr :: Int
                         , iyr :: Int
                         , eyr :: Int
                         , hgt :: Int
                         , hcl :: String
                         , ecl :: String
                         , pid :: Int
                         } deriving (Show)

type Parser = Parsec Void String

parseKey :: String -> Parser ()
parseKey key = do
  void (string key)
  void (char ':')

parseColor :: String -> Parser (Maybe String)
parseColor key = do
  value <- try (do
                   parseKey key
                   void $ char '#'
                   v <- some hexDigitChar
                   spaceBetween
                   return $ Just v
               ) <|> skipString key
  case value of
    Just v -> if length v == 6 then return value else return Nothing
    _ -> return Nothing

parseEyeColor :: String -> Parser (Maybe String)
parseEyeColor key = do
  parseKey key
  value <- (Just <$> ( string "amb"
                     <|> string "blu"
                     <|> string "brn"
                     <|> string "gry"
                     <|> string "grn"
                     <|> string "hzl"
                     <|> string "oth"
           )) <|> ((some $ alphaNumChar <|> char '#') *> return Nothing)
  spaceBetween
  return value

parseHeight :: String -> Parser (Maybe Int)
parseHeight key = do
  vu <- try (do
                parseKey key
                v <- some digitChar
                u <- string "cm" <|> string "in"
                spaceBetween
                return $ Just (v, u)
            ) <|> skipString key *> return Nothing
  case vu of
    Just (value, unit) -> let v = read value
                          in case unit of
                               "cm" -> if v >= 150 && v <= 193 then return (Just v) else return Nothing
                               "in" -> if v >= 59 && v <= 76 then return (Just v) else return Nothing
                               _ -> return Nothing
    _ -> return Nothing

parseInt :: String -> Int -> Int -> Parser (Maybe Int)
parseInt key low high = do
  parseKey key
  value <- some digitChar
  spaceBetween
  let vv = read value
  if vv >= low && vv <= high then return $ Just vv else return Nothing

parsePid :: String -> Parser (Maybe Int)
parsePid key = do
  value <- try (do
                   parseKey key
                   v <- some digitChar
                   spaceBetween
                   return $ Just v
               ) <|> skipString key *> return Nothing
  case value of
    Just v -> if length v == 9 then return (Just $ read v) else return Nothing
    _ -> return Nothing

skipInt :: String -> Parser (Maybe Int)
skipInt key = do
  parseKey key
  void (some $ alphaNumChar <|> char '#')
  spaceBetween
  return Nothing

skipString :: String -> Parser (Maybe String)
skipString key = do
  parseKey key
  void (some $ alphaNumChar <|> char '#')
  spaceBetween
  return Nothing

spaceBetween :: Parser ()
spaceBetween = void $ char ' ' <|> char '\n'

parsePassport :: Parser (Maybe Passport)
parsePassport = do
  -- (byr, iyr, eyr, hgt, hcl, ecl, pid, _) <- dbg "passport" . runPermutation $
  (byr, iyr, eyr, hgt, hcl, ecl, pid, _) <- runPermutation $
    (,,,,,,,) <$> toPermutationWithDefault Nothing (parseInt "byr" 1920 2002)
              <*> toPermutationWithDefault Nothing (parseInt "iyr" 2010 2020)
              <*> toPermutationWithDefault Nothing (parseInt "eyr" 2020 2030)
              <*> toPermutationWithDefault Nothing (parseHeight "hgt")
              <*> toPermutationWithDefault Nothing (parseColor "hcl")
              <*> toPermutationWithDefault Nothing (parseEyeColor "ecl")
              <*> toPermutationWithDefault Nothing (parsePid "pid")
              <*> toPermutationWithDefault Nothing (skipString "cid")
  void (char '\n')
  return $ makePassport byr iyr eyr hgt hcl ecl pid

makePassport :: Maybe Int -> Maybe Int -> Maybe Int -> Maybe Int -> Maybe String -> Maybe String -> Maybe Int -> Maybe Passport
makePassport (Just byr) ( Just iyr) (Just eyr) (Just hgt) (Just hcl) (Just ecl) (Just pid) = Just $ Passport { byr=byr, iyr=iyr, eyr=eyr, hgt=hgt, hcl=hcl, ecl=ecl, pid=pid }
makePassport _ _ _ _ _ _ _ = Nothing

parsePassports :: Parser [Passport]
parsePassports = do
  passports <- some parsePassport
  eof
  return $ catMaybes passports

parseInput :: String -> IO [Passport]
parseInput filename = do
  input <- readFile filename
  case runParser parsePassports filename input of
    Left bundle -> die $ errorBundlePretty bundle
    Right passports -> return passports

compute :: [Passport]-> Int
compute passports = length passports

main :: IO ()
main = do
  example <- parseInput "example"
  let exampleOutput = compute example
  when  (exampleOutput /= exampleExpectedOutput)  (die $ "example failed: got " ++ show exampleOutput ++ " instead of " ++ show exampleExpectedOutput)
  input <- parseInput "input"
  print $ compute input
