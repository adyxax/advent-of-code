-- requires cabal install --lib megaparsec parser-combinators vector
module Main (main) where
import Control.Monad (void, when)
import Data.Functor
import Data.List qualified as L
import Data.Maybe
import Data.Vector qualified as V
import Data.Void (Void)
import Text.Megaparsec
import Text.Megaparsec.Char
import System.Exit (die)

exampleExpectedOutput = 5031

type Line = V.Vector Char
type Map = V.Vector Line
data Instruction = Move Int | L | R deriving Show
data Input = Input Map [Instruction] deriving Show

type Parser = Parsec Void String

parseMapLine :: Parser Line
parseMapLine = do
  line <- some (char '.' <|> char ' ' <|> char '#') <* eol
  return $ V.generate (length line) (line !!)

parseMap :: Parser Map
parseMap = do
  lines <- some parseMapLine <* eol
  return $ V.generate (length lines) (lines !!)

parseInstruction :: Parser Instruction
parseInstruction = (Move . read <$> some digitChar)
               <|> (char 'L' $> L)
               <|> (char 'R' $> R)

parseInput' :: Parser Input
parseInput' = Input <$> parseMap
                    <*> some parseInstruction <* eol <* eof

parseInput :: String -> IO Input
parseInput filename = do
  input <- readFile filename
  case runParser parseInput' filename input of
    Left bundle -> die $ errorBundlePretty bundle
    Right input' -> return input'

data Heading = N | S | E | W deriving (Eq, Show)
data Cursor = Cursor Int Int Heading

isOut :: Map -> Int -> Int -> Bool
isOut m x y = isNothing line || isNothing tile || tile == Just ' '
  where
    line = m V.!? y
    tile = fromJust line V.!? x

stepOutside :: Map -> Int -> Int -> Int -> Heading -> Int -> Cursor
stepOutside m s x y h i | (t, h) == (a, N) = proceed fw (fn + rx) E
                        | (t, h) == (a, W) = proceed dw (ds - ry) E
                        | (t, h) == (b, N) = proceed (fw + rx) fs N
                        | (t, h) == (b, E) = proceed ee (es - ry) W
                        | (t, h) == (b, S) = proceed ce (cn + rx) W
                        | (t, h) == (c, W) = proceed (dw + ry) dn S
                        | (t, h) == (c, E) = proceed (bw + ry) bs N
                        | (t, h) == (d, N) = proceed cw (cn + rx) E
                        | (t, h) == (d, W) = proceed aw (as - ry) E
                        | (t, h) == (e, E) = proceed be (bs - ry) W
                        | (t, h) == (e, S) = proceed fe (fn + rx) W
                        | (t, h) == (f, W) = proceed (aw + ry) an S
                        | (t, h) == (f, S) = proceed (bw + rx) bn S
                        | (t, h) == (f, E) = proceed (ew + ry) es N
  where
    (tx, rx) = x `divMod` s
    (ty, ry) = y `divMod` s
    t = (tx, ty)
    proceed :: Int -> Int -> Heading -> Cursor
    proceed x' y' h' = case m V.! y' V.! x' of
      '.' -> step m s (Cursor x' y' h') (Move $ i - 1)
      '#' -> Cursor x y h
    a = (ax, ay)
    b = (bx, by)
    c = (cx, cy)
    d = (dx, dy)
    e = (ex, ey)
    f = (fx, fy)
    (ax, ay) = (1, 0)
    (bx, by) = (2, 0)
    (cx, cy) = (1, 1)
    (dx, dy) = (0, 2)
    (ex, ey) = (1, 2)
    (fx, fy) = (0, 3)
    (an, as, aw, ae) = (ay * s, (ay+1)*s-1, ax *s, (ax+1)*s-1)
    (bn, bs, bw, be) = (by * s, (by+1)*s-1, bx *s, (bx+1)*s-1)
    (cn, cs, cw, ce) = (cy * s, (cy+1)*s-1, cx *s, (cx+1)*s-1)
    (dn, ds, dw, de) = (dy * s, (dy+1)*s-1, dx *s, (dx+1)*s-1)
    (en, es, ew, ee) = (ey * s, (ey+1)*s-1, ex *s, (ex+1)*s-1)
    (fn, fs, fw, fe) = (fy * s, (fy+1)*s-1, fx *s, (fx+1)*s-1)

step :: Map -> Int -> Cursor -> Instruction -> Cursor
step _ _ (Cursor x y N) L = Cursor x y W
step _ _ (Cursor x y S) L = Cursor x y E
step _ _ (Cursor x y E) L = Cursor x y N
step _ _ (Cursor x y W) L = Cursor x y S
step _ _ (Cursor x y N) R = Cursor x y E
step _ _ (Cursor x y S) R = Cursor x y W
step _ _ (Cursor x y E) R = Cursor x y S
step _ _ (Cursor x y W) R = Cursor x y N
step m _ c (Move 0) = c
step m s (Cursor x y h) (Move i) | isOut m x' y' = stepOutside m s x y h i
                                 | tile == '.' = step m s (Cursor x' y' h) (Move $ i - 1)
                                 | tile == '#' = Cursor x y h
  where
    (x', y') = case h of
      N -> (x, y-1)
      S -> (x, y+1)
      E -> (x+1, y)
      W -> (x-1, y)
    tile = m V.! y' V.! x'

compute :: Input -> Int
compute (Input m i) = 1000 * (y+1) + 4 * (x+1) + hv
  where
    xmin = length (V.filter (== ' ') (m V.! 0))
    startingCursor = Cursor xmin 0 E
    s = length (m V.! 0) `div` 3
    Cursor x y h = L.foldl' (step m s) startingCursor i
    hv = case h of
      E -> 0
      S -> 1
      W -> 2
      N -> 3

main :: IO ()
main = do
  -- not doing the example, this solution is dependent on the shape of the input cube and sadly the example does not match it
  -- example <- parseInput "example"
  -- let exampleOutput = compute example
  -- when  (exampleOutput /= exampleExpectedOutput)  (die $ "example failed: got " ++ show exampleOutput ++ " instead of " ++ show exampleExpectedOutput)
  input <- parseInput "input"
  print $ compute input
