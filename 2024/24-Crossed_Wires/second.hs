-- requires cabal install --lib megaparsec parser-combinators heap vector
module Main (main) where

import           Control.Monad        (void, when)
import           Data.Functor
import qualified Data.List            as L
import qualified Data.Map             as M
import           Data.Maybe
import           Data.Ord             (comparing)
import qualified Data.Set             as S
import           Data.Void            (Void)
import           Text.Megaparsec
import           Text.Megaparsec.Char

import           Debug.Trace

type In = (String, Bool)
data Op = And | Or | Xor deriving (Eq, Ord, Show)
type Gate = (String, Op, String, String)
data Input = Input [In] [Gate] deriving Show

type Parser = Parsec Void String

parseIn :: Parser (String, Bool)
parseIn = (,) <$> some alphaNumChar <* string ": "
              <*> (char '1' $> True <|> char '0' $> False)

parseOp :: Parser Op
parseOp = string "AND" $> And
      <|> string "OR" $> Or
      <|> string "XOR" $> Xor

parseGate :: Parser Gate
parseGate = (,,,) <$> some alphaNumChar <* space
                  <*> parseOp <* space
                  <*> some alphaNumChar <* string " -> "
                  <*> some alphaNumChar

parseInput' :: Parser Input
parseInput' = Input <$> some (parseIn <* eol) <* eol
                    <*> some (parseGate <* eol) <* eof

parseInput :: String -> IO Input
parseInput filename = do
  input <- readFile filename
  case runParser parseInput' filename input of
    Left bundle  -> error $ errorBundlePretty bundle
    Right input' -> return input'

exec And = ( && )
exec Or  = ( || )
exec Xor = ( /= )

intToStringPad2 :: Int -> String
intToStringPad2 i | i < 10 = '0':show i
                  | otherwise = show i

--type GatesMap = M.Map String (String, Op, String)
type GatesMap = M.Map (String, Op, String) String

-- The gates could be in the wrong order around the operation
get :: GatesMap -> (String, Op, String) -> (GatesMap, String)
get gates g@(a, op, b) = case M.lookup g gates of
  Just o -> (M.delete g gates, o)
  Nothing -> let g' = (b, op, a) in case M.lookup g' gates of
    Just o  -> (M.delete g' gates, o)
    Nothing -> trace ("gate not found: " ++ show g) undefined

type SignalsMap = M.Map String String

getXYs :: String -> Op -> (GatesMap, SignalsMap) -> (GatesMap, SignalsMap)
getXYs prefix op acc = L.foldl' getXY acc [0..44]
  where
    getXY :: (GatesMap, SignalsMap) -> Int -> (GatesMap, SignalsMap)
    getXY (gates, signals) i = let is          = intToStringPad2 i
                                   (gates', s) = get gates ('x':is, op, 'y':is)
                               in (gates', M.insert (prefix ++ is) s signals)

getSmbs :: (GatesMap, SignalsMap) -> (GatesMap, SignalsMap)
getSmbs = getXYs "smb" Xor

getCarryBit0s :: (GatesMap, SignalsMap) -> (GatesMap, SignalsMap)
getCarryBit0s = getXYs "cb0" And

solve :: (GatesMap, SignalsMap) -> (GatesMap, SignalsMap)
solve acc = L.foldl' solveOne acc [1..44]
  where
    solveOne :: (GatesMap, SignalsMap) -> Int -> (GatesMap, SignalsMap)
    solveOne (gates, signals) i = let is = intToStringPad2 i
                                      js = intToStringPad2 $ i - 1
                                      cbp = signals M.! ("cb" ++ js)
                                      smb = signals M.! ("smb" ++ is)
                                      cb0 = signals M.! ("cb0" ++ is)
                                      (gates', cb1) = get gates (cbp, And, smb)
                                      signals' = M.insert ("cb1" ++ is) cb1 signals
                                      (gates'', cbn) = get gates' (cb1, Or, cb0)
                                      signals'' = M.insert ("cb" ++ is) cbn signals'
                                  in (gates'', signals'')

swapSignals :: [Gate] -> String -> String -> [Gate]
swapSignals gates x y = let gm = M.fromList $ map (\(a, op, b, out) -> (out, (a, op, b))) gates
                            xo = gm M.! x
                            yo = gm M.! y
                            gm' = M.insert x yo $ M.insert y xo gm
                        in map (\(out, (a, op, b)) -> (a, op, b, out)) $ M.toList gm'

-- I build the adder from the available gates, following the structure in the
-- svg file along this code. The swapSignals arguments have been determined by
-- rerunning manually this program 4 times.
--
-- When I got a 'gate not found: ("z11",Or,"dpf")' error, I ran:
-- ```
-- grep z11 input                                                                                          (base)
-- gkc AND qqw -> z11
-- ```
-- followed by
-- ```
-- grep dpf input                                                                                             (base)
-- wpd OR dpf -> dtq
-- y11 AND x11 -> dpf
-- ```
--
-- The only OR operation is the oen between wpd and dpf, with dpf as a common
-- signal. The two signals to swap are therefore z11 and wpd which I add bellow.
-- Rince and repeat until all four swaps are identified.
compute :: Input -> String
compute (Input _ gates) = trace (show $ gs''') $ L.intercalate "," $ L.sort ["z11", "wpd", "skh", "jqf", "z19", "mdd", "z37", "wts"]
  where
    gates' = swapSignals gates "z11" "wpd"
    gates'' = swapSignals gates' "skh" "jqf"
    gates''' = swapSignals gates'' "z19" "mdd"
    gates'''' = swapSignals gates''' "z37" "wts"
    gatesMap = M.fromList $ map (\(a, op, b, out) -> ((a, op, b), out)) gates''''
    (gm, gs) = getSmbs (gatesMap, M.empty)
    (gm', gs') = getCarryBit0s (gm, gs)
    gs'' = M.insert "cb00" (gs' M.! "cb000") gs'
    (gm'', gs''') = solve (gm', gs'')

main :: IO ()
main = do
  input <- parseInput "input"
  print $ compute input
