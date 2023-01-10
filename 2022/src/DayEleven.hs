module DayEleven where

import Data.Char (digitToInt, isNumber, isSymbol)
import Data.List (foldl', sort)
import qualified Data.Map as M
import qualified Data.Sequence as S
import qualified Data.Text as T
import Text.Parsec (Parsec, char, digit, eof, letter, many, newline, parse, sepBy, space, spaces, string, (<|>))

newtype Worry = Worry Integer deriving (Show, Eq)

newtype Counter = Counter
  {unCounter :: Word}
  deriving (Show)

data Monkey = Monkey
  { monkeyItems :: S.Seq Worry,
    monkeyOperation :: Worry -> Worry,
    monkeyTest :: Worry -> Integer,
    monkeyCounts :: Counter
  }

-- monkeyParser :: Parsec String () Monkey
monkeyParser = go
  where
    indexParse = string "Monkey " *> digit <* char ':' <* newline
    itemsParse = do
      spaces *> string "Starting items: "
      l <- sepBy (many digit) (char ',' *> spaces)
      newline
      return (fmap read l :: [Integer])
    operationParse = do
      op <-
        spaces
          *> string "Operation: new = old "
          *> (char '*' <|> char '+')
      num <- space *> many digit
      return (op, num)
    testParse = do
      div' <- many (space <|> letter <|> space <|> char ':') *> many digit
      true' <- many (space <|> letter <|> space <|> char ':') *> many digit
      false' <- many (space <|> letter <|> space <|> char ':') *> many digit
      newline
      return ((read div', read true', read false') :: (Integer, Integer, Integer))
    go = (,,,) <$> indexParse <*> itemsParse <*> operationParse <*> testParse

inc :: Counter -> Counter
inc (Counter a) = Counter (a + 1)

operate :: (Integer -> Integer) -> Worry -> Worry
operate f (Worry a) = Worry (f a)

-- bored :: Worry -> Worry
-- bored (Worry a) = Worry (a `div` 3)

bored :: Worry -> Worry
bored (Worry a) = Worry (a `mod` 9699690)

divisibleByThrow :: Integer -> (Integer, Integer) -> Worry -> Integer
divisibleByThrow a (true, false) (Worry b)
  | b `rem` a == 0 = true
  | otherwise = false

inspectItem :: M.Map Integer Monkey -> Integer -> (Worry, M.Map Integer Monkey)
inspectItem map index = (pop', M.update (\_ -> Just monkey') index map)
  where
    popMonkeyItem :: Monkey -> (Worry, Monkey)
    popMonkeyItem (Monkey (pop S.:<| items') op test c) = (pop, Monkey items' op test (inc c))
    (pop', monkey') = popMonkeyItem (map M.! index)

throwItem :: M.Map Integer Monkey -> Integer -> Worry -> M.Map Integer Monkey
throwItem map index worry = M.adjust (appendMonkeyItem worry) index map
  where
    appendMonkeyItem :: Worry -> Monkey -> Monkey
    appendMonkeyItem worry (Monkey items op test c) = Monkey (items S.|> worry) op test c

processMonkey :: M.Map Integer Monkey -> Integer -> M.Map Integer Monkey
processMonkey map index = case monkeyItems (map M.! index) of
  S.Empty -> map
  _ -> go'
    where
      (pop, map') = inspectItem map index
      monkey@(Monkey items op test count) = map' M.! index
      newworry = bored . op $ pop
      throwto = test newworry
      newmap = throwItem map' throwto newworry
      go' = processMonkey newmap index

monkeys =
  M.fromList
    [ (0, Monkey (S.fromList [Worry 79, Worry 98]) (operate (* 19)) (divisibleByThrow 23 (2, 3)) (Counter 0)),
      (1, Monkey (S.fromList [Worry 54, Worry 65, Worry 75, Worry 74]) (operate (+ 6)) (divisibleByThrow 19 (2, 0)) (Counter 0)),
      (2, Monkey (S.fromList [Worry 79, Worry 60, Worry 97]) (operate (^ 2)) (divisibleByThrow 13 (1, 3)) (Counter 0)),
      (3, Monkey (S.fromList [Worry 74]) (operate (+ 3)) (divisibleByThrow 17 (0, 1)) (Counter 0))
    ]

monkeyBusiness :: M.Map Integer Monkey -> M.Map Integer Monkey
monkeyBusiness map = foldl' processMonkey map (M.keys map)

listItems :: M.Map Integer Monkey -> M.Map Integer (S.Seq Worry)
listItems = fmap monkeyItems

listCounters :: M.Map Integer Monkey -> M.Map Integer Counter
listCounters = fmap monkeyCounts

round20 map = foldl' (\a _ -> monkeyBusiness a) map [0 .. 19]

round10k map = foldl' (\a _ -> monkeyBusiness a) map [0 .. 9999]

levelOfMonkey :: M.Map Integer Monkey -> Word
levelOfMonkey map = go
  where
    list = fmap (unCounter . snd) . M.toList . listCounters $ map
    go = product . take 2 . reverse . sort $ list

constructMonkey ::
  Integer ->
  [Integer] ->
  (Integer -> Integer) ->
  (Integer, Integer, Integer) ->
  (Integer, Monkey)
constructMonkey index list op (d, t, f) =
  (index, Monkey (S.fromList $ Worry <$> list) (operate op) (divisibleByThrow d (t, f)) (Counter 0))

main :: IO ()
main = do
  f <- readFile "data/day11"
  let p = parse (sepBy monkeyParser (string "\n")) "day11" f
  either print print p

main' :: IO T.Text
main' = do T.pack <$> readFile "data/day11"

-- 14400119988: too high
-- 13621684730: too high
-- 13119526120
