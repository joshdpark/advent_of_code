module DayTen where

data Instruction = Noop | Addx

type Cycle = Int
type Value = Int

parseline :: [String] -> (Instruction, Int -> Int)
parseline ["noop"] = (Noop, (+ 0))
parseline ("addx" : [x]) = (Addx, (+ read x))

execute :: (Instruction, Int -> Int) -> (Value, Cycle) -> (Value, Cycle)
execute (i, f) (v, c) = case i of
    Noop -> (f v, c + 1)
    Addx -> (f v, c + 2)

test = ["noop", "addx 3", "addx -5"]
process lines= scanl f (1, 0) $ map words lines
  where
    f a b = execute (parseline b) a

indexCycle :: [(Value, Cycle)] -> Int -> Cycle -> Maybe Int
indexCycle [] _ i = Nothing
indexCycle (x@(v,c):xs) p i
    | c >= i = Just (p * i)
    | otherwise = indexCycle xs v i


main = do
  instructions <- readFile "data/day10"
  let result = process . lines $ instructions
      cycles = [20, 60, 100, 140, 180, 220]
      signals = map (indexCycle result 0) cycles
  print $ fmap sum (sequence signals)


