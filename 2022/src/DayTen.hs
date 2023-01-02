module DayTen where

data Instruction = Noop | Addx
  deriving (Show, Eq)

newtype Register = Register Int
  deriving (Show, Eq)

newtype Position = Position Int
  deriving (Show, Eq)

data Pixel = Lit | Dark
  deriving (Show, Eq)

newtype Cycle = Cycle Int
  deriving (Show, Eq)

newtype Operation = Operation (Int -> Int)

inc :: Cycle -> Cycle
inc (Cycle a) = Cycle (a + 1)

operate :: Register -> Operation -> Register
operate (Register x) (Operation f) = Register (f x)

parseline :: [String] -> (Instruction, Operation)
parseline ["noop"] = (Noop, Operation id)
parseline ("addx" : [x]) = (Addx, Operation (+ read x))

-- keep a stack to keep track of cycles and instructions
buildStack :: [String] -> [(Instruction, Operation)]
buildStack = foldr acc []
  where
    acc = (:) . parseline . words

execute' ::
  Cycle ->
  Register ->
  [(Instruction, Operation)] ->
  [(Cycle, Register, Pixel)]
execute' cycle  register [] = []
execute' cycle register ((inst, op) : xs) = go
  where
    cycle' = inc cycle
    pixel = drawPixel register cycle
    go = case inst of
      Noop -> (cycle', register, pixel) : execute' cycle' register xs
      Addx -> (cycle', register, pixel) : (cycle'', register, drawPixel register cycle') : execute' cycle'' register' xs
        where
          register' = operate register op
          cycle'' = inc cycle'

execute :: [(Instruction, Operation)] -> [(Cycle, Register, Pixel)]
execute = execute' (Cycle 0) (Register 1)

test = ["noop", "addx 3", "addx -5"]

drawPixel :: Register -> Cycle -> Pixel
drawPixel (Register r) (Cycle c)
  | c' >= r - 1 && c' <= r + 1 = Lit
  | otherwise = Dark
    where
      c' = rem c 40

getCycles :: [Int] -> [(Cycle, Register, Pixel)] -> [(Int, Int)]
getCycles i [] = []
getCycles i ((Cycle c, Register r, _): xs)
    | c `elem` i = (c, r) : getCycles i xs
    | otherwise = getCycles i xs

drawPixels :: [Pixel] -> String
drawPixels [] = []
drawPixels (pixel:xs) = go
  where
    chr = case pixel of
        Lit -> '#'
        Dark -> '.'
    go = chr : drawPixels xs

splitPixelLine :: String -> [String]
splitPixelLine [] = []
splitPixelLine str = go
  where
    (line, xs) = splitAt 40 str
    go = line : splitPixelLine xs

main = do
  instructions <- readFile "data/day10"
  let stack = buildStack . lines $ instructions
      cycles = execute stack
      indices = [20, 60, 100, 140, 180, 220]
      result = getCycles indices cycles
      pixels = foldr (\(_,_,x) b -> x : b) [] cycles
  print cycles
  mapM putStrLn (splitPixelLine . drawPixels $ pixels)

