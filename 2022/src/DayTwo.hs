module DayTwo where

data Hand = Rock | Paper | Scissors deriving (Eq, Show)

data Outcome = Win | Lose | Draw deriving (Eq, Show)

playMatch :: (Hand, Hand) -> (Outcome, Int)
playMatch (h, h') =
    -- rock: 1, paper: 2, scissors:3
    -- win: 6, draw: 3, lose:0
    case (h, h') of
        (Rock, Scissors) -> (Lose, 3)
        (Rock, Rock) -> (Draw, 4)
        (Rock, Paper) -> (Win, 8)
        (Paper, Rock) -> (Lose, 1)
        (Paper, Paper) -> (Draw, 5)
        (Paper, Scissors) -> (Win, 9)
        (Scissors, Paper) -> (Lose, 2)
        (Scissors, Scissors) -> (Draw, 6)
        (Scissors, Rock) -> (Win, 7)

playOutcome :: (Hand, Outcome) -> Int
playOutcome (h, h') =
    -- rock: 1, paper: 2, scissors:3
    -- win: 6, draw: 3, lose:0
    case (h, h') of
        (Rock, Lose) -> 3
        (Rock, Draw) -> 4
        (Rock, Win) -> 8
        (Paper, Lose) -> 1
        (Paper, Draw) -> 5
        (Paper, Win) -> 9
        (Scissors, Lose) -> 2
        (Scissors, Draw) -> 6
        (Scissors, Win) -> 7

mapHand :: String -> (Hand, Hand)
mapHand s = go
    where
        (a, b) = break (==' ') s
        x = case a of
            "A" -> Rock
            "B" -> Paper
            "C" -> Scissors
            _ -> undefined
        y = case dropWhile (==' ') b of
            "X" -> Rock
            "Y" -> Paper
            "Z" -> Scissors
            _ -> undefined
        go = (x, y)

mapOutcome :: String -> (Hand, Outcome)
mapOutcome s = go
    where
        (a, b) = break (==' ') s
        x = case a of
            "A" -> Rock
            "B" -> Paper
            "C" -> Scissors
            _ -> undefined
        y = case dropWhile (==' ') b of
            "X" -> Lose
            "Y" -> Draw
            "Z" -> Win
            _ -> undefined
        go = (x, y)


addScore :: [(Outcome, Int)] -> Int
addScore = foldr (\(_, x) -> (+x)) 0

test = go
    where
        mock = lines "A Y\nB X\nC Z"
        x = map mapHand mock
        y = map playMatch x
        go = addScore y

main = do
    content <- readFile "data/day2"
    let d = lines content
    -- let x = map mapHand d
    -- let y = map playMatch x
    -- print (addScore y)
    let x' = map mapOutcome d
    let y' = map playOutcome x'
    print (sum y')
