module DayThree where
import Data.List
import Data.Char

splitRucksack :: String -> (String, String)
splitRucksack s = go
    where
        mid = div (length s) 2
        go = splitAt mid s

spotRedundant :: (String, String) -> String
spotRedundant (s, s') = s `intersect` s'

mapPriority :: String -> Int
mapPriority s
    | isLower c = ord c - 96
    | otherwise = ord c - 64 + 26
    where c = head s

groupLines :: [String] -> [[String]]
groupLines l = go
    where
        (h,t) = splitAt 3 l
        go = case (h, t) of
            (h, []) -> [h]
            (h, t) -> h : groupLines t

spotRedundantGroup :: [String] -> String
spotRedundantGroup s = go
    where
        s' = foldr intersect ['A'..'z'] s
        go = take 1 s'

test = go
    where
        -- d = "vJrwpWtwJgWrhcsFMMfFFhFp"
        -- go = mapPriority . spotRedundant . splitRucksack $ d
        d' = ["vJrwpWtwJgWrhcsFMMfFFhFp", "jqHRNqRjqzjGDLGLrsFMfFZSrLrFZsSL", "PmmdzqPrVvPwwTWBwg"]
        e = groupLines d'
        go = map spotRedundantGroup e




main :: IO ()
main = do
    d <- readFile "data/day3"
    let d' = lines d
    let result = map (mapPriority . spotRedundant . splitRucksack) d'
    print $ sum result
    let result' = go
            where
                e' = groupLines d'
                go = map (mapPriority . spotRedundantGroup) e'
    print $ sum result'
