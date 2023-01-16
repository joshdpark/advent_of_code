module DayTwelve where

import Data.Char (ord)
import Data.HashMap.Strict (HashMap, (!))
import qualified Data.HashMap.Strict as HM
import Data.List (find, foldl', sort)
import Data.Sequence (Seq ((:|>)), (<|), (|>))
import qualified Data.Sequence as S

data Distance a = Dist a | Inf
  deriving (Show, Eq)

instance (Ord a) => Ord (Distance a) where
  Inf <= Inf = True
  Inf <= Dist x = False
  Dist x <= Inf = True
  Dist x <= Dist y = x <= y

incDist :: Num a => Distance a -> Distance a
incDist (Dist a) = Dist (a + 1)
incDist Inf = Inf

type Vertex = (Int, Int)

newtype Edge = Edge Vertex
  deriving (Show, Eq)

type Graph = HashMap Vertex [Edge]

type Parents = HashMap Vertex (Maybe Vertex)

type Distances a = HashMap Vertex (Distance a)

data Direction
  = North
  | South
  | East
  | West
  deriving (Eq, Show)

step :: Vertex -> Direction -> Vertex
step (x, y) direction = case direction of
  North -> (x, y + 1)
  South -> (x, y - 1)
  East -> (x + 1, y)
  West -> (x - 1, y)

parseGrid :: String -> HashMap Vertex Char
parseGrid str = go
  where
    (y_axis, x_axis) = (length . lines $ str, length . head . lines $ str)
    coordinates = [(x, y) | y <- [y_axis, (y_axis - 1) .. 1], x <- [1 .. x_axis]]
    elevations = filter (/= '\n') str
    go = HM.fromList (zip coordinates elevations)

cost :: HashMap Vertex Char -> Vertex -> Vertex -> Int
cost hm this that = go
  where
    ord' x = case hm ! x of
      'S' -> ord 'a'
      'E' -> ord 'z'
      x -> ord x
    go = ord' that - ord' this

gridToGraph :: HashMap Vertex Char -> Graph
gridToGraph grid = HM.fromList (zip vertices edges)
  where
    edges = map (pathOptions grid) vertices
    vertices = HM.keys grid

pathOptions :: HashMap Vertex Char -> Vertex -> [Edge]
pathOptions grid origin = go
  where
    isPathPossible :: Vertex -> Bool
    isPathPossible to' = inbounds && climable
      where
        inbounds = HM.member to' grid
        climable
          | cost grid origin to' <= 1 = True
          | otherwise = False
    neighbors :: Vertex -> [Vertex]
    neighbors coord' = filter isPathPossible vertices
      where
        compass = [North, South, East, West]
        vertices = map (step coord') compass
    go = map Edge (neighbors origin)

gridStartEnd :: HashMap Vertex Char -> (Vertex, Vertex)
gridStartEnd grid = go
  where
    keys = HM.keys grid
    Just start = find (\a -> grid ! a == 'S') keys
    Just end = find (\a -> grid ! a == 'E') keys
    go = (start, end)

replace :: Vertex -> a -> HashMap Vertex a -> HashMap Vertex a
replace key value = HM.adjust (const value) key

bfs :: Num a => Graph -> Vertex -> Vertex -> (Seq Vertex, Distances a, Parents)
bfs graph start end = traverse queue distances' parents
  where
    queue = S.singleton start
    distances = HM.map (const Inf) graph
    parents = HM.map (const Nothing) graph
    distances' = replace start (Dist 0) distances
    traverse S.Empty d p = (S.Empty, d, p)
    -- traverse q@(end : vs) d p = (q, d, p)
    traverse (vs :|> v) d p = traverse newq newd newp
      where
        (newq, newd, newp) = foldl' update' (vs, d, p) edges
        edges = graph ! v
        update' (vs', d', p') (Edge e) = (vs'', d'', p'')
          where
            (vs'', d'', p'') = case d' ! e of
              Inf -> (e <| vs', replace e (incDist (d' ! v)) d', replace e (Just v) p')
              _ -> (vs', d', p')

bfs' :: Num a => Graph -> Vertex -> Vertex -> (Seq Vertex, HashMap Vertex a)
bfs' graph start end = traverse queue visited
  where
    queue = S.singleton start
    visited = HM.singleton start 0
    traverse S.Empty d = (S.Empty, d) 
    traverse (vs :|> v) d = traverse newq newv
      where
        (newq, newv) = foldl' update' (vs, d) edges
        edges = graph ! v
        update' (vs', visited) (Edge e) = (vs'', visited')
          where
            (vs'', visited')
              | HM.member e visited = (e <| vs', HM.insert e (visited ! v + 1) visited)
              | otherwise = (vs', visited)


pullThread :: Vertex -> Parents -> [Vertex]
pullThread thread parMap = case parMap ! thread of
  Just parent -> thread : pullThread parent parMap
  Nothing -> []

-- main :: IO ()
main = do
  f <- readFile "data/day12"
  let grid = parseGrid f
      graph = gridToGraph grid
      (start, end) = gridStartEnd grid
      -- (queue, distances, parents) = bfs graph start end
      (queue, distances, parents) = bfs graph start end
      allPossibleStarts = filter (\a -> grid ! a == 'a') (HM.keys grid)
      allPossibleDists = [bfs graph x end | x <- allPossibleStarts]
  print (distances ! end)
  -- print $ minimum (map (\(_, x, _) -> x ! end) allPossibleDists)
  print $ foldl' (\b (_,a,_) -> min b (a ! end)) Inf allPossibleDists

-- 499 answer is too high
