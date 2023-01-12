import qualified Data.Set as Set

-- https://meldingmonads.files.wordpress.com/2009/06/corecqueues.pdf
-- https://doisinkidney.com/posts/2018-12-18-traversing-graphs.html
type Graph a = a -> [a]

graph 1 = [2, 3]
graph 2 = [4, 5]
-- graph 2 = [4, 5, 1] -- add a cyclical part of the graph
graph 3 = [6, 7]
graph 5 = [8, 9]
graph 6 = [10]
graph _ = []

bfs g r = f r b []
  where
    -- f :: t1 -> ([t t1] -> [t1]) -> [t t1] -> [t1]
    f x fw bw = x : fw (g x : bw)

    -- b :: [t t1] -> t1]
    b [] = []
    b qs = foldl (foldr f) b qs []

-- foldr f :: ([t t1] -> [t1]) -> t2 t1 -> [t t1] -> [t1]

-- foldr (+) 0 [1..3] = (+ 1 (+ 2 (+ 3 0)))
-- foldr (:) [] [[a],[b], [c]] = [a] : ([b] : ([c] : []))

-- bfs graph 3 = [3, 6, 7, 10]
-- f 3 b []
-- 3 : b (graph 3 : [])
--     b [[6,7]]
--     foldl (foldr f) b [[6,7]] []
--     foldr f b [6,7] []
--     f 6 (f 7 b) []
--          7 : b ?
--     f 6 (7 : b ?) []
--     6 : (7 : b ? ) (g 6 : [])
--     6 : (7 : b (g 6 : []))
--     6 : 7 : b [[10]]
--             foldl (foldr f) b [[10]] []
--             foldr f b [10] []
--             f 10 b []
--             10 : b []
--  3 : 6 : 7 : 10 : []

myconcat :: [[a]] -> [a]
myconcat = foldr (\a b -> foldr (:) b a) []

-- anotherbfs g seed = zip3 frontier [0 ..] (Set.empty : visited)
iterbfs g seed = frontier
  where
    f (edges, visited, distance) = (e, v, distance + 1)
      where
        e = Set.difference (Set.fromList . concatMap g $ edges) visited
        v = Set.union visited edges
    frontier = iterate f (Set.singleton seed, Set.empty, 0)
    -- visited = zipWith (Set.union . Set.fromList) frontier (Set.empty : visited)

-- [[1,2,3], [4,5]]
-- foldr (:) [4, 5] []
-- foldr (:) (foldr (:) [] [4,5]) [1,2,3]
-- f a b = foldr (:) b a
-- f (f [4,5] []) [1, 2, 3]

f' x = [x + 1, x * 2]

-- [[0], [1, 2], [2, 3], [3, 4]]
--   |----f' 0
--        |------ f' 1
--           |----------- f'2

mybfs g x = qb x []
  where
    qb x (q : qs) = (x : q) : foldr qb qs (g x)
    qb x [] = [x] : foldr qb [] (g x)

--  [0] : foldr qb [] (f' 0)
--        foldr qb [] [1, 0]
--        qb 1 (qb 0 [])
--        qb 1 ([0] : ?)
--        1 : 0 : ?
--        () : foldr qb [0:1:?] (g 1)
--        [1,0] : foldr qb [0,1,?] [2, 1]
--                qb 2 (qb 1 [0,1,?])
--                      [1,0,?]
--                2:[1,0]

mybfs' seed = f b seed [] []
  where
    f combine x ls qs = combine (x : ls) (graph x : qs)
    b _ [] = []
    b input output = input : foldl (foldl f) b output [] []

lwe g r = f b r [] []
  where
    f b x ls qs = b (x : ls) (g x : qs)

    b _ [] = []
    b k qs = k : foldl (foldl f) b qs [] []

nested = [[1 .. 3], [4, 5], [6 .. 10]]

myconcatsum :: Num a => [[a]] -> a
myconcatsum = foldl (foldr (+)) 0

-- https://doisinkidney.com/posts/2018-06-03-breadth-first-traversals-in-too-much-detail.html
data Tree' a = Node
  { rootLabel :: a,
    subForest :: [Tree' a]
  }

breadthFirstEnumerate :: Tree' a -> [a]
breadthFirstEnumerate ts = f ts b []
  where
    f (Node x xs) fw bw = x : fw (xs : bw)

    b [] = []
    b qs = foldl (foldr f) b qs []

-- from corecusion on wikipedia
data Tree a b = Leaf a | Branch b (Tree a b) (Tree a b)
  deriving (Show)

bftrav :: Tree a b -> [Tree a b]
bftrav tree = queue
  where
    queue = tree : gen 1 queue

    gen 0 p = []
    gen len (Leaf _ : s) = gen (len - 1) s
    gen len (Branch _ l r : s) = l : r : gen (len + 1) s
