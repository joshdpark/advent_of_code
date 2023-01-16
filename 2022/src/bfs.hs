-- https://meldingmonads.files.wordpress.com/2009/06/corecqueues.pdf
-- https://doisinkidney.com/posts/2018-12-18-traversing-graphs.html
module UnderstandingBfs where
import Data.List (foldl', unfoldr)
type Graph a = a -> [a]

graph 1 = [2,3]
graph 2 = [4,5]
graph 3 = [6,7]
graph 5 = [8,9]
graph 6 = [10]
graph _ = []

bfs g r = f r b []
  where
    f x fw bw = x : fw (g x : bw)

    b [] = []
    b qs = foldl' (foldr f) b qs []

-- bfs graph 6 -> [6,10]
-- 6 :  b qs@(graph 6 : [])
-- 6 : (foldl (foldr f) b (graph 6 : []) []
-- 6 :  foldl (foldr f) b [[10]] []
--      |
--     (foldr f) ((foldr f) b [10]) [] []
--       |          |
--       |          (f 10 b)
--       |          |
--       |          10 : b (graph 10 : *) :: [[a]] -> [a]
--       |          10 : b ([] : *) :: [[a]] -> [a]
--       foldr f   (10 : b ([] : *)) [] []
--       f (10 : b ([] : *)) [] []
--       f (10 : b [[]]) []
--       (10 : b [[]]) : b (graph ??????


-- unfoldr -> build from a seed
fibs = unfoldr (\a@(n,f) -> Just (a, (n + 1, f * (n + 1)))) (0, 1)
