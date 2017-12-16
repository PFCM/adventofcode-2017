module TwelfthLib
  ( Graph
  , Node
  , connectedComponents
  , dfs
  , fromLists
  ) where

import qualified Data.Map as M
import Data.Map ((!))
import qualified Data.Set as S

type Graph = M.Map Node (S.Set Node)

type Node = Int

-- keep doing depth first searches until no nodes are left.
connectedComponents :: Graph -> [[Node]]
connectedComponents graph = fst . M.foldrWithKey collect ([], S.empty) $ graph
  where
    collect node _ v@(comps, visited)
      | node `S.member` visited = v
      | otherwise =
        let component = dfs node graph
            newVisited = S.union visited . S.fromList $ component
        in (component : comps, newVisited)

-- do a depth first search, returning the nodes visited
dfs :: Node -> Graph -> [Node]
dfs start graph = go [start] []
  where
    go [] path = path
    go (n:ns) path
      | n `elem` path = go ns path -- slow :(
      | otherwise = go (neighbours n ++ ns) (n : path)
    neighbours n = S.toList $ graph ! n

-- turn our list-of-lists graph representation into something a bit more
-- efficient for searching in
fromLists :: [[Int]] -> Graph
fromLists adjacencies = M.fromList (zip [0 ..] neighbourhoods)
  where
    neighbourhoods = map S.fromList adjacencies
