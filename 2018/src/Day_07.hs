{-# LANGUAGE TupleSections #-}

module Day_07 where

import Control.Monad
import Control.Monad.State
import Data.Bifunctor
import Data.List
import Data.Maybe
import Debug.Trace
import Text.Parsec hiding (State)
import Text.Parsec.String
import Util

type Node = Char

type Edge = (Node, Node)

parseEdge :: Parser Edge
parseEdge = do
  _ <- string "Step "
  step0 <- anyChar
  _ <- string " must be finished before step "
  step1 <- anyChar
  _ <- string " can begin."
  return (step0, step1)

input :: IO (Either ParseError [Edge])
input = mapM (parse parseEdge "") . lines <$> readFile "input/d7.txt"

-- input = mapM (parse parseEdge "") . lines <$> readFile "input/d7_test.txt"

type Graph = [(Node, [Node])]

edgesToGraph :: [Edge] -> Graph
edgesToGraph = groupWith (++) . map (second (: []))

graphToEdges :: Graph -> [Edge]
graphToEdges graph = concat [map (n,) ns | (n, ns) <- graph]

graphToNodes :: Graph -> [Node]
graphToNodes = nub . concatMap (uncurry (:))

transposeGraph :: Graph -> Graph
transposeGraph = edgesToGraph . map (\(n0, n1) -> (n1, n0)) . graphToEdges

data Mark = Unmarked | Temporary | Permanent
  deriving (Eq, Show)

-- topologicalSort :: Graph -> Maybe [Node]
-- topologicalSort graph = topologicalSort' (Just [], map (,Unmarked) $ graphToNodes graph)
--   where
--     topologicalSort' :: (Maybe [Node], [(Node, Mark)]) -> Maybe [Node]
--     topologicalSort' (sortedNodes, markedNodes) = case remainingNodes of
--       [] -> sortedNodes
--       _ -> case visit firstUnmarkedNode (sortedNodes, markedNodes) of
--         (Nothing, _) -> Nothing
--         state -> topologicalSort' state
--       where
--         remainingNodes = traceShowId $ filter (\(_, mark) -> mark /= Permanent) markedNodes
--         firstUnmarkedNode = traceShowId $ fst $ fromJust $ find (\(_, mark) -> mark == Unmarked) markedNodes
--     visit :: Node -> (Maybe [Node], [(Node, Mark)]) -> (Maybe [Node], [(Node, Mark)])
--     visit node (sortedNodes, markedNodes) = case fromJust $ lookup node markedNodes of
--       Permanent -> (sortedNodes, markedNodes)
--       Temporary -> (Nothing, markedNodes)
--       Unmarked -> ((node :) <$> sortedNodes', update node Permanent markedNodes')
--         where
--           (sortedNodes', markedNodes') =
--             foldr
--               visit
--               (sortedNodes, update node Temporary markedNodes)
--               (sortBy (flip compare) $ fromMaybe [] $ lookup node graph)

-- -- https://en.wikipedia.org/wiki/Topological_sorting#Kahn's_algorithm
-- topologicalSort :: Graph -> Maybe [Node]
-- topologicalSort graph = topologicalSort' graph [] sources
--   where
--     topologicalSort' :: Graph -> [Node] -> [Node] -> Maybe [Node]
--     topologicalSort' graph' l []
--       | all ((== []) . snd) graph' = Just $ reverse l
--       | otherwise = Nothing
--     topologicalSort' graph' l (n : s) = foldl' go (s, graph') (sort $ fromJust $ lookup n graph')
--       where
--         go :: ([Node], Graph) -> Node -> ([Node], Graph)
--         go (s', graph'') m
--           | null incomingEdges = (n : s', newGraph)
--           | otherwise = (s', newGraph)
--           where
--             incomingEdges = fromJust $ lookup m $ transposeGraph newGraph
--             newGraph = update n (delete m) graph''
--     sources =
--       snd $
--         fromJust $
--           find (\(m, _) -> isNothing m) $
--             groupBy' (`lookup` graph) $
--               graphToNodes graph

data TopologicalSortState = TSS
  { remainingGraph :: Graph,
    sortedNodes :: [Node],
    nodesToSearch :: [Node]
  }
  deriving (Show)

topologicalSort :: Graph -> Maybe [Node]
topologicalSort graph =
  evalState
    topologicalSort'
    TSS
      { remainingGraph = graph,
        sortedNodes = [],
        nodesToSearch = sources
      }
  where
    topologicalSort' :: State TopologicalSortState (Maybe [Node])
    topologicalSort' = do
      g <- gets remainingGraph
      l <- gets sortedNodes
      s <- gets nodesToSearch
      case sort s of
        [] -> if all (null . snd) g then gets (Just . reverse . sortedNodes) else return Nothing
        (n : s') -> do
          modify (\st -> st {nodesToSearch = s', sortedNodes = n : l})
          forM_ (sortBy (flip compare) $ fromMaybe [] $ lookup n g) processNode
          topologicalSort'
          where
            processNode :: Node -> State TopologicalSortState ()
            processNode m = do
              modify (\st -> st {remainingGraph = update n (delete m) (remainingGraph st)})
              g' <- gets remainingGraph
              when
                (null $ lookup m $ transposeGraph g')
                (modify (\st -> st {nodesToSearch = m : nodesToSearch st}))
    sources =
      snd $
        fromJust $
          find (\(m, _) -> isNothing m) $
            groupBy' (`lookup` transposeGraph graph) $
              graphToNodes graph

p1 :: [Edge] -> Maybe [Node]
p1 = topologicalSort . edgesToGraph

p2 :: [Edge] -> [Node]
p2 = undefined

main :: IO ()
main = do
  either (error . show) (print . p1) =<< input
  either (error . show) (print . p2) =<< input
