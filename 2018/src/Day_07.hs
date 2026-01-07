{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE TupleSections #-}

module Day_07 where

import Control.Monad
import Control.Monad.State
import Data.Bifunctor
import Data.Char (ord)
import Data.List
import Data.Maybe
import Data.Sequence (Seq ((:<|)), (><))
import qualified Data.Sequence as Seq
import qualified Data.Set as Set
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

type Graph = [(Node, [Node])]

edgesToGraph :: [Edge] -> Graph
edgesToGraph = groupWith (++) . map (second (: []))

graphToEdges :: Graph -> [Edge]
graphToEdges graph = concat [map (n,) ns | (n, ns) <- graph]

graphToNodes :: Graph -> [Node]
graphToNodes = nub . concatMap (uncurry (:))

transposeGraph :: Graph -> Graph
transposeGraph = edgesToGraph . map (\(n0, n1) -> (n1, n0)) . graphToEdges

incomingEdges :: Node -> Graph -> [Node]
incomingEdges node = fromMaybe [] . lookup node . transposeGraph

graphToSources :: Graph -> [Node]
graphToSources graph = [node | node <- graphToNodes graph, null (incomingEdges node graph)]

data Mark = Unmarked | Temporary | Permanent
  deriving (Eq, Show)

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
        nodesToSearch = graphToSources graph
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

data ConstructionState = CS
  { seconds :: Int,
    nodesToConstruct :: Seq.Seq (Node, Int),
    constructedNodes :: Set.Set Node
  }
  deriving (Show)

constructionTime :: Int -> Int -> Graph -> Int
constructionTime numWorkers timeModifier graph =
  evalState
    constructionTime'
    CS
      { seconds = 0,
        nodesToConstruct = Seq.fromList $ map (,0) $ sort $ graphToSources graph,
        constructedNodes = Set.empty
      }
  where
    constructionTime' :: State ConstructionState Int
    constructionTime' = do
      s <- gets seconds
      ns <- gets nodesToConstruct
      case ns of
        Seq.Empty -> return s
        _ -> do
          modify
            ( \st ->
                st
                  { nodesToConstruct =
                      foldr (Seq.adjust (second succ)) ns [0 .. min numWorkers (length ns) - 1]
                  }
            )
          ns' <- popNodes numWorkers =<< gets nodesToConstruct
          modify (\st -> st {nodesToConstruct = ns', seconds = succ $ seconds st})
          constructionTime'
      where
        popNodes :: Int -> Seq.Seq (Node, Int) -> State ConstructionState (Seq.Seq (Node, Int))
        popNodes 0 ns = return ns
        popNodes _ Seq.Empty = return Seq.Empty
        popNodes cnt ((n, s) :<| ns)
          | s == stepToTime n = do
              !_ <- gets (traceMsg ("finished constructing node " ++ show n))
              modify (\st -> st {constructedNodes = Set.insert n $ constructedNodes st})
              cs <- gets constructedNodes
              let subNodes = sort $ filter (readyToConstruct cs) $ fromMaybe [] $ lookup n graph
              let !_ = traceMsg "adding subnodes" subNodes
              popNodes (pred cnt) (ns >< Seq.fromList (map (,0) subNodes))
          | otherwise = do
              ns' <- popNodes (pred cnt) ns
              return $ (n, s) :<| ns'
        readyToConstruct cs = (`Set.isSubsetOf` cs) . Set.fromList . (`incomingEdges` graph)
        stepToTime n = ord n - ord 'A' + 1 + timeModifier

input :: IO (Either ParseError [Edge])
input = mapM (parse parseEdge "") . lines <$> readFile "input/d7.txt"

p1 :: [Edge] -> Maybe [Node]
p1 = topologicalSort . edgesToGraph

p2 :: [Edge] -> Int
p2 = constructionTime 5 60 . edgesToGraph

main :: IO ()
main = do
  either (error . show) (print . p1) =<< input
  either (error . show) (print . p2) =<< input
