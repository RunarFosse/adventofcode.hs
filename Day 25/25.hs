import Data.List
import Data.Tuple
import qualified Data.Map as M
import qualified Data.Set as S

-- 1

type Node = String
type Graph = M.Map Node [Node]
type Edges = S.Set (Node, Node)

createGraph :: [String] -> Graph
createGraph lines = foldr addNode M.empty tokenLines
                  where tokenLines = map ((\tokens -> ((init . head) tokens, tail tokens)) . words) lines
                        addNode :: (Node, [Node]) -> Graph -> Graph
                        addNode (node, connections) graph | M.member node graph = foldr (\connection -> addReverseConnection (connection, node)) (M.insert node (nub (graph M.! node ++ connections)) graph) connections
                                                          | otherwise = foldr (\connection -> addReverseConnection (connection, node)) (M.insert node connections graph) connections
                        addReverseConnection :: (Node, Node) -> Graph -> Graph
                        addReverseConnection (connection, node) graph | M.member connection graph = M.insert connection (nub (node : graph M.! connection)) graph
                                                                      | otherwise = M.insert connection [node] graph

createEdgeSet :: Graph -> Edges
createEdgeSet graph = foldr (\edge set -> if S.member (swap edge) set then set else S.insert edge set) S.empty [(node, connection) | (node, connections) <- M.assocs graph, connection <- connections]

type Visited = S.Set String
type EdgeOccurence = M.Map (Node, Node) Int 

bfs :: [(Node, Node)] -> Edges -> Graph -> Visited -> EdgeOccurence -> (Visited, EdgeOccurence)
bfs [] _ _ visited edgeOccurences = (visited, edgeOccurences)
bfs ((node, last):queue) edges graph visited edgeOccurences | S.member node visited = bfs queue edges graph visited edgeOccurences
                                                    | otherwise = bfs (queue ++ map (\connection -> (connection, node )) next) edges graph (S.insert node visited) (M.adjust (+1) (if M.member (node, last) edgeOccurences then (node, last) else (last, node)) edgeOccurences)
                                                     where connections = graph M.! node
                                                           next = filter (\connection -> (S.member (node, connection) edges || S.member (connection, node) edges) && S.notMember connection visited) connections
                                                           newOccurences = foldr (\nextNode occurences -> M.adjust (+1) (if M.member (node, nextNode) occurences then (node, nextNode) else (nextNode, node)) occurences) edgeOccurences next

findCut :: Edges -> Graph -> Edges
findCut edges graph = S.fromList (map fst $ take 3 $ sortBy (\(_, count1) (_, count2) -> compare count2 count1) (M.assocs totalEdgeOccurences))
                    where nodes = M.keys graph
                          totalEdgeOccurences = foldr (\node occurences -> snd $ bfs [(node, "")] edges graph S.empty occurences) (M.fromList [(edge, 0) | edge <- S.elems edges]) nodes

-- 2

-- Merry Christmas!

main :: IO()
main = do
        file <- readFile "Day 25/25.txt"
        let graph = createGraph $ lines file
            edges = createEdgeSet graph
            cut = findCut edges graph
            sizes = ((length . fst) $ bfs [((fst . head . S.elems) cut, "")] (S.difference edges cut) graph S.empty M.empty, length graph - fst sizes)
        putStrLn $ show $ fst sizes * snd sizes