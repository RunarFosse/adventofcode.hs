import Data.Map as M hiding (map)
import Data.List

-- 1

type Graph = Map String [String]

createGraph :: String -> Graph
createGraph file = M.fromList [((init . head . words) line, (tail . words) line) | line <- lines file]

countPaths :: String -> String -> Graph -> Int
countPaths node end graph | node == end = 1
                          | M.notMember node graph = 0
                          | otherwise = sum [countPaths neighbour end graph | neighbour <- graph M.! node]

-- 2

countVisitingPaths :: String -> String -> Graph -> [String] -> Int
countVisitingPaths start end graph visiting = visitingPaths ! (start, (M.fromList . zip visiting . repeat) False)
                        where possibleVisits = (map (M.fromList . zip visiting) . sequence . replicate (length visiting)) [False, True]
                              nodes = nub (M.keys graph ++ (concat . M.elems) graph) 
                              visitingPaths = M.fromList [((node, visits), opt node visits) | node <- nodes, visits <- possibleVisits]
                              opt :: String -> Map String Bool -> Int
                              opt node visits | node == end = if all id visits then 1 else 0
                                              | M.notMember node graph = 0
                                              | otherwise = sum [visitingPaths ! (neighbour, newVisits) | neighbour <- graph M.! node]
                                              where newVisits = if M.notMember node visits then visits else M.insert node True visits

main :: IO()
main = do
        file <- readFile "Day 11/11.txt"
        let graph = createGraph file
            paths = countPaths "you" "out" graph
            visitingPaths = countVisitingPaths "svr" "out" graph ["dac", "fft"]
        putStrLn $ show $ paths
        putStrLn $ show $ visitingPaths