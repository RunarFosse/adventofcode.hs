import Data.List
import qualified Data.Map as M
import qualified Data.Set as S

-- 1

type Network = M.Map String (S.Set String)
type Clique = [String]

parseNetwork :: [String] -> Network
parseNetwork [] = M.empty
parseNetwork (line:lines) = M.insertWith S.union pc1 (S.fromList [pc2]) $ M.insertWith S.union pc2 (S.fromList [pc1]) $ parseNetwork lines
                          where (pc1, pc2) = (take 2 line, (take 2 . drop 3) line)

isClique :: Network -> String -> Clique -> Bool
isClique network element clique = all (\other -> S.member element (network M.! other)) clique

findCliques :: Network -> Int -> S.Set Clique -> [Clique] -> [Clique]
findCliques _ _ _ [] = []
findCliques network size seen (clique:queue) | length clique == size = clique : findCliques network size (S.insert clique seen) queue
                                             | otherwise = findCliques network size (S.union (S.fromList neighbours) seen) (queue ++ neighbours)
                                             where neighbours = filter (\clique -> S.notMember clique seen) [sort (neighbour:clique) | neighbour <- S.toList (network M.! (head clique)), isClique network neighbour clique]

-- 2

findLargestMaximalClique :: Network -> S.Set Clique -> Clique -> [Clique] -> Clique
findLargestMaximalClique _ _ largest [] = largest
findLargestMaximalClique network seen largest (clique:queue) = findLargestMaximalClique network (S.union (S.fromList neighbours) seen) (if length clique > length largest then clique else largest) (queue ++ neighbours)
                                                             where neighbours = filter (\clique -> S.notMember clique seen) [sort (neighbour:clique) | neighbour <- S.toList (network M.! (head clique)), isClique network neighbour clique]

main :: IO()
main = do
        file <- readFile "Day 23/23.txt"
        let network = parseNetwork (lines file)
            pcs = (map singleton . M.keys) network
            chiefPcs = filter ((=='t') . head . head) pcs
            chiefCliques = findCliques network 3 S.empty chiefPcs
            lanClique = findLargestMaximalClique network S.empty [] pcs
        putStrLn $ show $ length chiefCliques
        putStrLn $ intercalate "," lanClique