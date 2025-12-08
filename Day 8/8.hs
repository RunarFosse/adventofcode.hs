import Data.Array as A
import Data.List
import Data.Map as M hiding (map, take, foldl', drop)

-- 1

type Position = (Int, Int, Int)
type UnionFind = Map Position Position

readPosition :: String -> Position
readPosition line = (read left :: Int, read middle :: Int, read right :: Int)
                where (left, rest) = (takeWhile (/=',') line, (tail . dropWhile (/=',')) line)
                      (middle, right) = (takeWhile (/=',') rest, (tail . dropWhile (/=',')) rest)

closestPairs :: [Position] -> [(Position, Position)]
closestPairs positions = sortFunction positionPairs
                    where positionPairs = [(position1, position2) | (i, position1) <- zip [0..] positions, position2 <- drop (i + 1) positions]
                          sortFunction = sortBy (\pair1 pair2 -> compare (distance pair1) (distance pair2))

distance :: (Position, Position) -> Float
distance ((x1, y1, z1), (x2, y2, z2)) = sqrt ((fromIntegral x2 - fromIntegral x1)^2 + (fromIntegral y2 - fromIntegral y1)^2 + (fromIntegral z2 - fromIntegral z1)^2)

initializeCircuits :: [Position] -> UnionFind
initializeCircuits positions = M.fromList $ zip positions positions

findCircuit :: Position -> UnionFind -> (Position, UnionFind)
findCircuit position circuits | position == parent = (position, circuits)
                              | otherwise = (root, M.insert position root compressedCircuits)
                            where parent = circuits M.! position
                                  (root, compressedCircuits) = findCircuit parent circuits

unionCircuits :: Position -> Position -> UnionFind -> UnionFind
unionCircuits position1 position2 circuits = M.insert parent1 parent2 circuits2
                                where (parent1, circuits1) = findCircuit position1 circuits
                                      (parent2, circuits2) = findCircuit position2 circuits1

connect :: [(Position, Position)] -> UnionFind -> UnionFind
connect [] circuits = circuits
connect ((position1, position2):positions) circuits = connect positions (unionCircuits position1 position2 circuits)

computeCircuitSizes :: UnionFind -> [Int]
computeCircuitSizes circuits = (reverse . sort . map length . group . sort) parents
                    where (_, parents) = foldl' (\(circuits, parents) position -> let (parent, compressedCircuits) = findCircuit position circuits in (compressedCircuits, parent:parents)) (circuits, []) (M.keys circuits)
                          

-- 2

findLastConnection :: [(Position, Position)] -> UnionFind -> (Position, Position)
findLastConnection positions circuits = findLastConnectionHelper positions circuits (head positions)
                        where findLastConnectionHelper :: [(Position, Position)] -> UnionFind -> (Position, Position) -> (Position, Position)
                              findLastConnectionHelper [] _ lastPair = lastPair
                              findLastConnectionHelper ((position1, position2):positions) circuits lastPair = findLastConnectionHelper positions connectedCircuits pair
                                                        where (parent1, circuits1) = findCircuit position1 circuits
                                                              (parent2, circuits2) = findCircuit position2 circuits1
                                                              pair = if parent1 == parent2 then lastPair else (position1, position2)
                                                              connectedCircuits = connect [(position1, position2)] circuits2

main :: IO()
main = do
        file <- readFile "Day 8/8.txt"
        let positions =  map readPosition (lines file)
            positionPairs = closestPairs positions
            circuits = initializeCircuits positions
            connectedCircuits = connect (take 1000 positionPairs) circuits
            connectedSizes = computeCircuitSizes connectedCircuits
            lastConnection = findLastConnection positionPairs connectedCircuits
        putStrLn $ show $ (product . take 3) connectedSizes
        putStrLn $ show $ (\((x1, _, _), (x2, _, _)) -> x1 * x2) lastConnection