import Data.Array

-- 1

type Position = (Int, Int)
type Graph = Array Position Bool

createGraph :: String -> Graph
createGraph file = array ((0, 0), (n - 1, m - 1)) [((x, y), c == '@') | (y, line) <- zip [0..] ls, (x, c) <- zip [0..] line]
                where ls = lines file
                      (m, n) = (length ls, (length . head) ls)

findRolls :: Graph -> [Position] -> [Position]
findRolls graph positions = filter (graph !) positions

findMovable :: Graph -> [Position] -> [Position]
findMovable graph [] = []
findMovable graph ((x, y):rolls) | isMovable = (x, y) : findMovable graph rolls
                                 | otherwise = findMovable graph rolls
                                where (n, m) = (snd . bounds) graph
                                      neighbours = [(x', y') | x' <- [x-1..x+1], y' <- [y-1..y+1], (x', y') /= (x, y) && x' >= 0 && x' <= n && y' >= 0 && y' <= m]
                                      isMovable = ((< 4) . sum . map (\position -> if graph ! position then 1 else 0)) neighbours

-- 2

countAllMovable :: Graph -> [Position] -> Int
countAllMovable graph rolls | null movable = 0
                            | otherwise = length movable + countAllMovable newGraph newRolls
                        where movable = findMovable graph rolls
                              newGraph = graph // zip movable (repeat False)
                              newRolls = findRolls newGraph rolls

main :: IO()
main = do
        file <- readFile "Day 4/4.txt"
        let graph = createGraph file
            rolls = findRolls graph (indices graph)
            movable = length $ findMovable graph rolls
            allMovable = countAllMovable graph rolls
        putStrLn $ show $ movable
        putStrLn $ show $ allMovable
        