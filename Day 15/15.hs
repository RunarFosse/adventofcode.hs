import Data.Array
import Prelude hiding (Left, Right)
import Data.List

-- 1

type Position = (Int, Int)
type Warehouse = Array Position Char

parseInput :: String -> (Warehouse, [Position])
parseInput file = (warehouse, directions)
                where (mapLines, directionLines) = (span (/= "") . lines) file
                      (m, n) = (length mapLines, (length . head) mapLines)
                      warehouse = array ((0, 0), (n - 1, m - 1)) [((x, y), c) | (y, line) <- zip [0..] mapLines, (x, c) <- zip [0..] line]
                      directions = (readDirections . concat . tail) directionLines

readDirections :: String -> [Position]
readDirections [] = []
readDirections ('^':cs) = (0, -1) : readDirections cs
readDirections ('v':cs) = (0, 1) : readDirections cs
readDirections ('<':cs) = (-1, 0) : readDirections cs
readDirections ('>':cs) = (1, 0) : readDirections cs

performMove :: (Warehouse, Position) -> Position -> (Warehouse, Position)
performMove (warehouse, (x, y)) (dx, dy) | warehouse ! (x, y) == '[' && dx == 0 && rightDoubleMovedWarehouse ! (x, y + dy) == '.' && rightDoubleMovedWarehouse ! (x + 1, y + dy) == '.' = (rightDoubleMovedWarehouse // [((x, y + dy), warehouse ! (x, y)), ((x + 1, y + dy), warehouse ! (x + 1, y)), ((x, y), '.'),  ((x + 1, y), '.')], (x + dx, y + dy))
                                         | warehouse ! (x, y) == ']' && dx == 0 && leftDoubleMovedWarehouse ! (x, y + dy) == '.' && leftDoubleMovedWarehouse ! (x - 1, y + dy) == '.' = (leftDoubleMovedWarehouse // [((x, y + dy), warehouse ! (x, y)), ((x - 1, y + dy), warehouse ! (x - 1, y)), ((x, y), '.'),  ((x - 1, y), '.')], (x + dx, y + dy))
                                         | not (elem (warehouse ! (x, y)) ['[', ']'] && dx == 0) && not (elem (warehouse ! (x, y)) ['#', '.']) && movedWarehouse ! (x + dx, y + dy) == '.' = (movedWarehouse // [((x + dx, y + dy), warehouse ! (x, y)), ((x, y), '.')], (x + dx, y + dy))
                                         | otherwise = (warehouse, (x, y))
                                         where (movedWarehouse, _) = performMove (warehouse, (x + dx, y + dy)) (dx, dy)
                                               (rightDoubleMovedWarehouse, _) = performMove (movedWarehouse, (x + dx + 1, y + dy)) (dx, dy)
                                               (leftDoubleMovedWarehouse, _) = performMove (movedWarehouse, (x + dx - 1, y + dy)) (dx, dy)

computeGPSSum :: [Position] -> Int
computeGPSSum = foldr (\(x, y) r -> x + 100 * y + r) 0

-- 2

inflateWarehouse :: Warehouse -> Warehouse
inflateWarehouse warehouse = array ((0, 0), (2 * n + 1, m)) ([((2 * x, y), if c == 'O' then '[' else c) | ((x, y), c) <- assocs warehouse] ++ [((2 * x + 1, y), if c == '@' then '.' else if c == 'O' then ']' else c) | ((x, y), c) <- assocs warehouse])
                           where (_, (n, m)) = bounds warehouse

main :: IO()
main = do
        file <- readFile "Day 15/15.txt"
        let (warehouse, directions) = parseInput file
            robot = (fst . head . filter ((=='@') . snd) . assocs) warehouse
            (finalWarehouse, _) = foldl performMove (warehouse, robot) directions
            boxes = (map fst . filter ((=='O') . snd) . assocs) finalWarehouse
            inflatedWarehouse = inflateWarehouse warehouse
            inflatedRobot = (fst . head . filter ((=='@') . snd) . assocs) inflatedWarehouse
            (finalInflatedWarehouse, _) = foldl performMove (inflatedWarehouse, inflatedRobot) directions
            inflatedBoxes = (map fst . filter ((=='[') . snd) . assocs) finalInflatedWarehouse
        putStrLn $ show $ computeGPSSum boxes
        putStrLn $ show $ computeGPSSum inflatedBoxes