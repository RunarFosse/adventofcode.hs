import Numeric
import Prelude hiding (Left, Right)

-- 1

data Operation = Up Int String | Down Int String | Left Int String | Right Int String deriving Show

extractOperations :: [[String]] -> [Operation]
extractOperations [] = []
extractOperations (["U", count, color]:ops) = Up (read count) color : extractOperations ops
extractOperations (["D", count, color]:ops) = Down (read count) color : extractOperations ops
extractOperations (["L", count, color]:ops) = Left (read count) color : extractOperations ops
extractOperations (["R", count, color]:ops) = Right (read count) color : extractOperations ops

type Polygon = [(Int, Int)]

createPolygon :: [Operation] -> (Int, Int) -> Polygon
createPolygon [] (x, y) = [(x, y)]
createPolygon ((Up count _):ops) (x, y) = (x, y) : createPolygon ops (x, y-count)
createPolygon ((Down count _):ops) (x, y) = (x, y) : createPolygon ops (x, y+count)
createPolygon ((Left count _):ops) (x, y) = (x, y) : createPolygon ops (x-count, y)
createPolygon ((Right count _):ops) (x, y) = (x, y) : createPolygon ops (x+count, y)

shoelace :: Polygon -> Int
shoelace [_] = 0
shoelace ((x1, y1):(x2, y2):poly) = (y1 + y2) * (x1 - x2) `div` 2 + shoelace ((x2, y2):poly)

countPerimeter :: Polygon -> Int
countPerimeter [_] = 0
countPerimeter ((x1, y1):(x2, y2):poly) = abs (x2 - x1) + abs (y2 - y1) + countPerimeter ((x2, y2):poly)

calculateArea :: Polygon -> Int
calculateArea polygon = shoelace polygon + (countPerimeter polygon `div` 2) + 1

-- 2

colorToOperation :: String -> Operation
colorToOperation string = case direction of
                                '0' -> Right distance ""
                                '1' -> Down distance ""
                                '2' -> Left distance ""
                                '3' -> Up distance ""
                        where hex = (take 6 . drop 2) string
                              distance = (fst . head) $ readHex (take 5 hex)
                              direction = (head . reverse) hex

extractCorrectOperations :: [Operation] -> [Operation]
extractCorrectOperations [] = []
extractCorrectOperations ((Up _ color):ops) = colorToOperation color : extractCorrectOperations ops
extractCorrectOperations ((Down _ color):ops) = colorToOperation color : extractCorrectOperations ops
extractCorrectOperations ((Left _ color):ops) = colorToOperation color : extractCorrectOperations ops
extractCorrectOperations ((Right _ color):ops) = colorToOperation color : extractCorrectOperations ops

main :: IO()
main = do
        file <- readFile "Day 18/18.txt"
        let operations = extractOperations $ (map words . lines) file
            polygon = createPolygon operations (0, 0)
            area = calculateArea polygon
            correctOperations = extractCorrectOperations operations
            correctPolygon = createPolygon correctOperations (0, 0)
            correctArea = calculateArea correctPolygon
        putStrLn $ show area
        putStrLn $ show correctArea