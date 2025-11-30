import Data.Char
import Data.Array
import Data.List

-- 1

type Position = (Int, Int)
type Map = Array Position Int

createMap :: String -> Map
createMap file = array ((0, 0), (n-1, m-1)) [((x, y), digitToInt c) | (y, line) <- zip [0..] (lines file), (x, c) <- zip [0..] line]
               where (m, n) = ((length . lines) file, (length . head . lines) file)

computeTrailScore :: Map -> Position -> Int
computeTrailScore topoMap = length . nub . countEndPositions topoMap 0

countEndPositions :: Map -> Int -> Position -> [Position]
countEndPositions topoMap height (x, y) | height /= topoMap ! (x, y) = []
                                        | height == 9 = [(x, y)]
                                        | otherwise = foldr (\(x, y) r -> countEndPositions topoMap (height + 1) (x, y) ++ r) [] validNeighbours
                                         where (n, m) = (snd . bounds) topoMap 
                                               neighbours = [(x + a, y + b) | a <- [-1..1], b <- [-1..1], abs a /= abs b]
                                               validNeighbours = filter (\(x, y) -> x >= 0 && x <= n && y >= 0 && y <= m) neighbours

-- 2

computeTrailRating :: Map -> Position -> Int
computeTrailRating topoMap = length . countEndPositions topoMap 0
                                          
main :: IO()
main = do
        file <- readFile "Day 10/10.txt"
        let topoMap = createMap file
            trailheads = (filter ((==0) . snd) . assocs) topoMap
            scores = map (computeTrailScore topoMap . fst) trailheads
            ratings = map (computeTrailRating topoMap . fst) trailheads
        putStrLn $ show $ sum scores
        putStrLn $ show $ sum ratings
